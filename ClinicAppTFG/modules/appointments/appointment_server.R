library(toastui)
library(shinyWidgets)
library(emayili)


appointmentServer <- function(id, pool, current_user){
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    refresh <- reactiveVal(0)
    
    # Helper para obtener usuario actual
    get_user <- reactive({
      if (is.function(current_user)) current_user() else current_user
    })
    
    rv <- reactiveValues(
      cita_id = NULL,
      edit_mode = FALSE
    )
    
    # --- 1. CONFIGURACIÓN SMTP ---
    smtp_server <- server(
      host = "smtp.gmail.com",
      port = 465,
      username = "clinicapptfg@gmail.com",
      password = "qffj vlrz kmzq jpwz"
    )
    
    # --- FUNCIÓN DE LIMPIEZA ---
    limpiar_todo <- function() {
      removeModal()
      shinyjs::runjs("$('body').removeClass('modal-open'); $('.modal-backdrop').remove();")
    }
    
    # ---------------- 1. RESUMEN DIARIO DINÁMICO ----------------
    output$resumen_diario <- renderUI({
      u <- get_user()
      if (u$tipo_usuario == "paciente") return(NULL)
      
      fecha_view <- as.Date(if(!is.null(input$cal_viewDate)) input$cal_viewDate else Sys.Date())
      gab_sel <- if(!is.null(input$filtro_gabinete)) input$filtro_gabinete else "0"
      
      query <- "SELECT estado FROM citas WHERE DATE(fecha_inicio) = ? AND estado != 'cancelada'"
      params <- list(as.character(fecha_view))
      
      if (gab_sel != "0") {
        query <- paste(query, "AND gabinete = ?")
        params <- c(params, list(gab_sel))
      }
      
      res <- DBI::dbGetQuery(pool, query, params = params)
      total <- nrow(res)
      comp  <- sum(res$estado == "completada")
      
      div(class="resumen-card shadow-sm mb-3 p-3 bg-light border-start border-primary",
          div(class="row text-center",
              div(class="col-md-3", tags$small("Día seleccionado"), div(class="fw-bold", format(fecha_view, "%d/%m/%Y"))),
              div(class="col-md-3", tags$small("Total Citas"), div(class="text-primary fw-bold", total)),
              div(class="col-md-3", tags$small("Completadas"), div(class="text-success fw-bold", comp)),
              div(class="col-md-3", tags$small("Pendientes"), div(class="text-warning fw-bold", total - comp))
          )
      )
    })
    
    # ---------------- 2. CALENDARIO FILTRADO ----------------
    output$cal <- renderCalendar({
      refresh()
      u <- get_user()
      req(u)
      
      query <- "SELECT c.*, p.nombre paciente, d.nombre doctor 
                FROM citas c 
                JOIN usuarios p ON c.paciente_id = p.id 
                JOIN usuarios d ON c.profesional_id = d.id
                WHERE c.estado != 'cancelada'"
      params <- list()
      
      if (u$tipo_usuario == "paciente") {
        query <- paste(query, "AND c.paciente_id = ?")
        params <- c(params, list(u$id))
      } else {
        if (!is.null(input$filtro_gabinete) && input$filtro_gabinete != "0") {
          query <- paste(query, "AND c.gabinete = ?")
          params <- c(params, list(input$filtro_gabinete))
        }
        if (!is.null(input$filtro_doctor) && input$filtro_doctor != "0") {
          query <- paste(query, "AND c.profesional_id = ?")
          params <- c(params, list(input$filtro_doctor))
        }
      }
      
      df <- if (length(params) > 0) DBI::dbGetQuery(pool, query, params = params) else DBI::dbGetQuery(pool, query)
      
      if (nrow(df) > 0) {
        df$fecha_inicio <- as.POSIXct(as.character(df$fecha_inicio), tz = "Europe/Madrid")
        df$fecha_fin    <- as.POSIXct(as.character(df$fecha_fin), tz = "Europe/Madrid")
        df$color_display <- ifelse(df$estado == 'completada', "#9E9E9E", df$color)
        
        cal_data <- data.frame(
          id = as.character(df$id), 
          calendarId = as.character(df$gabinete),
          title = paste0(ifelse(df$estado == 'completada', "[✔] ", ""), df$paciente, " (", df$tipo_servicio, ")"), 
          start = df$fecha_inicio, end = df$fecha_fin, category = "time", 
          backgroundColor = df$color_display, borderColor = df$color_display
        )
        calendar(cal_data, navigation = TRUE, useDetailPopup = FALSE, isReadOnly = TRUE)
      } else {
        calendar(data.frame(), navigation = TRUE, isReadOnly = TRUE)
      }
    })
    
    # ---------------- 3. LÓGICA DE SOLAPE Y DISPONIBILIDAD (MySQL) ----------------
    
    check_solape <- function(fecha_inicio, fecha_fin, gabinete, doctor_id, cita_id = NULL) {
      query <- "SELECT COUNT(*) as count FROM citas 
                WHERE estado != 'cancelada'
                AND ((fecha_inicio < ? AND fecha_fin > ?))
                AND (gabinete = ? OR profesional_id = ?)"
      params <- list(fecha_fin, fecha_inicio, gabinete, doctor_id)
      
      if (!is.null(cita_id)) {
        query <- paste(query, "AND id != ?")
        params <- c(params, list(cita_id))
      }
      res <- DBI::dbGetQuery(pool, query, params = params)
      return(res$count > 0)
    }
    
    output$tabla_disponibilidad <- renderTable({
      req(input$fecha_cita, input$gabinete)
      
      # DATE_FORMAT para MySQL/MariaDB (%i es para minutos)
      query <- "SELECT 
                  DATE_FORMAT(fecha_inicio, '%H:%i') as Inicio, 
                  DATE_FORMAT(fecha_fin, '%H:%i') as Fin, 
                  u.nombre as Paciente
                FROM citas c
                JOIN usuarios u ON c.paciente_id = u.id
                WHERE DATE(c.fecha_inicio) = ? 
                AND c.gabinete = ? 
                AND c.estado != 'cancelada'
                ORDER BY fecha_inicio ASC"
      
      res <- DBI::dbGetQuery(pool, query, params = list(as.character(input$fecha_cita), input$gabinete))
      
      if(nrow(res) == 0) return(data.frame(Mensaje = "Gabinete libre todo el día"))
      res
    }, striped = TRUE, hover = TRUE, spacing = 'xs', width = '100%')
    
    output$feedback_solape <- renderUI({
      req(input$fecha_cita, input$hora_inicio, input$gabinete, input$doctor)
      
      t_start <- as.POSIXct(paste(input$fecha_cita, input$hora_inicio), tz = "Europe/Madrid")
      t_end   <- t_start + (input$duracion * 60)
      
      # Convertimos a string para la consulta SQL
      ts_str <- format(t_start, "%Y-%m-%d %H:%M:%S")
      te_str <- format(t_end, "%Y-%m-%d %H:%M:%S")
      
      if (check_solape(ts_str, te_str, input$gabinete, input$doctor, rv$cita_id)) {
        div(class="alert alert-danger p-2 mt-2", style="font-size: 0.8rem;",
            icon("exclamation-triangle"), " Conflicto: Horario ocupado en gabinete o por profesional.")
      } else {
        div(class="alert alert-success p-2 mt-2", style="font-size: 0.8rem;",
            icon("check-circle"), " Horario disponible.")
      }
    })
    
    output$horas_disponibles <- renderUI({
      req(input$fecha_cita)
      choices <- format(seq(as.POSIXct(paste(input$fecha_cita, "09:00")), 
                            as.POSIXct(paste(input$fecha_cita, "20:00")), by="15 min"), "%H:%M")
      
      sel <- NULL
      if (rv$edit_mode && !is.null(rv$cita_id)) {
        act <- DBI::dbGetQuery(pool, "SELECT fecha_inicio FROM citas WHERE id=?", params = list(rv$cita_id))
        if(nrow(act)>0) sel <- format(as.POSIXct(as.character(act$fecha_inicio), tz="Europe/Madrid"), "%H:%M")
      }
      selectInput(ns("hora_inicio"), "Hora de inicio", choices = choices, selected = sel)
    })
    
    # ---------------- 4. FORMULARIO DINÁMICO (MODAL) ----------------
    
    abrir_form <- function(mode = "new") {
      rv$edit_mode <- (mode == "edit")
      u <- get_user()
      
      # --- INICIALIZACIÓN ---
      cita_pasada <- FALSE
      cita_futura <- FALSE
      datos_cita <- NULL
      
      # --- 1. RECUPERAR DATOS SI ES EDICIÓN ---
      if (rv$edit_mode && !is.null(rv$cita_id)) {
        datos_cita <- DBI::dbGetQuery(pool, 
                                      "SELECT * FROM citas WHERE id = ?", 
                                      params = list(rv$cita_id))
        
        if (!is.null(datos_cita) && nrow(datos_cita) > 0) {
          fecha_f <- as.POSIXct(as.character(datos_cita$fecha_fin), tz = "Europe/Madrid")
          fecha_i <- as.POSIXct(as.character(datos_cita$fecha_inicio), tz = "Europe/Madrid")
          ahora   <- Sys.time()
          
          cita_pasada <- fecha_f < ahora
          cita_futura <- fecha_i > ahora
        }
      }
      
      # --- 2. LISTADOS ---
      pac <- DBI::dbGetQuery(pool, 
                             "SELECT id, nombre FROM usuarios WHERE tipo_usuario='paciente' ORDER BY nombre")
      
      doc <- DBI::dbGetQuery(pool, 
                             "SELECT id, nombre FROM usuarios WHERE tipo_usuario IN ('doctor','higienista') ORDER BY nombre")
      
      # --- 3. MODAL ---
      showModal(modalDialog(
        title = span(icon("calendar-check"), 
                     if(u$tipo_usuario == "paciente") " Detalle de mi Cita" 
                     else if(cita_pasada) " Historial de Cita" 
                     else " Gestión de Cita"),
        size = "l",
        
        # ================= CAPA PRINCIPAL =================
        div(id = ns("capa_principal"),
            
            # -------- PACIENTE --------
            if (u$tipo_usuario == "paciente") {
              
              div(class = "p-4 text-center",
                  
                  div(class = "mb-3", icon("calendar-day", class = "fa-4x text-purple")),
                  tags$h3("Resumen de tu Cita"),
                  hr(),
                  
                  if(!is.null(datos_cita) && nrow(datos_cita) > 0) {
                    
                    tagList(
                      tags$p(class="lead", "Tu cita está programada para el:"),
                      
                      tags$h4(class="fw-bold text-primary",
                              format(as.Date(datos_cita$fecha_inicio), "%d de %B, %Y")),
                      
                      tags$div(class="my-3",
                               tags$span(class="h5", icon("clock"), " Hora: ",
                                         tags$span(class="badge bg-dark",
                                                   format(as.POSIXct(as.character(datos_cita$fecha_inicio)), "%H:%M")))
                      ),
                      
                      tags$p(style="font-size: 1.1rem;", icon("user-md"), " Profesional: ",
                             tags$strong(doc$nombre[doc$id == datos_cita$profesional_id])),
                      
                      tags$p(style="font-size: 1.1rem;", icon("stethoscope"), " Tratamiento: ",
                             tags$strong(datos_cita$tipo_servicio)),
                      
                      # --- SOLICITUD CAMBIO ---
                      if(cita_futura) {
                        div(id = ns("seccion_solicitud"),
                            class = "mt-4 p-4 bg-light rounded border text-center",
                            
                            tags$h5(class="text-muted mb-3",
                                    "¿Necesitas cambiar o anular esta cita?"),
                            
                            div(style="max-width: 500px; margin: 0 auto;",
                                
                                tags$label("Explica tu necesidad:", class="fw-bold mb-2"),
                                
                                textAreaInput(ns("motivo_cambio"),
                                              label = NULL,
                                              placeholder = "Ej: No puedo asistir, cambiar a la tarde...",
                                              rows = 3,
                                              width = "100%"),
                                
                                div(class="mt-3 d-flex justify-content-center",
                                    actionButton(ns("btn_enviar_solicitud"),
                                                 "Enviar solicitud a recepción",
                                                 class = "btn-orange-pastel",
                                                 icon = icon("paper-plane"))
                                )
                            )
                        )
                      } else {
                        div(class = "alert alert-info mt-4 mx-auto",
                            style="max-width: 400px;",
                            icon("info-circle"),
                            " Esta cita ya ha pasado.")
                      }
                    )
                    
                  } else {
                    div(class="alert alert-danger",
                        "No se han podido cargar los datos de la cita.")
                  }
              )
              
              # -------- HISTORIAL --------
            } else if (cita_pasada) {
              
              div(class="p-3",
                  
                  div(style="max-width: 450px; margin: 0 auto;",
                      
                      div(style="
              color: #3f51b5;
              font-weight: 600;
              font-size: 14px;
              margin-bottom: 6px;
              display: flex;
              align-items: center;
              gap: 6px;
            ",
                          icon("notes-medical"),
                          "Anotación clínica"
                      ),
                      
                      textAreaInput(ns("observaciones"),
                                    label = NULL,
                                    value = if(!is.null(datos_cita)) datos_cita$observaciones else "",
                                    rows = 6,
                                    width = "100%",
                                    placeholder = "Ej: Evolución favorable del paciente...")
                  )
              )
              
              # -------- STAFF (CREAR / EDITAR) --------
            } else {
              
              div(class = "row g-4",
                  
                  # ---- FORMULARIO ----
                  div(class = "col-md-7",
                      
                      selectInput(ns("paciente"), "Paciente",
                                  choices = setNames(pac$id, pac$nombre),
                                  selected = if(!is.null(datos_cita)) datos_cita$paciente_id else NULL),
                      
                      selectInput(ns("doctor"), "Profesional",
                                  choices = setNames(doc$id, doc$nombre),
                                  selected = if(!is.null(datos_cita)) datos_cita$profesional_id else NULL),
                      
                      div(class="row",
                          div(class="col-4",
                              numericInput(ns("gabinete"), "Gab.",
                                           value = if(!is.null(datos_cita)) datos_cita$gabinete else 1,
                                           min=1, max=3)),
                          
                          div(class="col-8",
                              textInput(ns("servicio"), "Servicio",
                                        value = if(!is.null(datos_cita)) datos_cita$tipo_servicio else ""))
                      ),
                      
                      dateInput(ns("fecha_cita"), "Fecha",
                                value = if(!is.null(datos_cita)) as.Date(datos_cita$fecha_inicio) else Sys.Date()),
                      
                      uiOutput(ns("horas_disponibles")),
                      
                      numericInput(ns("duracion"), "Duración (min)",
                                   value = 30, step=15),
                      
                      uiOutput(ns("feedback_solape"))
                  ),
                  
                  # ---- DISPONIBILIDAD ----
                  div(class = "col-md-5",
                      div(class = "bg-light p-3 border h-100",
                          tableOutput(ns("tabla_disponibilidad"))
                      )
                  )
              )
            }
        ),
        
        # ================= CAPA CANCELACIÓN =================
        shinyjs::hidden(
          div(id = ns("capa_cancelacion"),
              class = "p-5 text-center",
              
              icon("trash-alt", class = "text-danger fa-4x mb-3"),
              tags$h3("¿Anular esta cita?"),
              
              actionButton(ns("btn_confirmar_si"),
                           "Sí, Anular",
                           class = "btn-orange-pastel"),
              
              actionButton(ns("btn_confirmar_no"),
                           "No",
                           class = "btn-dark-custom")
          )
        ),
        
        # ================= FOOTER =================
        footer = tagList(
          div(id = ns("footer_normal"),
              
              actionButton(ns("btn_cerrar_modal"),
                           "Cerrar",
                           class = "btn-dark-custom",
                           onclick = sprintf(
                             "Shiny.setInputValue('%s', Math.random())",
                             ns("close_modal"))
              ),
              
              if (u$tipo_usuario != "paciente") {
                tagList(
                  if (cita_pasada)
                    actionButton(ns("btn_finalizar"),
                                 "Guardar Historial",
                                 class = "btn-purple"),
                  
                  if (!cita_pasada)
                    actionButton(ns("guardar"),
                                 "Confirmar Cita",
                                 class = "btn-purple"),
                  
                  if (!cita_pasada && rv$edit_mode)
                    actionButton(ns("btn_cancelar_inicio"),
                                 "Anular",
                                 class = "btn-orange-pastel")
                )
              }
          )
        )
      ))
    }
    
    observeEvent(input$close_modal, {
      removeModal()
    })
    
    observeEvent(input$guardar, {
      u <- get_user()
      if (!u$tipo_usuario %in% c('admin', 'recepcion')) return()
      
      req(input$fecha_cita, input$hora_inicio)
      
      # --- FECHAS ---
      t_start <- as.POSIXct(paste(input$fecha_cita, input$hora_inicio), tz = "Europe/Madrid")
      t_end   <- t_start + (input$duracion * 60)
      
      ts_str <- format(t_start, "%Y-%m-%d %H:%M:%S")
      te_str <- format(t_end, "%Y-%m-%d %H:%M:%S")
      
      # --- VALIDACIÓN SOLAPE ---
      if (check_solape(ts_str, te_str, input$gabinete, input$doctor,
                       if (rv$edit_mode) rv$cita_id else NULL)) {
        showNotification("Error: Conflicto de horario.", type = "error")
        return()
      }
      
      color_hex <- switch(as.character(input$gabinete),
                          "1"="#7e57c2", "2"="#26a69a", "3"="#ffa726", "#26a69a")
      
      # ========================
      #  USAR MISMA CONEXIÓN (CLAVE)
      # ========================
      con <- pool::poolCheckout(pool)
      on.exit(pool::poolReturn(con), add = TRUE)
      
      tryCatch({
        
        # --- INSERT / UPDATE ---
        if (rv$edit_mode) {
          
          DBI::dbExecute(con,
                         "UPDATE citas 
         SET paciente_id=?, profesional_id=?, gabinete=?, fecha_inicio=?, fecha_fin=?, tipo_servicio=?, color=? 
         WHERE id=?",
                         params = list(input$paciente, input$doctor, input$gabinete,
                                       ts_str, te_str, input$servicio, color_hex, rv$cita_id)
          )
          
        } else {
          
          DBI::dbExecute(con,
                         "INSERT INTO citas (paciente_id, profesional_id, gabinete, fecha_inicio, fecha_fin, tipo_servicio, color, estado) 
         VALUES (?,?,?,?,?,?,?, 'programada')",
                         params = list(input$paciente, input$doctor, input$gabinete,
                                       ts_str, te_str, input$servicio, color_hex)
          )
          
          rv$cita_id <- DBI::dbGetQuery(con, "SELECT LAST_INSERT_ID() as id")$id
        }
        
        # ========================
        # EMAIL
        # ========================
        info_paciente <- DBI::dbGetQuery(con, "
      SELECT u.email, u.nombre 
      FROM citas c 
      JOIN usuarios u ON c.paciente_id = u.id 
      WHERE c.id = ?", 
                                         params = list(rv$cita_id)
        )
        
        estado_db <- DBI::dbGetQuery(con,
                                     "SELECT estado FROM citas WHERE id = ?",
                                     params = list(rv$cita_id)
        )$estado[1]
        
        mensaje_estado <- switch(estado_db,
                                 "cancelada" = "ha sido CANCELADA ❌",
                                 "completada" = "ha sido completada ✔",
                                 "programada" = if (rv$edit_mode) "ha sido MODIFICADA ✏️" else "ha sido PROGRAMADA 📅",
                                 "ha sido actualizada"
        )
        
        if (nrow(info_paciente) > 0 && !is.null(info_paciente$email)) {
          
            tryCatch({
              
              email_msg <- envelope() %>%
                from("clinicapptfg@gmail.com") %>%
                to(info_paciente$email) %>%
                subject("Actualización de su Cita - ClinicApp") %>%
                text(paste0(
                  "Hola ", info_paciente$nombre, ",\n\n",
                  "Su cita ", mensaje_estado, ".\n",
                  "Fecha: ", format(as.POSIXct(ts_str), "%d/%m/%Y a las %H:%M"), "\n\n",
                  "Gracias por confiar en nosotros."
                ))
              
              smtp_server(email_msg)
              
              print(paste("EMAIL ENVIADO A:", info_paciente$email))
              
            }, error = function(e) {
              print(paste("ERROR EMAIL:", e$message))
            })
        }
        
        # --- UI ---
        showNotification("Cita guardada correctamente.", type = "message")
        limpiar_todo()
        refresh(refresh() + 1)
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })    
    observeEvent(input$btn_cancelar_inicio, {
      shinyjs::hide("capa_principal"); shinyjs::hide("footer_normal"); shinyjs::show("capa_cancelacion")
    })
    
    observeEvent(input$btn_confirmar_no, {
      shinyjs::hide("capa_cancelacion"); shinyjs::show("capa_principal"); shinyjs::show("footer_normal")
    })
    
    observeEvent(input$btn_confirmar_si, {
      u <- get_user()
      
      if (u$tipo_usuario %in% c('admin', 'recepcion')) {
        
        # ========================
        # CONEXIÓN
        # ========================
        con <- pool::poolCheckout(pool)
        on.exit(pool::poolReturn(con), add = TRUE)
        
        tryCatch({
          
          # --- CANCELAR CITA ---
          DBI::dbExecute(con, 
                         "UPDATE citas SET estado='cancelada' WHERE id=?", 
                         params = list(rv$cita_id))
          
          # ========================
          # OBTENER DATOS PACIENTE
          # ========================
          info_paciente <- DBI::dbGetQuery(con, "
        SELECT u.email, u.nombre, c.fecha_inicio
        FROM citas c 
        JOIN usuarios u ON c.paciente_id = u.id 
        WHERE c.id = ?", 
                                           params = list(rv$cita_id)
          )
          
          # ========================
          #  ENVIAR EMAIL
          # ========================
          if (nrow(info_paciente) > 0 && 
              !is.na(info_paciente$email) && 
              info_paciente$email != "") {
            
            tryCatch({
              
              fecha_txt <- format(
                as.POSIXct(info_paciente$fecha_inicio, tz = "Europe/Madrid"),
                "%d/%m/%Y a las %H:%M"
              )
              
              email_msg <- envelope() %>%
                from("clinicapptfg@gmail.com") %>%
                to(info_paciente$email) %>%
                subject("Cancelación de su Cita - ClinicApp") %>%
                text(paste0(
                  "Hola ", info_paciente$nombre, ",\n\n",
                  "Le informamos que su cita ha sido CANCELADA ❌.\n\n",
                  "Fecha original: ", fecha_txt, "\n\n",
                  "Si desea reprogramarla, no dude en contactarnos.\n\n",
                  "Un saludo,\nClinicApp"
                ))
              
              smtp_server(email_msg)
              
              print(paste("EMAIL CANCELACIÓN ENVIADO A:", info_paciente$email))
              
            }, error = function(e) {
              print(paste("ERROR EMAIL CANCELACIÓN:", e$message))
            })
          }
          
          # ========================
          # UI
          # ========================
          showNotification("Cita cancelada.", type = "warning")
          limpiar_todo()
          refresh(refresh() + 1)
          
        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
        })
      }
    })
    
    observeEvent(input$btn_finalizar, {
      DBI::dbExecute(pool, "UPDATE citas SET estado='completada', observaciones=? WHERE id=?", params = list(input$observaciones, rv$cita_id))
      showNotification("Historial clínico actualizado.")
      limpiar_todo(); refresh(refresh() + 1)
    })
    
    observeEvent(input$archivar_solicitud, {
      DBI::dbExecute(pool, "UPDATE solicitudes_citas SET leido = TRUE WHERE id = ?", params = list(input$archivar_solicitud))
      refresh(refresh() + 1)
    })
    
    # --- GUARDAR SOLICITUD DEL PACIENTE ---
    observeEvent(input$btn_enviar_solicitud, {
      req(input$motivo_cambio, rv$cita_id)
      u <- get_user()
      
      # Insertamos en la tabla de solicitudes (asegúrate de haber creado la tabla en SQL)
      tryCatch({
        DBI::dbExecute(pool, 
                       "INSERT INTO solicitudes_citas (cita_id, paciente_id, motivo) VALUES (?, ?, ?)",
                       params = list(rv$cita_id, u$id, input$motivo_cambio)
        )
        
        showNotification("Solicitud enviada con éxito. Recepción la revisará pronto.", type = "message")
        removeModal() # Cerramos el formulario tras enviar
        refresh(refresh() + 1) # Refrescamos para que aparezca en la lista de recepción
      }, error = function(e) {
        showNotification("Error al enviar solicitud: Verifique la base de datos.", type = "error")
      })
    })
    
    # --- RENDERIZAR LA LISTA PARA RECEPCIÓN ---
    output$lista_solicitudes_pendientes <- renderUI({
      u <- get_user()
      
      # VALIDACIÓN DE SEGURIDAD: Si no hay usuario o no es staff, salir
      if (is.null(u) || is.null(u$tipo_usuario)) return(NULL)
      if (!(u$tipo_usuario %in% c("admin", "recepcion"))) return(NULL)
      
      refresh() # Dependencia reactiva
      
      # Traer solicitudes
      solicitudes <- DBI::dbGetQuery(pool, "
    SELECT s.*, u.nombre as paciente_nom, c.fecha_inicio 
    FROM solicitudes_citas s
    JOIN usuarios u ON s.paciente_id = u.id
    JOIN citas c ON s.cita_id = c.id
    WHERE s.leido = FALSE
    ORDER BY s.fecha_solicitud DESC")
      
      # Si la consulta falla o está vacía, no mostrar nada
      if(is.null(solicitudes) || nrow(solicitudes) == 0) return(NULL)
      
      tagList(
        hr(),
        h4(class="text-purple mb-3", icon("envelope-open-text"), " Solicitudes de cambio pendientes"),
        div(class="row",
            lapply(1:nrow(solicitudes), function(i) {
              s <- solicitudes[i, ]
              div(class="col-md-4",
                  div(class="card mb-3 border-start border-warning border-4 shadow-sm",
                      div(class="card-body",
                          tags$h6(class="fw-bold", s$paciente_nom),
                          tags$p(class="small text-muted mb-2", 
                                 icon("calendar"), " Cita original: ", format(as.POSIXct(s$fecha_inicio), "%d/%m %H:%M")),
                          div(class="p-2 bg-light rounded mb-2 small italic", paste0('"', s$motivo, '"')),
                          
                          div(class="d-flex gap-2",
                              actionButton(ns(paste0("gestionar_", s$id)), "Gestionar Cita", 
                                           class="btn btn-sm btn-purple",
                                           onclick = sprintf("Shiny.setInputValue('%s', %d, {priority:'event'})", ns("cal_click"), s$cita_id)),
                              actionButton(ns(paste0("leido_", s$id)), "Archivar", 
                                           class="btn btn-sm btn-outline-secondary",
                                           onclick = sprintf("Shiny.setInputValue('%s', %d, {priority:'event'})", ns("marcar_leido"), s$id))
                          )
                      )
                  )
              )
            })
        )
      )
    })
    # --- EVENTO PARA MARCAR COMO LEÍDO ---
    observeEvent(input$marcar_leido, {
      req(input$marcar_leido)
      
      tryCatch({
        DBI::dbExecute(pool, "UPDATE solicitudes_citas SET leido = TRUE WHERE id = ?", 
                       params = list(input$marcar_leido))
        refresh(refresh() + 1) # Esto forzará que la lista desaparezca
        showNotification("Solicitud archivada", type = "message")
      }, error = function(e) {
        showNotification("Error al archivar", type = "error")
      })
    })  
    
    observeEvent(input$cal_click, { 
      req(input$cal_click)
      
      id <- if (is.list(input$cal_click)) input$cal_click$id else input$cal_click
      
      rv$cita_id <- id
      abrir_form("edit") 
    }) 
    
    observeEvent(input$crear_manual, { rv$cita_id = NULL; abrir_form("new") })
    # ---------------- 6. UI Y FILTROS ----------------
    output$controles_staff <- renderUI({
      u <- get_user()
      
      # Protección 1: Verificar que 'u' existe y no es nulo
      req(u, u$tipo_usuario)
      
      # Protección 2: Si es paciente, no mostrar nada
      if (u$tipo_usuario == "paciente") return(NULL)
      
      # Usamos %in% en lugar de == para evitar errores de longitud cero
      es_gestor <- any(u$tipo_usuario %in% c('admin', 'recepcion'))
      
      div(class="row mb-2 align-items-end",
          div(class="col-md-3", 
              pickerInput(ns("filtro_gabinete"), "Gabinete", 
                          choices = c("Todos"="0", "Gabinete 1"="1", "Gabinete 2"="2", "Gabinete 3"="3"), 
                          selected="0")),
          div(class="col-md-3", uiOutput(ns("ui_filtro_doctor"))),
          div(class="col-md-3", 
              if (es_gestor) actionButton(ns("crear_manual"), 
                                          label = tagList(icon("plus"), " Nueva Cita"), 
                                          class="btn-purple") # <--- CAMBIO AQUÍ
          )
      )
    })
    output$ui_filtro_doctor <- renderUI({
      docs <- DBI::dbGetQuery(pool, "SELECT id, nombre FROM usuarios WHERE tipo_usuario IN ('doctor','higienista')")
      pickerInput(ns("filtro_doctor"), "Doctor", choices = c("Todos" = "0", setNames(docs$id, docs$nombre)), selected = "0")
    })
    

  })
}