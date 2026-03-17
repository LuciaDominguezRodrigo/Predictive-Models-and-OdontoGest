library(toastui)
library(shinyWidgets)

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
      cita_pasada <- FALSE
      datos_cita <- NULL
      
      # 1. Recuperar datos de la cita si es edición
      if (rv$edit_mode && !is.null(rv$cita_id)) {
        datos_cita <- DBI::dbGetQuery(pool, "SELECT * FROM citas WHERE id = ?", params = list(rv$cita_id))
        if (nrow(datos_cita) > 0) {
          cita_pasada <- as.POSIXct(as.character(datos_cita$fecha_fin), tz = "Europe/Madrid") < Sys.time()
        }
      }
      
      # 2. Cargar listados para personal (doctor/recepción)
      pac <- DBI::dbGetQuery(pool, "SELECT id, nombre FROM usuarios WHERE tipo_usuario='paciente' ORDER BY nombre")
      doc <- DBI::dbGetQuery(pool, "SELECT id, nombre FROM usuarios WHERE tipo_usuario IN ('doctor','higienista') ORDER BY nombre")
      
      showModal(modalDialog(
        title = span(icon("calendar-check"), if(u$tipo_usuario == "paciente") " Mis Citas" else if(cita_pasada) " Historial de Cita" else " Gestión de Cita"),
        size = "l", 
        
        # CONTENEDOR PRINCIPAL
        div(id = ns("capa_principal"),
            if (u$tipo_usuario == "paciente") {
              # --- VISTA PARA PACIENTES (RESUMEN SENCILLO) ---
              div(class = "p-4 text-center",
                  div(class = "mb-3", icon("calendar-day", class = "fa-4x text-purple")),
                  tags$h3("Resumen de tu Cita"),
                  hr(),
                  tags$p(class="lead", "Tienes una cita programada para el:"),
                  tags$h4(class="fw-bold text-primary", format(as.Date(datos_cita$fecha_inicio), "%d de %B, %Y")),
                  tags$div(class="my-3",
                           tags$span(class="h5", icon("clock"), " Hora: ", tags$span(class="badge bg-dark", format(as.POSIXct(datos_cita$fecha_inicio), "%H:%M")))
                  ),
                  tags$p(style="font-size: 1.1rem;", icon("user-md"), " Profesional: ", tags$strong(doc$nombre[doc$id == datos_cita$profesional_id])),
                  tags$p(style="font-size: 1.1rem;", icon("stethoscope"), " Tratamiento: ", tags$strong(datos_cita$tipo_servicio)),
                  div(class = "alert alert-info mt-4 mx-auto", style="max-width: 400px;",
                      icon("info-circle"), " Recuerda acudir con puntualidad.")
              )
              
            } else if (cita_pasada) {
              # --- VISTA HISTORIAL (SOLO OBSERVACIONES PARA DOCTORES) ---
              div(class="p-3",
                  tags$h5(class="fw-bold mb-3", "Evolución y Notas Clínicas"),
                  textAreaInput(ns("observaciones"), "Observaciones Médicas / Notas de la sesión", 
                                value = if(!is.null(datos_cita)) datos_cita$observaciones else "", 
                                rows = 12, placeholder = "Escribe aquí los detalles del tratamiento realizado...")
              )
              
            } else {
              # --- VISTA EDICIÓN/NUEVA (RECEPCIÓN Y ADMIN) ---
              div(class = "row g-4", 
                  div(class = "col-md-7",
                      div(class="px-2",
                          selectInput(ns("paciente"), "Paciente", 
                                      choices = setNames(pac$id, pac$nombre), 
                                      selected = if(!is.null(datos_cita)) datos_cita$paciente_id else NULL, 
                                      width = "100%"),
                          selectInput(ns("doctor"), "Profesional a cargo", 
                                      choices = setNames(doc$id, doc$nombre), 
                                      selected = if(!is.null(datos_cita)) datos_cita$profesional_id else NULL,
                                      width = "100%"),
                          hr(class="my-4"),
                          div(class="row",
                              div(class="col-4", numericInput(ns("gabinete"), "Gabinete", value = if(!is.null(datos_cita)) datos_cita$gabinete else 1, min=1, max=3)),
                              div(class="col-8", textInput(ns("servicio"), "Tratamiento / Servicio", value = if(!is.null(datos_cita)) datos_cita$tipo_servicio else ""))
                          ),
                          div(class="row mt-2",
                              div(class="col-6", dateInput(ns("fecha_cita"), "Fecha prevista", value = if(!is.null(datos_cita)) as.Date(datos_cita$fecha_inicio) else Sys.Date())),
                              div(class="col-6", numericInput(ns("duracion"), "Duración estimada (min)", value = 30, step=15))
                          ),
                          uiOutput(ns("horas_disponibles")),
                          uiOutput(ns("feedback_solape"))
                      )
                  ),
                  div(class = "col-md-5",
                      div(class = "bg-light p-3 rounded-3 border h-100",
                          tags$h6(class="fw-bold mb-3 text-purple", icon("clock"), "Estado del Gabinete"),
                          div(style = "max-height: 450px; overflow-y: auto;",
                              tableOutput(ns("tabla_disponibilidad"))
                          )
                      )
                  )
              )
            }
        ),
        
        # CAPA DE CANCELACIÓN (OCULTA POR DEFECTO)
        shinyjs::hidden(
          div(id = ns("capa_cancelacion"), class = "p-5 text-center",
              icon("trash-alt", class = "text-danger fa-4x mb-3"),
              tags$h3("¿Anular esta cita?"),
              tags$p(class="text-muted", "Esta acción eliminará la cita del calendario y liberará el gabinete."),
              hr(),
              div(class="d-flex justify-content-center gap-3",
                  actionButton(ns("btn_confirmar_si"), "Sí, Anular Cita", class = "btn-orange-pastel"),
                  actionButton(ns("btn_confirmar_no"), "No, Mantenerla", class = "btn-dark-custom")
              )
          )
        ),
        
        footer = tagList(
          div(id = ns("footer_normal"),
              # Botón Cerrar (Negro)
              actionButton(ns("btn_cerrar_modal"), "Cerrar", class = "btn-dark-custom", 
                           onclick = sprintf("Shiny.setInputValue('%s', Math.random())", ns("close_modal"))),
              
              # Acciones según usuario (No se muestran al paciente)
              if (u$tipo_usuario != "paciente") {
                tagList(
                  if (cita_pasada && u$tipo_usuario %in% c("admin", "doctor", "higienista"))
                    actionButton(ns("btn_finalizar"), "Guardar Cambios", class = "btn-purple"),
                  
                  if (!cita_pasada && u$tipo_usuario %in% c("admin", "recepcion"))
                    actionButton(ns("guardar"), "Confirmar Cita", class = "btn-purple"),
                  
                  if (!cita_pasada && rv$edit_mode && u$tipo_usuario %in% c("admin", "recepcion"))
                    actionButton(ns("btn_cancelar_inicio"), "Anular Cita", class = "btn-orange-pastel")
                )
              }
          )
        )
      ))
      
      # Bloqueo de edición para profesionales en citas futuras
      if (u$tipo_usuario %in% c("doctor", "higienista") && !cita_pasada) {
        shinyjs::disable("capa_principal")
      }
    }    # ---------------- 5. EVENTOS DE BASE DE DATOS ----------------
    
    observeEvent(input$close_modal, {
      removeModal()
    })
    observeEvent(input$guardar, {
      u <- get_user()
      if (!u$tipo_usuario %in% c('admin', 'recepcion')) return()
      
      req(input$fecha_cita, input$hora_inicio)
      t_start <- as.POSIXct(paste(input$fecha_cita, input$hora_inicio), tz = "Europe/Madrid")
      t_end   <- t_start + (input$duracion * 60)
      
      ts_str <- format(t_start, "%Y-%m-%d %H:%M:%S")
      te_str <- format(t_end, "%Y-%m-%d %H:%M:%S")
      
      if (check_solape(ts_str, te_str, input$gabinete, input$doctor, if(rv$edit_mode) rv$cita_id else NULL)) {
        showNotification("Error: Conflicto de horario.", type = "error")
        return()
      }
      
      color_hex <- switch(as.character(input$gabinete), "1"="#7e57c2", "2"="#26a69a", "3"="#ffa726", "#26a69a")
      
      if (rv$edit_mode) {
        DBI::dbExecute(pool, "UPDATE citas SET paciente_id=?, profesional_id=?, gabinete=?, fecha_inicio=?, fecha_fin=?, tipo_servicio=?, color=? WHERE id=?",
                       params = list(input$paciente, input$doctor, input$gabinete, ts_str, te_str, input$servicio, color_hex, rv$cita_id))
      } else {
        DBI::dbExecute(pool, "INSERT INTO citas (paciente_id, profesional_id, gabinete, fecha_inicio, fecha_fin, tipo_servicio, color, estado) VALUES (?,?,?,?,?,?,?, 'programada')",
                       params = list(input$paciente, input$doctor, input$gabinete, ts_str, te_str, input$servicio, color_hex))
      }
      showNotification("Cita procesada correctamente.")
      limpiar_todo(); refresh(refresh() + 1)
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
        DBI::dbExecute(pool, "UPDATE citas SET estado='cancelada' WHERE id=?", params = list(rv$cita_id))
        showNotification("Cita eliminada.", type = "warning")
        limpiar_todo(); refresh(refresh() + 1)
      }
    })
    
    observeEvent(input$btn_finalizar, {
      DBI::dbExecute(pool, "UPDATE citas SET estado='completada', observaciones=? WHERE id=?", params = list(input$observaciones, rv$cita_id))
      showNotification("Historial clínico actualizado.")
      limpiar_todo(); refresh(refresh() + 1)
    })
    
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
    
    observeEvent(input$cal_click, { req(input$cal_click$id); rv$cita_id <- input$cal_click$id; abrir_form("edit") })
    observeEvent(input$crear_manual, { rv$cita_id = NULL; abrir_form("new") })
  })
}