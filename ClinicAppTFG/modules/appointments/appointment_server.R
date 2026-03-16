appointmentServer <- function(id, pool, current_user){
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    refresh <- reactiveVal(0)
    
    rv <- reactiveValues(
      cita_id = NULL,
      edit_mode = FALSE
    )
    
    # --- FUNCIÓN DE LIMPIEZA ---
    limpiar_todo <- function() {
      removeModal()
      shinyjs::runjs("$('body').removeClass('modal-open'); $('.modal-backdrop').remove();")
    }
    
    # ---------------- FILTRO DOCTOR (SUPERIOR) ----------------
    output$ui_filtro_doctor <- renderUI({
      docs <- DBI::dbGetQuery(pool, "SELECT id, nombre FROM usuarios WHERE tipo_usuario IN ('doctor','higienista')")
      shinyWidgets::pickerInput(ns("filtro_doctor"), "Filtrar por Profesional", 
                                choices = c("Todos" = "0", setNames(docs$id, docs$nombre)), 
                                selected = "0", width = "100%")
    })
    
    # ---------------- CALENDARIO (FILTROS CORREGIDOS) ----------------
    output$cal <- renderCalendar({
      refresh()
      
      # Base de la consulta
      query <- "SELECT c.*, p.nombre paciente, d.nombre doctor 
                FROM citas c 
                JOIN usuarios p ON c.paciente_id = p.id 
                JOIN usuarios d ON c.profesional_id = d.id
                WHERE c.estado != 'cancelada'"
      
      params <- list()
      
      # Filtro Gabinete
      if (!is.null(input$filtro_gabinete) && input$filtro_gabinete != "0") {
        query <- paste(query, "AND c.gabinete = ?")
        params <- c(params, list(input$filtro_gabinete))
      }
      
      # Filtro Doctor
      if (!is.null(input$filtro_doctor) && input$filtro_doctor != "0") {
        query <- paste(query, "AND c.profesional_id = ?")
        params <- c(params, list(input$filtro_doctor))
      }
      
      # Ejecución segura: Solo enviamos params si la lista no está vacía
      df <- if (length(params) > 0) {
        DBI::dbGetQuery(pool, query, params = params)
      } else {
        DBI::dbGetQuery(pool, query)
      }
      
      if (nrow(df) > 0) {
        # Corrección de horas para evitar desfases UTC
        df$fecha_inicio <- as.POSIXct(as.character(df$fecha_inicio), tz = "Europe/Madrid")
        df$fecha_fin   <- as.POSIXct(as.character(df$fecha_fin), tz = "Europe/Madrid")
        
        df$color_display <- ifelse(df$estado == 'completada', "#9E9E9E", df$color)
        
        cal_df <- data.frame(
          id = as.character(df$id), 
          calendarId = as.character(df$gabinete),
          title = paste0(ifelse(df$estado == 'completada', "[✔] ", ""), df$paciente, " (", df$tipo_servicio, ")"), 
          start = df$fecha_inicio, 
          end = df$fecha_fin, 
          category = "time", 
          backgroundColor = df$color_display, 
          borderColor = df$color_display
        )
      } else { cal_df <- data.frame() }
      
      calendar(cal_df, navigation = TRUE, useCreationPopup = FALSE, useDetailPopup = FALSE, isReadOnly = TRUE)
    })
    
    # ---------------- FORMULARIO DINÁMICO ----------------
    abrir_form <- function(mode = "new") {
      rv$edit_mode <- (mode == "edit")
      user_data <- if (is.function(current_user)) current_user() else current_user
      u_tipo <- tolower(user_data$tipo_usuario)
      
      cita_pasada <- FALSE
      datos_cita <- NULL
      
      if (rv$edit_mode && !is.null(rv$cita_id)) {
        datos_cita <- DBI::dbGetQuery(pool, "SELECT fecha_fin, estado, observaciones FROM citas WHERE id = ?", params = list(rv$cita_id))
        if (nrow(datos_cita) > 0) {
          cita_pasada <- as.POSIXct(as.character(datos_cita$fecha_fin), tz = "Europe/Madrid") < Sys.time()
        }
      }
      
      pac <- DBI::dbGetQuery(pool, "SELECT id, nombre FROM usuarios WHERE tipo_usuario='paciente'")
      doc <- DBI::dbGetQuery(pool, "SELECT id, nombre FROM usuarios WHERE tipo_usuario IN ('doctor','higienista')")
      
      showModal(modalDialog(
        title = if (cita_pasada) "Cita Finalizada - Observaciones" else if (rv$edit_mode) "Editar Cita" else "Programar Nueva Cita",
        size = "m",
        div(id = ns("capa_principal"),
            if (cita_pasada) {
              tagList(
                div(class = "alert alert-info", icon("stethoscope"), " Ingrese los detalles clínicos para cerrar el historial de esta cita."),
                textAreaInput(ns("observaciones"), "Observaciones Médicas", 
                              value = if (!is.null(datos_cita)) datos_cita$observaciones else "", rows = 8),
                shinyjs::hidden(dateInput(ns("fecha_cita"), "", value = Sys.Date())),
                shinyjs::hidden(textInput(ns("hora_inicio"), "", value = "00:00")),
                shinyjs::hidden(numericInput(ns("duracion"), "", value = 30))
              )
            } else {
              tagList(
                div(id = ns("campos_clinicos"),
                    div(class = "row",
                        div(class = "col-md-6", selectInput(ns("paciente"), "Paciente", choices = setNames(pac$id, pac$nombre))),
                        div(class = "col-md-6", selectInput(ns("doctor"), "Profesional", choices = setNames(doc$id, doc$nombre)))
                    ),
                    div(class = "row",
                        div(class = "col-md-4", numericInput(ns("gabinete"), "Gabinete", value = 1, min = 1, max = 3)),
                        div(class = "col-md-8", textInput(ns("servicio"), "Servicio / Tratamiento", value = "Revisión"))
                    ),
                    div(class = "row",
                        div(class = "col-md-6", dateInput(ns("fecha_cita"), "Fecha", value = Sys.Date())),
                        div(class = "col-md-6", numericInput(ns("duracion"), "Duración (min)", value = 30, step = 15, min = 15))
                    ),
                    uiOutput(ns("horas_disponibles"))
                ),
                hr(),
                div(style = "background: #fdfdfd; padding: 10px; border: 1px solid #eee; border-radius: 8px;",
                    tags$strong("Ocupación hoy en este gabinete:"),
                    tableOutput(ns("resumen_agenda"))
                )
              )
            }
        ),
        
        shinyjs::hidden(
          div(id = ns("capa_cancelacion"), class = "alert alert-danger text-center",
              tags$h4("¿Desea eliminar esta cita?"),
              p("Esta acción no se puede deshacer."),
              actionButton(ns("btn_confirmar_si"), "Sí, eliminar", class = "btn-danger"),
              actionButton(ns("btn_confirmar_no"), "No, volver", class = "btn-default")
          )
        ),
        
        footer = tagList(
          div(id = ns("footer_normal"),
              modalButton("Cerrar"),
              if (cita_pasada) {
                actionButton(ns("btn_finalizar"), "Guardar y Completar", class = "btn-primary")
              } else {
                tagList(
                  if (rv$edit_mode && u_tipo %in% c("admin", "recepcion")) 
                    actionButton(ns("btn_cancelar_inicio"), "Eliminar", class = "btn-danger"),
                  actionButton(ns("guardar"), "Guardar Cita", class = "btn-success")
                )
              }
          )
        )
      ))
      if (u_tipo == "doctor" && rv$edit_mode && !cita_pasada) shinyjs::disable("campos_clinicos")
    }
    
    # ---------------- HORAS DISPONIBLES (ESTRICTO) ----------------
    output$horas_disponibles <- renderUI({
      req(input$fecha_cita, input$gabinete, input$doctor)
      fecha <- as.Date(input$fecha_cita)
      slots <- seq(as.POSIXct(paste(fecha, "09:00:00"), tz = "Europe/Madrid"), 
                   as.POSIXct(paste(fecha, "20:00:00"), tz = "Europe/Madrid"), by = "15 min")
      
      ocupadas <- DBI::dbGetQuery(pool, "SELECT id, fecha_inicio, fecha_fin FROM citas 
                                         WHERE DATE(fecha_inicio)=? AND estado!='cancelada' 
                                         AND (gabinete=? OR profesional_id=?)", 
                                  params = list(fecha, input$gabinete, input$doctor))
      
      if (rv$edit_mode) ocupadas <- ocupadas[ocupadas$id != rv$cita_id, ]
      
      libres <- Filter(function(s) {
        if (nrow(ocupadas) == 0) return(TRUE)
        s_char <- format(s, "%Y-%m-%d %H:%M:%S")
        !any(s_char >= as.character(ocupadas$fecha_inicio) & s_char < as.character(ocupadas$fecha_fin))
      }, slots)
      
      sel <- NULL
      if (rv$edit_mode) {
        act <- DBI::dbGetQuery(pool, "SELECT fecha_inicio FROM citas WHERE id=?", params = list(rv$cita_id))
        if (nrow(act) > 0) sel <- format(as.POSIXct(as.character(act$fecha_inicio), tz = "Europe/Madrid"), "%H:%M")
      }
      selectInput(ns("hora_inicio"), "Hora de inicio", choices = format(libres, "%H:%M"), selected = sel)
    })
    
    # ---------------- RESUMEN AGENDA ----------------
    output$resumen_agenda <- renderTable({
      req(input$fecha_cita, input$gabinete)
      DBI::dbGetQuery(pool, "SELECT DATE_FORMAT(fecha_inicio, '%H:%i') as Inicio, 
                             DATE_FORMAT(fecha_fin, '%H:%i') as Fin, 
                             u.nombre as Paciente
                             FROM citas c JOIN usuarios u ON c.paciente_id = u.id
                             WHERE DATE(c.fecha_inicio) = ? AND c.gabinete = ? AND c.estado != 'cancelada'
                             ORDER BY fecha_inicio ASC", params = list(input$fecha_cita, input$gabinete))
    }, width = "100%")
    
    # ---------------- GUARDADO / EDITAR ----------------
    observeEvent(input$guardar, {
      req(input$fecha_cita, input$hora_inicio, input$duracion)
      t_start <- as.POSIXct(paste(input$fecha_cita, input$hora_inicio), tz = "Europe/Madrid")
      t_end <- t_start + (input$duracion * 60)
      
      ts_str <- format(t_start, "%Y-%m-%d %H:%M:%S")
      te_str <- format(t_end, "%Y-%m-%d %H:%M:%S")
      
      # Validación final contra la BD
      check <- DBI::dbGetQuery(pool, "SELECT COUNT(*) as count FROM citas WHERE estado!='cancelada' 
                                      AND (gabinete=? OR profesional_id=?) 
                                      AND ((fecha_inicio < ? AND fecha_fin > ?) OR (fecha_inicio >= ? AND fecha_inicio < ?))",
                               params = list(input$gabinete, input$doctor, te_str, ts_str, ts_str, te_str))
      
      if (rv$edit_mode) { 
        # Restar si choca con ella misma
        check_self <- DBI::dbGetQuery(pool, "SELECT COUNT(*) as count FROM citas WHERE id=? 
                                             AND ((fecha_inicio < ? AND fecha_fin > ?) OR (fecha_inicio >= ? AND fecha_inicio < ?))",
                                      params = list(rv$cita_id, te_str, ts_str, ts_str, te_str))
        check$count <- check$count - check_self$count
      }
      
      if (check$count > 0) {
        showNotification("No se puede guardar: Conflicto de horario con otra cita.", type = "error")
        return()
      }
      
      color_hex <- switch(as.character(input$gabinete), "1" = "#7e57c2", "2" = "#26a69a", "3" = "#ffa726", "#26a69a")
      
      if (rv$edit_mode) {
        DBI::dbExecute(pool, "UPDATE citas SET paciente_id=?, profesional_id=?, gabinete=?, fecha_inicio=?, fecha_fin=?, tipo_servicio=?, color=? WHERE id=?",
                       params = list(input$paciente, input$doctor, input$gabinete, ts_str, te_str, input$servicio, color_hex, rv$cita_id))
        showNotification("Cita actualizada.")
      } else {
        DBI::dbExecute(pool, "INSERT INTO citas (paciente_id, profesional_id, gabinete, fecha_inicio, fecha_fin, tipo_servicio, color, estado) VALUES (?,?,?,?,?,?,?, 'programada')",
                       params = list(input$paciente, input$doctor, input$gabinete, ts_str, te_str, input$servicio, color_hex))
        showNotification("Cita creada correctamente.")
      }
      limpiar_todo(); refresh(refresh() + 1)
    })
    
    # --- FINALIZAR ---
    observeEvent(input$btn_finalizar, {
      DBI::dbExecute(pool, "UPDATE citas SET estado = 'completada', observaciones = ? WHERE id = ?",
                     params = list(input$observaciones, rv$cita_id))
      showNotification("Historial clínico guardado.")
      limpiar_todo(); refresh(refresh() + 1)
    })
    
    # --- CLIC CALENDARIO ---
    observeEvent(input$cal_click, {
      req(input$cal_click$id)
      rv$cita_id <- input$cal_click$id
      abrir_form("edit")
    })
    
    observeEvent(input$crear_manual, { rv$cita_id <- NULL; abrir_form("new") })
    
    # --- CANCELACIÓN ---
    observeEvent(input$btn_cancelar_inicio, { shinyjs::hide("capa_principal"); shinyjs::hide("footer_normal"); shinyjs::show("capa_cancelacion") })
    observeEvent(input$btn_confirmar_no, { shinyjs::hide("capa_cancelacion"); shinyjs::show("capa_principal"); shinyjs::show("footer_normal") })
    observeEvent(input$btn_confirmar_si, { 
      DBI::dbExecute(pool, "UPDATE citas SET estado = 'cancelada' WHERE id = ?", params = list(rv$cita_id))
      limpiar_todo(); refresh(refresh() + 1); showNotification("Cita cancelada.", type = "warning")
    })
  })
}