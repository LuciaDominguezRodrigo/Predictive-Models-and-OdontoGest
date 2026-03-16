appointmentServer <- function(id, pool, current_user){
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    refresh <- reactiveVal(0)
    
    rv <- reactiveValues(
      cita_id = NULL,
      edit_mode = FALSE
    )
    
    limpiar_todo <- function() {
      removeModal()
      shinyjs::runjs("$('body').removeClass('modal-open'); $('.modal-backdrop').remove();")
    }
    
    # ---------------- CALENDARIO ----------------
    output$cal <- renderCalendar({
      refresh()
      df <- DBI::dbGetQuery(pool, "SELECT c.*, p.nombre paciente, d.nombre doctor FROM citas c 
                                   JOIN usuarios p ON c.paciente_id = p.id JOIN usuarios d ON c.profesional_id = d.id
                                   WHERE c.estado != 'cancelada'")
      if(nrow(df) > 0){
        df$fecha_inicio <- as.POSIXct(df$fecha_inicio, tz="Europe/Madrid")
        df$fecha_fin   <- as.POSIXct(df$fecha_fin, tz="Europe/Madrid")
        df$color_display <- ifelse(df$estado == 'completada', "#9E9E9E", df$color)
        
        cal_df <- data.frame(id = as.character(df$id), calendarId = as.character(df$gabinete),
                             title = paste(ifelse(df$estado == 'completada', "[✔] ", ""), df$paciente,"-",df$tipo_servicio), 
                             start = df$fecha_inicio, end = df$fecha_fin, category = "time", 
                             backgroundColor = df$color_display, borderColor = df$color_display)
      } else { cal_df <- data.frame() }
      calendar(cal_df, navigation = TRUE, useCreationPopup = FALSE, useDetailPopup = FALSE, isReadOnly = TRUE)
    })
    
    # ---------------- FORMULARIO DINÁMICO ----------------
    abrir_form <- function(mode = "new"){
      rv$edit_mode <- (mode == "edit")
      user_data <- if(is.function(current_user)) current_user() else current_user
      u_tipo <- tolower(user_data$tipo_usuario)
      
      cita_pasada <- FALSE
      datos_cita <- NULL
      
      if(rv$edit_mode && !is.null(rv$cita_id)) {
        datos_cita <- DBI::dbGetQuery(pool, "SELECT fecha_fin, estado, observaciones FROM citas WHERE id = ?", params = list(rv$cita_id))
        if(nrow(datos_cita) > 0) {
          # Si la fecha de fin es menor a "ahora", es una cita para finalizar/observaciones
          cita_pasada <- as.POSIXct(datos_cita$fecha_fin) < Sys.time()
        }
      }
      
      pac <- DBI::dbGetQuery(pool, "SELECT id,nombre FROM usuarios WHERE tipo_usuario='paciente'")
      doc <- DBI::dbGetQuery(pool, "SELECT id,nombre FROM usuarios WHERE tipo_usuario IN ('doctor','higienista')")
      
      showModal(modalDialog(
        title = if(cita_pasada) "Finalizar Cita / Observaciones" else if(rv$edit_mode) "Editar Cita" else "Nueva Cita",
        size = "m",
        div(id = ns("capa_principal"),
            if(cita_pasada) {
              tagList(
                div(class="alert alert-info", "Esta cita ya ha pasado. Introduzca las observaciones clínicas para darla por completada."),
                textAreaInput(ns("observaciones"), "Observaciones Médicas", 
                              value = if(!is.null(datos_cita)) datos_cita$observaciones else "", rows = 8),
                # Inputs ocultos para que el botón guardar no falle por falta de req()
                shinyjs::hidden(dateInput(ns("fecha_cita"), "", value = Sys.Date())),
                shinyjs::hidden(textInput(ns("hora_inicio"), "", value = "00:00"))
              )
            } else {
              tagList(
                div(id = ns("campos_clinicos"),
                    div(class="row",
                        div(class="col-md-6", selectInput(ns("paciente"), "Paciente", choices=setNames(pac$id,pac$nombre))),
                        div(class="col-md-6", selectInput(ns("doctor"), "Profesional", choices=setNames(doc$id,doc$nombre)))
                    ),
                    div(class="row",
                        div(class="col-md-4", numericInput(ns("gabinete"), "Gabinete", value=1, min=1, max=3)),
                        div(class="col-md-8", textInput(ns("servicio"), "Servicio", value="Revisión"))
                    ),
                    div(class="row",
                        div(class="col-md-6", dateInput(ns("fecha_cita"), "Fecha", value=Sys.Date())),
                        div(class="col-md-6", numericInput(ns("duracion"), "Duración (min)", value=30, step=15, min=15))
                    ),
                    uiOutput(ns("horas_disponibles"))
                ),
                hr(),
                div(style="background: #f8f9fa; padding: 10px; border: 1px solid #ddd; border-radius: 5px;",
                    tags$strong("Agenda del Gabinete:"),
                    tableOutput(ns("resumen_agenda"))
                )
              )
            }
        ),
        
        shinyjs::hidden(
          div(id = ns("capa_cancelacion"), class="alert alert-danger text-center",
              tags$h4("¿Eliminar esta cita?"),
              actionButton(ns("btn_confirmar_si"), "Confirmar", class="btn-danger"),
              actionButton(ns("btn_confirmar_no"), "Volver", class="btn-default")
          )
        ),
        
        footer = tagList(
          div(id = ns("footer_normal"),
              modalButton("Volver"),
              if(cita_pasada) {
                actionButton(ns("btn_finalizar"), "Guardar y Completar", class="btn-primary")
              } else {
                tagList(
                  if(rv$edit_mode && u_tipo %in% c("admin", "recepcion")) 
                    actionButton(ns("btn_cancelar_inicio"), "Eliminar", class="btn-danger"),
                  actionButton(ns("guardar"), "Guardar", class="btn-success")
                )
              }
          )
        )
      ))
      
      # Bloqueo para médicos en citas futuras
      if(u_tipo == "doctor" && rv$edit_mode && !cita_pasada) shinyjs::disable("campos_clinicos")
    }
    
    # ---------------- LÓGICA DE HORAS ----------------
    output$horas_disponibles <- renderUI({
      req(input$fecha_cita, input$gabinete, input$doctor)
      fecha <- as.Date(input$fecha_cita)
      slots <- seq(as.POSIXct(paste(fecha, "09:00:00"), tz="Europe/Madrid"), 
                   as.POSIXct(paste(fecha, "20:00:00"), tz="Europe/Madrid"), by="15 min")
      
      query <- "SELECT id, fecha_inicio, fecha_fin FROM citas 
                WHERE DATE(fecha_inicio)=? AND estado!='cancelada' 
                AND (gabinete = ? OR profesional_id = ?)"
      ocupadas <- DBI::dbGetQuery(pool, query, params = list(fecha, input$gabinete, input$doctor))
      
      if(rv$edit_mode && !is.null(rv$cita_id)) ocupadas <- ocupadas[ocupadas$id != rv$cita_id, ]
      
      libres <- Filter(function(s) {
        if(nrow(ocupadas) == 0) return(TRUE)
        !any(s >= as.POSIXct(ocupadas$fecha_inicio, tz="Europe/Madrid") & 
               s <  as.POSIXct(ocupadas$fecha_fin, tz="Europe/Madrid"))
      }, slots)
      
      sel <- NULL
      if(rv$edit_mode) {
        act <- DBI::dbGetQuery(pool, "SELECT fecha_inicio FROM citas WHERE id=?", params=list(rv$cita_id))
        if(nrow(act)>0) sel <- format(as.POSIXct(act$fecha_inicio), "%H:%M")
      }
      selectInput(ns("hora_inicio"), "Hora de inicio disponible", choices = format(libres, "%H:%M"), selected = sel)
    })
    
    output$resumen_agenda <- renderTable({
      req(input$fecha_cita, input$gabinete)
      DBI::dbGetQuery(pool, "SELECT DATE_FORMAT(fecha_inicio, '%H:%i') as Inicio, 
                             DATE_FORMAT(fecha_fin, '%H:%i') as Fin, u.nombre as Paciente
                             FROM citas c JOIN usuarios u ON c.paciente_id = u.id
                             WHERE DATE(c.fecha_inicio) = ? AND c.gabinete = ? AND c.estado != 'cancelada'
                             ORDER BY fecha_inicio ASC", params = list(input$fecha_cita, input$gabinete))
    }, width = "100%")
    
    # ---------------- GUARDAR Y FINALIZAR ----------------
    observeEvent(input$btn_finalizar, {
      DBI::dbExecute(pool, "UPDATE citas SET estado = 'completada', observaciones = ? WHERE id = ?",
                     params = list(input$observaciones, rv$cita_id))
      showNotification("Cita finalizada con observaciones.")
      limpiar_todo(); refresh(refresh() + 1)
    })
    
    observeEvent(input$guardar, {
      req(input$fecha_cita, input$hora_inicio, input$duracion)
      
      t_start <- as.POSIXct(paste(input$fecha_cita, input$hora_inicio), tz="Europe/Madrid")
      t_end <- t_start + (input$duracion * 60)
      
      ts_str <- format(t_start, "%Y-%m-%d %H:%M:%S")
      te_str <- format(t_end, "%Y-%m-%d %H:%M:%S")
      
      # VALIDACIÓN ESTRICTA (Incluso en edición)
      check_q <- "SELECT COUNT(*) as count FROM citas 
                  WHERE estado != 'cancelada' 
                  AND (gabinete = ? OR profesional_id = ?)
                  AND ((fecha_inicio < ? AND fecha_fin > ?) OR (fecha_inicio >= ? AND fecha_inicio < ?))"
      
      params_check <- list(input$gabinete, input$doctor, te_str, ts_str, ts_str, te_str)
      if(rv$edit_mode) {
        check_q <- paste(check_q, "AND id != ?")
        params_check <- c(params_check, rv$cita_id)
      }
      
      if(DBI::dbGetQuery(pool, check_q, params = params_check)$count > 0) {
        showNotification("No se puede guardar: El doctor o el gabinete ya tienen una cita en ese horario.", type = "error")
        return()
      }
      
      color_hex <- switch(as.character(input$gabinete), "1"="#7e57c2", "2"="#26a69a", "3"="#ffa726", "#26a69a")
      
      if(rv$edit_mode){
        DBI::dbExecute(pool, "UPDATE citas SET paciente_id=?, profesional_id=?, gabinete=?, fecha_inicio=?, fecha_fin=?, tipo_servicio=?, color=? WHERE id=?",
                       params = list(input$paciente, input$doctor, input$gabinete, ts_str, te_str, input$servicio, color_hex, rv$cita_id))
      } else {
        DBI::dbExecute(pool, "INSERT INTO citas (paciente_id, profesional_id, gabinete, fecha_inicio, fecha_fin, tipo_servicio, color, estado) VALUES (?,?,?,?,?,?,?, 'programada')",
                       params = list(input$paciente, input$doctor, input$gabinete, ts_str, te_str, input$servicio, color_hex))
      }
      limpiar_todo(); refresh(refresh()+1)
    })
    
    # --- EVENTOS DE CLIC ---
    observeEvent(input$crear_manual, { rv$cita_id <- NULL; abrir_form("new") })
    observeEvent(input$cal_click, {
      req(input$cal_click$id); rv$cita_id <- input$cal_click$id
      cita <- DBI::dbGetQuery(pool, "SELECT * FROM citas WHERE id=?", params = list(rv$cita_id))
      if(nrow(cita)==0) return()
      abrir_form("edit")
      if(as.POSIXct(cita$fecha_fin) >= Sys.time()){
        updateSelectInput(session, "paciente", selected = cita$paciente_id)
        updateSelectInput(session, "doctor", selected = cita$profesional_id)
        updateNumericInput(session, "gabinete", value = cita$gabinete)
        updateTextInput(session, "servicio", value = cita$tipo_servicio)
        updateDateInput(session, "fecha_cita", value = as.Date(cita$fecha_inicio))
        dur <- as.numeric(difftime(as.POSIXct(cita$fecha_fin), as.POSIXct(cita$fecha_inicio), units="mins"))
        updateNumericInput(session, "duracion", value = dur)
      }
    })
    
    observeEvent(input$btn_cancelar_inicio, { shinyjs::hide("capa_principal"); shinyjs::hide("footer_normal"); shinyjs::show("capa_cancelacion") })
    observeEvent(input$btn_confirmar_no, { shinyjs::hide("capa_cancelacion"); shinyjs::show("capa_principal"); shinyjs::show("footer_normal") })
    observeEvent(input$btn_confirmar_si, { DBI::dbExecute(pool, "UPDATE citas SET estado = 'cancelada' WHERE id = ?", params = list(rv$cita_id)); limpiar_todo(); refresh(refresh() + 1) })
  })
}