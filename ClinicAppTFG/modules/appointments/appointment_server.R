appointmentServer <- function(id, pool, current_user){
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    refresh <- reactiveVal(0)
    
    rv <- reactiveValues(
      cita_id = NULL,
      edit_mode = FALSE
    )
    
    # --- FUNCIÓN DE LIMPIEZA TOTAL ---
    limpiar_todo <- function() {
      removeModal()
      shinyjs::runjs("$('body').removeClass('modal-open'); $('.modal-backdrop').remove();")
    }
    
    # ---------------- FILTRO DOCTOR ----------------
    output$ui_filtro_doctor <- renderUI({
      docs <- DBI::dbGetQuery(pool, "SELECT id, nombre FROM usuarios WHERE tipo_usuario IN ('doctor','higienista')")
      shinyWidgets::pickerInput(ns("filtro_doctor"), "Doctor", choices = c("Todos"="0", setNames(docs$id, docs$nombre)), selected = "0")
    })
    
    # ---------------- CALENDARIO ----------------
    output$cal <- renderCalendar({
      refresh()
      df <- DBI::dbGetQuery(pool, "SELECT c.*, p.nombre paciente, d.nombre doctor FROM citas c 
                                   JOIN usuarios p ON c.paciente_id = p.id JOIN usuarios d ON c.profesional_id = d.id
                                   WHERE c.estado != 'cancelada'")
      if(nrow(df) > 0){
        df$fecha_inicio <- as.POSIXct(df$fecha_inicio, tz="Europe/Madrid")
        df$fecha_fin   <- as.POSIXct(df$fecha_fin, tz="Europe/Madrid")
        cal_df <- data.frame(id = as.character(df$id), calendarId = as.character(df$gabinete),
                             title = paste(df$paciente,"-",df$tipo_servicio), start = df$fecha_inicio,
                             end = df$fecha_fin, category = "time", backgroundColor = df$color, borderColor = df$color)
      } else { cal_df <- data.frame() }
      calendar(cal_df, navigation = TRUE, useCreationPopup = FALSE, useDetailPopup = FALSE, isReadOnly = TRUE)
    })
    
    # ---------------- FORMULARIO DINÁMICO ----------------
    abrir_form <- function(mode = "new"){
      rv$edit_mode <- (mode == "edit")
      pac <- DBI::dbGetQuery(pool, "SELECT id,nombre FROM usuarios WHERE tipo_usuario='paciente'")
      doc <- DBI::dbGetQuery(pool, "SELECT id,nombre FROM usuarios WHERE tipo_usuario IN ('doctor','higienista')")
      
      showModal(modalDialog(
        title = if(rv$edit_mode) "Editar Cita" else "Nueva Cita",
        size = "m",
        
        # CAPA 1: FORMULARIO (Siempre visible al abrir)
        div(id = ns("capa_formulario"),
            selectInput(ns("paciente"), "Paciente", choices=setNames(pac$id,pac$nombre)),
            selectInput(ns("doctor"), "Profesional", choices=setNames(doc$id,doc$nombre)),
            numericInput(ns("gabinete"), "Gabinete", value=1, min=1, max=3),
            textInput(ns("servicio"), "Servicio", value="Revisión"),
            dateInput(ns("fecha_cita"), "Fecha", value=Sys.Date()),
            uiOutput(ns("horas_disponibles")),
            numericInput(ns("duracion"), "Duración (min)", value=60, step=15)
        ),
        
        # CAPA 2: AVISO CANCELACIÓN (Oculto por defecto)
        shinyjs::hidden(
          div(id = ns("capa_cancelacion"), class="alert alert-danger",
              tags$h4("¿Confirmar cancelación?"),
              p("Esta cita dejará de ser visible en el calendario.")
          )
        ),
        
        footer = tagList(
          # Botones modo Edición
          shinyjs::hidden(actionButton(ns("btn_cancelar_inicio"), "Cancelar Cita", class="btn-danger", style="float:left;")),
          # Botones modo Confirmar Borrado
          shinyjs::hidden(actionButton(ns("btn_confirmar_si"), "Sí, borrar permanentemente", class="btn-danger")),
          shinyjs::hidden(actionButton(ns("btn_confirmar_no"), "No, volver", class="btn-default")),
          actionButton(ns("guardar"), "Guardar", class="btn-success")
        )
      ))
      
      # Lógica de visibilidad inicial de botones según el modo
      if(rv$edit_mode) shinyjs::show("btn_cancelar_inicio")
    }
    
    # ---------------- EVENTOS DE CANCELACIÓN (SIN PARPADEO) ----------------
    observeEvent(input$btn_cancelar_inicio, {
      shinyjs::hide("capa_formulario")
      shinyjs::hide("btn_cancelar_inicio")
      shinyjs::hide("guardar")
      
      shinyjs::show("capa_cancelacion")
      shinyjs::show("btn_confirmar_si")
      shinyjs::show("btn_confirmar_no")
    })
    
    observeEvent(input$btn_confirmar_no, {
      shinyjs::hide("capa_cancelacion")
      shinyjs::hide("btn_confirmar_si")
      shinyjs::hide("btn_confirmar_no")
      
      shinyjs::show("capa_formulario")
      shinyjs::show("btn_cancelar_inicio")
      shinyjs::show("guardar")
    })
    
    observeEvent(input$btn_confirmar_si, {
      DBI::dbExecute(pool, "UPDATE citas SET estado = 'cancelada' WHERE id = ?", params = list(rv$cita_id))
      limpiar_todo()
      refresh(refresh() + 1)
      showNotification("Cita cancelada", type = "warning")
    })
    
    # ---------------- RESTO DE EVENTOS (IGUAL) ----------------
    observeEvent(input$crear_manual, { rv$cita_id <- NULL; abrir_form("new") })
    
    observeEvent(input$cal_click, {
      info <- input$cal_click
      req(info$id); rv$cita_id <- info$id
      cita <- DBI::dbGetQuery(pool, "SELECT * FROM citas WHERE id=?", params = list(rv$cita_id))
      if(nrow(cita)==0) return()
      abrir_form("edit")
      updateSelectInput(session, "paciente", selected = cita$paciente_id)
      updateSelectInput(session, "doctor", selected = cita$profesional_id)
      updateNumericInput(session, "gabinete", value = cita$gabinete)
      updateTextInput(session, "servicio", value = cita$tipo_servicio)
      updateDateInput(session, "fecha_cita", value = as.Date(cita$fecha_inicio))
    })
    
    observeEvent(input$guardar, {
      req(input$fecha_cita, input$hora_inicio)
      hora <- substr(input$hora_inicio, 1, 5)
      t_start <- as.POSIXct(paste(input$fecha_cita, hora), tz="Europe/Madrid")
      t_end <- t_start + (input$duracion * 60)
      color_hex <- switch(as.character(input$gabinete), "1"="#7e57c2", "2"="#26a69a", "3"="#ffa726", "#26a69a")
      
      if(rv$edit_mode){
        DBI::dbExecute(pool, "UPDATE citas SET paciente_id=?, profesional_id=?, gabinete=?, fecha_inicio=?, fecha_fin=?, tipo_servicio=?, color=? WHERE id=?",
                       params = list(input$paciente, input$doctor, input$gabinete, format(t_start,"%Y-%m-%d %H:%M:%S"), format(t_end,"%Y-%m-%d %H:%M:%S"), input$servicio, color_hex, rv$cita_id))
      } else {
        DBI::dbExecute(pool, "INSERT INTO citas (paciente_id, profesional_id, gabinete, fecha_inicio, fecha_fin, tipo_servicio, color, estado) VALUES (?,?,?,?,?,?,?, 'programada')",
                       params = list(input$paciente, input$doctor, input$gabinete, format(t_start,"%Y-%m-%d %H:%M:%S"), format(t_end,"%Y-%m-%d %H:%M:%S"), input$servicio, color_hex))
      }
      limpiar_todo()
      refresh(refresh()+1)
    })
    
    # Horas disponibles (UI)
    output$horas_disponibles <- renderUI({
      req(input$fecha_cita)
      fecha <- as.Date(input$fecha_cita)
      inicio_dia <- as.POSIXct(paste(fecha,"09:00:00"), tz="Europe/Madrid")
      fin_dia    <- as.POSIXct(paste(fecha,"20:00:00"), tz="Europe/Madrid")
      slots <- seq(inicio_dia, fin_dia, by="15 min")
      slots_df <- data.frame(hora = format(slots,"%H:%M"), inicio = slots, ocupado = FALSE)
      
      query <- "SELECT c.fecha_inicio, c.fecha_fin FROM citas c WHERE DATE(c.fecha_inicio)=? AND c.estado != 'cancelada'"
      params <- list(fecha)
      if(rv$edit_mode) { query <- paste(query, "AND c.id != ?"); params <- list(fecha, rv$cita_id) }
      citas <- DBI::dbGetQuery(pool, query, params = params)
      
      if(nrow(citas) > 0){
        for(i in 1:nrow(citas)){
          slots_df$ocupado[slots_df$inicio >= as.POSIXct(citas$fecha_inicio[i]) & slots_df$inicio < as.POSIXct(citas$fecha_fin[i])] <- TRUE
        }
      }
      selectInput(ns("hora_inicio"), "Hora disponible", choices = slots_df$hora[!slots_df$ocupado])
    })
  })
}