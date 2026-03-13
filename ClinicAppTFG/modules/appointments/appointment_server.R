appointmentServer <- function(id, pool, current_user){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    refresh <- reactiveVal(0)
    rv <- reactiveValues(cita_id = NULL, start = NULL, end = NULL)
    
    # PERMISOS
    puede_gestionar <- reactive({
      req(current_user())
      current_user()$tipo_usuario %in% c("admin","recepcion")
    })
    
    # FILTRO DOCTOR
    output$ui_filtro_doctor <- renderUI({
      docs <- DBI::dbGetQuery(pool, "SELECT id,nombre FROM usuarios WHERE tipo_usuario='doctor'")
      pickerInput(ns("filtro_doctor"), "Doctor",
                  choices = c("Todos"="0", setNames(docs$id, docs$nombre)),
                  selected="0")
    })
    
    # --- CALENDARIO (Configuración para evitar el arrastre y forzar el click) ---
    output$cal <- renderCalendar({
      refresh()
      
      df <- DBI::dbGetQuery(pool, "SELECT c.*, p.nombre paciente, d.nombre doctor FROM citas c 
                                   JOIN usuarios p ON c.paciente_id = p.id 
                                   JOIN usuarios d ON c.profesional_id = d.id")
      
      cal_df <- if(nrow(df) == 0) data.frame() else {
        data.frame(
          id = as.character(df$id),
          calendarId = as.character(df$gabinete),
          title = paste(df$paciente, "-", df$tipo_servicio),
          start = df$fecha_inicio,
          end = df$fecha_fin,
          category = "time",
          backgroundColor = df$color,
          borderColor = df$color
        )
      }
      
      calendar(cal_df, 
               navigation = TRUE, 
               useCreationPopup = FALSE, 
               useDetailPopup = FALSE,
               isReadOnly = TRUE # <-- ESTO QUITA LAS FLECHAS Y EL ARRASTRE
      ) %>%
        cal_events(
          clickSchedule = JS(sprintf("function(e){
      var id = e.schedule ? e.schedule.id : e.event.id;
      console.log('ID clicado:', id);
      Shiny.setInputValue('%s', id, {priority:'event'});
  }", ns("editar_click")))
        )
    })
    
    # --- FORMULARIO MODAL ---
    abrir_form <- function(editar=FALSE, datos=NULL){
      pac <- DBI::dbGetQuery(pool, "SELECT id,nombre FROM usuarios WHERE tipo_usuario='paciente'")
      doc <- DBI::dbGetQuery(pool, "SELECT id,nombre FROM usuarios WHERE tipo_usuario='doctor'")
      
      showModal(modalDialog(
        title = if(editar) "Modificar Cita" else "Nueva Cita", 
        size = "m",
        div(class = "row",
            div(class = "col-md-6", selectInput(ns("paciente"), "Paciente", choices=setNames(pac$id,pac$nombre), selected = if(editar) datos$paciente_id)),
            div(class = "col-md-6", selectInput(ns("doctor"), "Doctor", choices=setNames(doc$id,doc$nombre), selected = if(editar) datos$profesional_id))
        ),
        div(class = "row",
            div(class = "col-md-6", numericInput(ns("gabinete"), "Gabinete", value = if(editar) datos$gabinete else 1, min=1, max=3)),
            div(class = "col-md-6", textInput(ns("servicio"), "Servicio", value = if(editar) datos$tipo_servicio else "Revisión"))
        ),
        hr(),
        airDatepickerInput(ns("fecha_cita"), "Fecha y Hora de Inicio", value = rv$start, 
                           timepicker = TRUE, timepickerOpts = timepickerOptions(minutesStep = 15),
                           dateFormat = "yyyy-mm-dd", width = "100%"),
        numericInput(ns("duracion"), "Duración (minutos)", 
                     value = if(editar) as.numeric(difftime(rv$end, rv$start, units="mins")) else 60, step = 15),
        footer = tagList(
          if(editar) actionButton(ns("borrar"), "Eliminar", class="btn-danger", style="float:left"),
          modalButton("Cancelar"),
          actionButton(ns("guardar"), "Guardar", class="btn-success")
        )
      ))
    }
    
    # --- OBSERVADORES ---
    
    # Al pulsar botón "Nueva cita"
    observeEvent(input$crear_manual,{
      rv$cita_id <- NULL
      rv$start <- Sys.time()
      rv$end <- Sys.time() + 3600
      abrir_form(FALSE)
    })
    
    # AL CLICAR UNA CITA (Captura el evento del JS)
    observeEvent(input$editar_click, {
      id_cita <- as.numeric(input$editar_click)
      req(id_cita)
      
      datos <- DBI::dbGetQuery(pool, "SELECT * FROM citas WHERE id=?", params=list(id_cita))
      if(nrow(datos) == 0) return()
      
      rv$cita_id <- id_cita
      rv$start <- as.POSIXct(datos$fecha_inicio)
      rv$end <- as.POSIXct(datos$fecha_fin)
      abrir_form(TRUE, datos)
    })
    
    # GUARDAR
    observeEvent(input$guardar,{
      
      t_start_dt <- as.POSIXct(input$fecha_cita)
      t_end_dt <- t_start_dt + (input$duracion * 60)
      
      t_start <- format(t_start_dt, "%Y-%m-%d %H:%M:%S")
      t_end <- format(t_end_dt, "%Y-%m-%d %H:%M:%S")
      
      cols <- c("1"="#7e57c2", "2"="#26a69a", "3"="#ffa726")
      color_hex <- cols[as.character(input$gabinete)]
      
      if(is.null(rv$cita_id)){
        
        DBI::dbExecute(pool,
                       "INSERT INTO citas (paciente_id, profesional_id, gabinete, fecha_inicio, fecha_fin, tipo_servicio, color)
       VALUES (?,?,?,?,?,?,?)",
                       params=list(input$paciente, input$doctor, input$gabinete, t_start, t_end, input$servicio, color_hex)
        )
        
      } else {
        
        DBI::dbExecute(pool,
                       "UPDATE citas
       SET paciente_id=?, profesional_id=?, gabinete=?, fecha_inicio=?, fecha_fin=?, tipo_servicio=?, color=?
       WHERE id=?",
                       params=list(input$paciente, input$doctor, input$gabinete, t_start, t_end, input$servicio, color_hex, rv$cita_id)
        )
        
      }
      
      removeModal(session = session)
      refresh(refresh() + 1)
      
    })
    
    # BORRAR
    observeEvent(input$borrar, {
      
      req(rv$cita_id)
      
      DBI::dbExecute(pool,
                     "DELETE FROM citas WHERE id=?",
                     params=list(rv$cita_id)
      )
      
      rv$cita_id <- NULL
      
      removeModal(session = session)
      refresh(refresh() + 1)
      
    })  })
}