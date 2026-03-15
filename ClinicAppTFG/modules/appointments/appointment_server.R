appointmentServer <- function(id, pool, current_user){
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    refresh <- reactiveVal(0)
    
    rv <- reactiveValues(
      cita_id = NULL
    )
    
    # ---------------- FILTRO DOCTOR ----------------
    output$ui_filtro_doctor <- renderUI({
      
      docs <- DBI::dbGetQuery(
        pool,
        "SELECT id, nombre FROM usuarios WHERE tipo_usuario IN ('doctor','higienista')"
      )
      
      shinyWidgets::pickerInput(
        ns("filtro_doctor"),
        "Doctor",
        choices = c("Todos"="0", setNames(docs$id, docs$nombre)),
        selected = "0"
      )
    })
    
    
    # ---------------- CALENDARIO ----------------
    output$cal <- renderCalendar({
      
      refresh()
      
      df <- DBI::dbGetQuery(
        pool,
        "
        SELECT c.*, p.nombre paciente, d.nombre doctor
        FROM citas c
        JOIN usuarios p ON c.paciente_id = p.id
        JOIN usuarios d ON c.profesional_id = d.id
        "
      )
      
      if(nrow(df) > 0){
        
        df$fecha_inicio <- as.POSIXct(df$fecha_inicio, tz="Europe/Madrid")
        df$fecha_fin   <- as.POSIXct(df$fecha_fin, tz="Europe/Madrid")
        
        cal_df <- data.frame(
          id = as.character(df$id),
          calendarId = as.character(df$gabinete),
          title = paste(df$paciente,"-",df$tipo_servicio),
          start = df$fecha_inicio,
          end = df$fecha_fin,
          category = "time",
          backgroundColor = df$color,
          borderColor = df$color
        )
        
      } else {
        
        cal_df <- data.frame()
        
      }
      
      calendar(
        cal_df,
        navigation = TRUE,
        useCreationPopup = FALSE,
        useDetailPopup = FALSE,
        isReadOnly = TRUE
      )
      
    })
    
    
    # ---------------- HORAS DISPONIBLES ----------------
    output$horas_disponibles <- renderUI({
      
      req(input$fecha_cita)
      
      fecha <- as.Date(input$fecha_cita)
      
      inicio_dia <- as.POSIXct(paste(fecha,"09:00:00"), tz="Europe/Madrid")
      fin_dia    <- as.POSIXct(paste(fecha,"20:00:00"), tz="Europe/Madrid")
      
      slots <- seq(inicio_dia, fin_dia, by="15 min")
      
      slots_df <- data.frame(
        hora = format(slots,"%H:%M"),
        inicio = slots,
        ocupado = FALSE,
        stringsAsFactors = FALSE
      )
      
      citas <- DBI::dbGetQuery(
        pool,
        "
        SELECT c.fecha_inicio, c.fecha_fin, u.nombre paciente
        FROM citas c
        JOIN usuarios u ON c.paciente_id=u.id
        WHERE DATE(c.fecha_inicio)=?
        ",
        params=list(fecha)
      )
      
      citas_texto <- c()
      
      if(nrow(citas) > 0){
        
        citas$fecha_inicio <- as.POSIXct(citas$fecha_inicio, tz="Europe/Madrid")
        citas$fecha_fin    <- as.POSIXct(citas$fecha_fin, tz="Europe/Madrid")
        
        for(i in seq_len(nrow(citas))){
          
          slots_df$ocupado[
            slots_df$inicio >= citas$fecha_inicio[i] &
              slots_df$inicio <  citas$fecha_fin[i]
          ] <- TRUE
          
          dur <- as.numeric(
            difftime(citas$fecha_fin[i], citas$fecha_inicio[i], units="mins")
          )
          
          citas_texto <- c(
            citas_texto,
            paste0(
              format(citas$fecha_inicio[i],"%H:%M"),
              " - ",
              citas$paciente[i],
              " (", dur, " min)"
            )
          )
        }
      }
      
      slots_df$label <- ifelse(
        slots_df$ocupado,
        paste0(slots_df$hora," ❌ Ocupado"),
        paste0(slots_df$hora," ✔ Libre")
      )
      
      tagList(
        
        selectInput(
          ns("hora_inicio"),
          "Hora disponible",
          choices = slots_df$label[!slots_df$ocupado],
          selected = NULL
        ),
        
        tags$hr(),
        tags$strong("Citas ocupadas ese día"),
        
        tags$ul(
          lapply(citas_texto, function(x) tags$li(x))
        )
      )
      
    })
    
    
    # ---------------- FORMULARIO ----------------
    abrir_form <- function(){
      
      pac <- DBI::dbGetQuery(
        pool,
        "SELECT id,nombre FROM usuarios WHERE tipo_usuario='paciente'"
      )
      
      doc <- DBI::dbGetQuery(
        pool,
        "SELECT id,nombre FROM usuarios WHERE tipo_usuario IN ('doctor','higienista')"
      )
      
      showModal(
        
        modalDialog(
          
          title = "Nueva cita",
          size="m",
          
          selectInput(
            ns("paciente"),
            "Paciente",
            choices=setNames(pac$id,pac$nombre)
          ),
          
          selectInput(
            ns("doctor"),
            "Profesional",
            choices=setNames(doc$id,doc$nombre)
          ),
          
          numericInput(
            ns("gabinete"),
            "Gabinete",
            value=1,
            min=1,
            max=3
          ),
          
          textInput(
            ns("servicio"),
            "Servicio",
            value="Revisión"
          ),
          
          dateInput(
            ns("fecha_cita"),
            "Fecha",
            value=Sys.Date()
          ),
          
          uiOutput(ns("horas_disponibles")),
          
          numericInput(
            ns("duracion"),
            "Duración (min)",
            value=60,
            step=15
          ),
          
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(
              ns("guardar"),
              "Guardar",
              class="btn-success"
            )
          )
        )
      )
    }
    
    
    # ---------------- NUEVA CITA ----------------
    observeEvent(input$crear_manual,{
      abrir_form()
    })
    
    observeEvent(input$cal_click, {
      
      info <- input$cal_click
      
      rv$cita_id <- info$id
      
      cita <- DBI::dbGetQuery(
        pool,
        "SELECT * FROM citas WHERE id=?",
        params = list(rv$cita_id)
      )
      
      if(nrow(cita)==0) return()
      
      abrir_form()
      
      updateSelectInput(session,"paciente",selected=cita$paciente_id)
      updateSelectInput(session,"doctor",selected=cita$profesional_id)
      updateNumericInput(session,"gabinete",value=cita$gabinete)
      updateTextInput(session,"servicio",value=cita$tipo_servicio)
      updateDateInput(session,"fecha_cita",value=as.Date(cita$fecha_inicio))
    })
    
    # ---------------- GUARDAR ----------------
    observeEvent(input$guardar,{
      
      req(input$fecha_cita,input$hora_inicio)
      
      hora <- substr(input$hora_inicio,1,5)
      
      t_start <- as.POSIXct(
        paste(input$fecha_cita,hora),
        tz="Europe/Madrid"
      )
      
      t_end <- t_start + (input$duracion*60)
      
      color_hex <- "#26a69a"
      
      DBI::dbExecute(
        pool,
        "
        INSERT INTO citas
        (paciente_id,profesional_id,gabinete,fecha_inicio,fecha_fin,tipo_servicio,color)
        VALUES (?,?,?,?,?,?,?)
        ",
        params=list(
          input$paciente,
          input$doctor,
          input$gabinete,
          format(t_start,"%Y-%m-%d %H:%M:%S"),
          format(t_end,"%Y-%m-%d %H:%M:%S"),
          input$servicio,
          color_hex
        )
      )
      
      removeModal()
      refresh(refresh()+1)
      
      showNotification("Cita guardada correctamente")
      
    })
    
  })
  
}