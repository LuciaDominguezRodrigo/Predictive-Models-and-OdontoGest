appointmentServer <- function(id, pool, current_user){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    refresh <- reactiveVal(0)
    # rv guarda objetos POSIXct limpios para evitar errores de formato
    rv <- reactiveValues(cita_id = NULL, start = NULL, end = NULL)
    
    # ---------------- PERMISOS ----------------
    puede_gestionar <- reactive({
      req(current_user())
      current_user()$tipo_usuario %in% c("admin","recepcion")
    })
    
    # ---------------- FILTRO DOCTOR ----------------
    output$ui_filtro_doctor <- renderUI({
      docs <- DBI::dbGetQuery(pool, 
    "SELECT id, nombre FROM usuarios WHERE tipo_usuario = 'doctor' OR tipo_usuario = 'higienista'"
)
      
      pickerInput(
        ns("filtro_doctor"), "Doctor",
        choices = c("Todos"="0", setNames(docs$id, docs$nombre)),
        selected="0"
      )
    })
    
    # ---------------- DETECTAR CONFLICTOS (Lógica Corregida) ----------------
    hay_conflicto <- function(pool, doctor, gabinete, inicio, fin, cita_id=NULL){
      
      # Convertimos a string para la base de datos
      inicio_str <- format(inicio, "%Y-%m-%d %H:%M:%S")
      fin_str    <- format(fin, "%Y-%m-%d %H:%M:%S")
      
      # Lógica: Hay solapamiento si la nueva empieza antes de que termine una existente 
      # Y termina después de que empiece esa misma existente.
      query <- "
      SELECT c.*, u.nombre as doctor_nom
      FROM citas c
      JOIN usuarios u ON u.id = c.profesional_id
      WHERE (c.profesional_id = ? OR c.gabinete = ?)
      AND (? < c.fecha_fin AND ? > c.fecha_inicio)"
      
      params <- list(doctor, gabinete, inicio_str, fin_str)
      
      if(!is.null(cita_id)){
        query <- paste(query, "AND c.id != ?")
        params <- c(params, cita_id)
      }
      
      res <- DBI::dbGetQuery(pool, query, params=params)
      if(nrow(res) > 0) return(res)
      return(NULL)
    }
    
    # ---------------- CALENDARIO ----------------
    # ---------------- CALENDARIO CON FILTROS ACTIVOS ----------------
    # ---------------- CALENDARIO (CORREGIDO) ----------------
    output$cal <- renderCalendar({
      refresh() # Dependencia reactiva para refrescar al guardar/borrar
      
      # 1. Empezamos con la query limpia
      query_base <- "
        SELECT c.*, p.nombre paciente, d.nombre doctor
        FROM citas c
        JOIN usuarios p ON c.paciente_id=p.id
        JOIN usuarios d ON c.profesional_id=d.id
        WHERE 1=1"
      
      # 2. Creamos una lista vacía para los parámetros
      params <- list()
      
      # 3. Filtro de Gabinete: SOLO añadimos el ? si el valor NO es "0"
      if (!is.null(input$filtro_gabinete) && input$filtro_gabinete != "0") {
        query_base <- paste(query_base, "AND c.gabinete = ?")
        params <- c(params, input$filtro_gabinete)
      }
      
      # 4. Filtro de Profesional: SOLO añadimos el ? si el valor NO es "0"
      if (!is.null(input$filtro_doctor) && input$filtro_doctor != "0") {
        query_base <- paste(query_base, "AND d.id = ?")
        params <- c(params, input$filtro_doctor)
      }
      
      # 5. EJECUCIÓN SEGURA
      # Si params está vacío, pasamos NULL o simplemente no lo ponemos
      df <- if (length(params) > 0) {
        DBI::dbGetQuery(pool, query_base, params = params)
      } else {
        DBI::dbGetQuery(pool, query_base) # Query sin parámetros (SELECT de todo)
      }
      
      # 6. Transformación de datos para el widget
      cal_df <- if(nrow(df) == 0) {
        data.frame() 
      } else {
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
      
      # 7. Renderizado del componente
      calendar(
        cal_df,
        navigation = TRUE,
        useCreationPopup = FALSE,
        useDetailPopup = FALSE,
        isReadOnly = TRUE
      ) %>%
        cal_events(
          clickSchedule = JS(sprintf("
            function(e){
              var id = e.event ? e.event.id : e.schedule.id;
              Shiny.setInputValue('%s', id, {priority:'event'});
            }", ns("editar_click"))
          )
        )
    })    # ---------------- FORMULARIO (Responsive) ----------------
    abrir_form <- function(editar=FALSE, datos=NULL){
      
      pac <- DBI::dbGetQuery(pool, "SELECT id, nombre FROM usuarios WHERE tipo_usuario='paciente'")
      doc <- DBI::dbGetQuery(pool, "
          SELECT id, nombre 
          FROM usuarios 
          WHERE tipo_usuario IN ('doctor', 'higienista')")
      
      showModal(
        modalDialog(
          title = if(editar) "Modificar Cita" else "Nueva Cita",
          size = "m",
          
          div(class="container-fluid",
              div(class="row",
                  div(class="col-12 col-sm-6",
                      selectInput(ns("paciente"), "Paciente", choices=setNames(pac$id, pac$nombre), selected=if(editar) datos$paciente_id)
                  ),
                  div(class="col-12 col-sm-6",
                      selectInput(ns("doctor"), "Profesional", choices=setNames(doc$id, doc$nombre), selected=if(editar) datos$profesional_id)
                  )
              ),
              div(class="row",
                  div(class="col-12 col-sm-6",
                      numericInput(ns("gabinete"), "Gabinete", value=if(editar) datos$gabinete else 1, min=1, max=3)
                  ),
                  div(class="col-12 col-sm-6",
                      textInput(ns("servicio"), "Servicio", value=if(editar) datos$tipo_servicio else "Revisión")
                  )
              ),
              hr(),
              airDatepickerInput(
                ns("fecha_cita"),
                "Fecha y Hora de Inicio",
                value = rv$start,
                timepicker = TRUE,
                timepickerOpts = timepickerOptions(minutesStep=15),
                dateFormat = "yyyy-mm-dd",
                width = "100%",
                autoClose = TRUE
              ),
              numericInput(ns("duracion"), "Duración (min)", value=if(editar) as.numeric(difftime(rv$end, rv$start, units="mins")) else 60, step=15)
          ),
          
          footer = tagList(
            if(editar) actionButton(ns("borrar"), "Eliminar", class="btn-danger", style="float:left"),
            modalButton("Cancelar"),
            actionButton(ns("guardar"), "Guardar", class="btn-success")
          )
        )
      )
    }
    
    # ---------------- EVENTOS (NUEVA / EDITAR) ----------------
    observeEvent(input$crear_manual, {
      rv$cita_id <- NULL
      rv$start <- as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:00:00"), tz="Europe/Madrid")
      rv$end <- rv$start + 3600
      abrir_form(FALSE)
    })
    
    observeEvent(input$editar_click, {
      id_cita <- as.numeric(input$editar_click)
      req(id_cita)
      
      datos <- DBI::dbGetQuery(pool, "SELECT * FROM citas WHERE id=?", params=list(id_cita))
      if(nrow(datos) == 0) return()
      
      rv$cita_id <- id_cita
      rv$start   <- as.POSIXct(datos$fecha_inicio, tz="Europe/Madrid")
      rv$end     <- as.POSIXct(datos$fecha_fin, tz="Europe/Madrid")
      
      abrir_form(TRUE, datos)
    })
    
    # ---------------- GUARDAR ----------------
    observeEvent(input$guardar, {
      req(input$fecha_cita)
      
      # Forzamos la zona horaria al leer del input
      t_start_dt <- as.POSIXct(input$fecha_cita, tz="Europe/Madrid")
      t_end_dt   <- t_start_dt + (as.numeric(input$duracion) * 60)
      
      # Validar conflictos
      conflictos <- hay_conflicto(pool, input$doctor, input$gabinete, t_start_dt, t_end_dt, rv$cita_id)
      
      if(!is.null(conflictos)){
        showNotification("⚠️ Error: El doctor o el gabinete ya tienen una cita en este horario.", type="error")
        return()
      }
      
      # Formateo para SQL
      t_start_sql <- format(t_start_dt, "%Y-%m-%d %H:%M:%S")
      t_end_sql   <- format(t_end_dt, "%Y-%m-%d %H:%M:%S")
      
      cols <- c("1"="#7e57c2", "2"="#26a69a", "3"="#ffa726")
      color_hex <- cols[as.character(input$gabinete)]
      
      if(is.null(rv$cita_id)){
        DBI::dbExecute(pool,
                       "INSERT INTO citas (paciente_id, profesional_id, gabinete, fecha_inicio, fecha_fin, tipo_servicio, color) VALUES (?,?,?,?,?,?,?)",
                       params=list(input$paciente, input$doctor, input$gabinete, t_start_sql, t_end_sql, input$servicio, color_hex))
      } else {
        DBI::dbExecute(pool,
                       "UPDATE citas SET paciente_id=?, profesional_id=?, gabinete=?, fecha_inicio=?, fecha_fin=?, tipo_servicio=?, color=? WHERE id=?",
                       params=list(input$paciente, input$doctor, input$gabinete, t_start_sql, t_end_sql, input$servicio, color_hex, rv$cita_id))
      }
      
      removeModal()
      refresh(refresh() + 1)
      showNotification("Cita procesada con éxito", type="message")
    })
    
    # ---------------- BORRAR ----------------
    observeEvent(input$borrar, {
      req(rv$cita_id)
      DBI::dbExecute(pool, "DELETE FROM citas WHERE id=?", params=list(rv$cita_id))
      rv$cita_id <- NULL
      removeModal()
      refresh(refresh() + 1)
    })
    
  })
}