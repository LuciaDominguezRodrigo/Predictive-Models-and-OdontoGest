certificateServer <- function(id, pool, current_user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Datos
    citas_pasadas <- reactive({
      req(current_user())
      
      DBI::dbGetQuery(pool, "
        SELECT c.id, c.fecha_inicio, c.tipo_servicio, u.nombre as doctor
        FROM citas c
        JOIN usuarios u ON c.profesional_id = u.id
        WHERE c.paciente_id = ? 
          AND c.fecha_fin < NOW() 
          AND c.estado != 'cancelada'
        ORDER BY c.fecha_inicio DESC
      ", params = list(current_user()$id))
    })
    
    output$selector_cita <- renderUI({
      df <- citas_pasadas()
      
      if (nrow(df) == 0) {
        return(p("No hay citas disponibles"))
      }
      
      choices <- setNames(
        df$id,
        paste0(
          format(as.POSIXct(df$fecha_inicio), "%d/%m/%Y"),
          " - ",
          df$tipo_servicio,
          " (", df$doctor, ")"
        )
      )
      
      selectInput(ns("cita_id"), "Selecciona una cita:", choices)
    })
    
    # 3. ÚNICO downloadHandler (sin líos)
    output$download_pdf <- downloadHandler(
      
      filename = function() {
        paste0("Justificante_", Sys.Date(), ".pdf")
      },
      
      content = function(file) {
        df <- citas_pasadas()
        req(input$cita_id)
        
        cita <- df[df$id == input$cita_id, ]
        
        # 🔒 valores simples
        fecha <- format(as.POSIXct(cita$fecha_inicio), "%d/%m/%Y %H:%M")
        servicio <- as.character(cita$tipo_servicio)
        doctor <- as.character(cita$doctor)
        paciente <- as.character(current_user()$nombre)
        
        # 📄 HTML directo (sin Rmd)
        html <- htmltools::tags$html(
          htmltools::tags$head(
            htmltools::tags$style("
              body { font-family: Arial; padding: 40px; }
              h2 { text-align: center; }
            ")
          ),
          htmltools::tags$body(
            htmltools::tags$h2("JUSTIFICANTE MÉDICO"),
            htmltools::tags$hr(),
            
            htmltools::tags$p(strong("Paciente: "), paciente),
            htmltools::tags$p(strong("Fecha: "), fecha),
            htmltools::tags$p(strong("Servicio: "), servicio),
            htmltools::tags$p(strong("Doctor/a: "), doctor),
            
            htmltools::tags$br(),
            htmltools::tags$p(
              "Se confirma la asistencia del paciente a nuestra clínica."
            ),
            
            htmltools::tags$br(),
            htmltools::tags$br(),
            htmltools::tags$p("Sello de la clínica")
          )
        )
        
        # Guardar HTML temporal
        temp_html <- tempfile(fileext = ".html")
        htmltools::save_html(html, temp_html)
        
        # Convertir a PDF
        webshot2::webshot(temp_html, file)
      }
    )
    
  })
}