library(shiny)
library(DBI)
library(htmltools)
library(webshot2)

certificateServer <- function(id, pool, current_user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Cargar solo citas en estado 'completada'
    citas_completadas <- reactive({
      req(current_user())
      DBI::dbGetQuery(pool, "
        SELECT c.id, c.fecha_inicio, c.tipo_servicio, u.nombre as doctor
        FROM citas c
        JOIN usuarios u ON c.profesional_id = u.id
        WHERE c.paciente_id = ? 
          AND c.estado = 'completada'
        ORDER BY c.fecha_inicio DESC
      ", params = list(current_user()$id))
    })
    
    # 2. Renderizar el selector y el botón dinámicamente
    output$ui_selector <- renderUI({
      df <- citas_completadas()
      
      if (nrow(df) == 0) {
        return(
          div(class = "alert alert-light border-0 shadow-sm p-3 text-center",
              style = "border-left: 5px solid #6a1b9a !important;",
              icon("info-circle", class = "text-purple-corp"),
              span(class = "ms-2", "No tenemos el justificante todavía. Estará disponible cuando el médico añada las anotaciones y complete la cita.")
          )
        )
      }
      
      choices <- setNames(df$id, 
                          paste0(format(as.POSIXct(df$fecha_inicio), "%d/%m/%Y"), 
                                 " - ", df$tipo_servicio))
      
      tagList(
        selectInput(ns("cita_seleccionada"), "Selecciona la cita finalizada:", choices = choices),
        downloadButton(ns("download_pdf"), "Descargar Justificante PDF", class = "btn-download")
      )
    })
    
    # 3. Handler de descarga con bloqueo de seguridad
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("Justificante_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        # Bloquear UI para evitar clics dobles mientras webshot trabaja
        shinyjs::disable("download_pdf")
        on.exit(shinyjs::enable("download_pdf"))
        
        req(input$cita_seleccionada)
        
        # Obtener datos específicos de la cita
        df <- citas_completadas()
        cita <- df[df$id == input$cita_seleccionada, ]
        
        # Generar contenido HTML con estilo para el PDF
        html_doc <- htmltools::tags$html(
          htmltools::tags$body(
            style = "font-family: Arial, sans-serif; padding: 50px; color: #333;",
            htmltools::tags$div(
              style = "text-align: center; margin-bottom: 30px;",
              htmltools::tags$h1("JUSTIFICANTE MÉDICO", style = "color: #6a1b9a;"),
              htmltools::tags$hr()
            ),
            htmltools::tags$div(
              style = "line-height: 2; font-size: 16px;",
              htmltools::tags$p(strong("PACIENTE: "), current_user()$nombre),
              htmltools::tags$p(strong("FECHA: "), format(as.POSIXct(cita$fecha_inicio), "%d/%m/%Y %H:%M")),
              htmltools::tags$p(strong("ESPECIALIDAD: "), cita$tipo_servicio),
              htmltools::tags$p(strong("DOCTOR/A: "), cita$doctor)
            ),
            htmltools::tags$br(),
            htmltools::tags$p("Se confirma la asistencia del paciente a nuestra clínica en la fecha y hora indicadas para recibir tratamiento facultativo."),
            htmltools::tags$div(
              style = "margin-top: 80px; text-align: right;",
              htmltools::tags$p("Firmado: Dirección del Centro Médico"),
              htmltools::tags$p(format(Sys.Date(), "%d/%m/%Y"), style = "color: #888;")
            )
          )
        )
        
        # Proceso de renderizado PDF
        tmp <- tempfile(fileext = ".html")
        htmltools::save_html(html_doc, tmp)
        webshot2::webshot(tmp, file)
      }
    )
  })
}