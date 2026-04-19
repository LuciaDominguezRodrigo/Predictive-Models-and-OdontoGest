library(shiny)
library(DBI)
library(htmltools)
library(webshot2)
library(rmarkdown)


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
    # 3. Handler de descarga UNIFICADO (Usa RMarkdown/LaTeX en lugar de Webshot)
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("Justificante_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        req(input$cita_seleccionada)
        
        # Bloquear UI para evitar clics dobles
        shinyjs::disable("download_pdf")
        on.exit(shinyjs::enable("download_pdf"))
        
        # Obtener datos específicos de la cita
        df <- citas_completadas()
        cita <- df[df$id == input$cita_seleccionada, ]
        
        # 1. Preparar archivos temporales
        temp_rmd <- file.path(tempdir(), "justificante.Rmd")
        temp_pdf <- file.path(tempdir(), "justificante.pdf")
        
        # 2. Crear la plantilla RMarkdown (Estilo similar al historial clínico)
        writeLines(c(
          "---",
          "title: 'JUSTIFICANTE MÉDICO'",
          "output: pdf_document",
          "params:",
          "  paciente: NA",
          "  fecha: NA",
          "  especialidad: NA",
          "  doctor: NA",
          "---",
          "",
          "## Información de la Cita",
          "---",
          "**PACIENTE:** `r params$paciente`  ",
          "**FECHA:** `r params$fecha`  ",
          "**ESPECIALIDAD:** `r params$especialidad`  ",
          "**DOCTOR/A:** `r params$doctor`  ",
          "",
          "\\vspace{2cm}",
          "",
          "Se confirma la asistencia del paciente a nuestra clínica en la fecha y hora indicadas para recibir tratamiento facultativo.",
          "",
          "\\vspace{3cm}",
          "",
          "\\begin{flushright}",
          "Firmado: Dirección del Centro Médico  ",
          paste0("Fecha de emisión: ", format(Sys.Date(), "%d/%m/%Y")),
          "\\end{flushright}"
        ), temp_rmd)
        
        # 3. Renderizar usando el motor nativo
        rmarkdown::render(
          temp_rmd, 
          output_file = temp_pdf, 
          params = list(
            paciente = current_user()$nombre,
            fecha = format(as.POSIXct(cita$fecha_inicio), "%d/%m/%Y %H:%M"),
            especialidad = cita$tipo_servicio,
            doctor = cita$doctor
          ), 
          envir = new.env(parent = globalenv()), 
          quiet = TRUE
        )
        
        # 4. Entregar el archivo
        file.copy(temp_pdf, file)
      }
    )
  })
}