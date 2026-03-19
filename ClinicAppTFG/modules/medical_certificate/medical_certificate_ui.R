library(shiny)
library(shinycssloaders)

certificateUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML(paste0("
        /* Tarjeta principal con estilo moderno */
        .card-justificante {
          border: none;
          border-radius: 15px;
          box-shadow: 0 10px 25px rgba(106, 27, 154, 0.1);
          background: white;
        }
        
        .text-purple-corp { color: #6a1b9a !important; font-weight: 600; }

        /* FORZAR COLOR MORADO EN EL BOTÓN */
        #", ns("download_pdf"), ".btn-download, 
        #", ns("download_pdf"), ".btn-default {
          background-color: #6a1b9a !important;
          border-color: #6a1b9a !important;
          color: white !important;
          font-weight: 600;
          padding: 12px 20px;
          transition: all 0.3s ease;
          border-radius: 8px;
          width: 100%;
          margin-top: 15px;
          opacity: 1;
        }

        /* Efecto Hover */
        #", ns("download_pdf"), ".btn-download:hover {
          background-color: #4a148c !important;
          border-color: #4a148c !important;
          transform: translateY(-1px);
          box-shadow: 0 4px 12px rgba(106, 27, 154, 0.2);
        }
        
        #", ns("download_pdf"), " i { margin-right: 8px; }

        /* Ajuste para el selector */
        .selectize-input.focus { border-color: #6a1b9a !important; }
      ")))
    ),
    
    div(
      class = "card card-justificante p-4",
      div(class = "d-flex align-items-center mb-3",
          icon("file-medical", class = "fa-2x text-purple-corp me-3"),
          h3(class = "text-purple-corp mb-0", "Mis Justificantes")
      ),
      p(class = "text-muted", "Selecciona una cita completada para descargar tu justificante oficial."),
      hr(),
      
      # Spinner morado mientras se cargan los datos o se genera el PDF
      shinycssloaders::withSpinner(
        uiOutput(ns("ui_selector")),
        color = "#6a1b9a",
        type = 6,
        size = 0.8
      )
    )
  )
}