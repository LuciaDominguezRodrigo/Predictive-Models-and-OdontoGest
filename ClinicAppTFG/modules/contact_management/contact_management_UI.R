library(bslib)


contactManagementUI <- function(id) {
  ns <- NS(id)
  div(class = "container-fluid py-4",
      div(class = "card shadow-lg border-0 rounded-4 overflow-hidden",
          
          # Encabezado con degradado CSS estándar
          div(class = "p-4 p-md-5 text-white",
              style = "background: linear-gradient(135deg, #6b21a8 0%, #4338ca 100%);",
              div(class = "d-flex justify-content-between align-items-center",
                  div(
                    h2(class = "display-5 fw-bold mb-1", "Buzón de Consultas"),
                    p(class = "lead mb-0 opacity-75", "Gestión y seguimiento de atención al paciente.")
                  ),
                  icon("paper-plane", class = "display-1 opacity-25 d-none d-md-block")
              )
          ),
          
          # Cuerpo con Pestañas
          div(class = "card-body p-4",
              tabsetPanel(
                id = ns("buzon_tabs"),
                type = "pills", 
                
                tabPanel(
                  title = "📥 Pendientes",
                  div(class = "mt-4 px-2",
                      uiOutput(ns("mensajes_lista"))
                  )
                ),
                
                tabPanel(
                  title = "📂 Historial",
                  div(class = "mt-4 px-2",
                      uiOutput(ns("mensajes_archivados"))
                  )
                )
              )
          )
      )
  )
}


ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),  # o el tema que quieras
  contactManagementUI("contact")
)