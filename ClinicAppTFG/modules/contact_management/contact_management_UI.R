library(shiny)
library(bslib)
library(fontawesome)

contactManagementUI <- function(id) {
  ns <- NS(id)
  
  div(
    class = "container-fluid py-4",
    
    div(
      class = "card shadow-lg border-0 rounded-4 overflow-hidden",
      
      # ---- ENCABEZADO ----
      div(
        class = "p-4 p-md-5 text-white position-relative",
        style = "
          background: linear-gradient(135deg, #7e22ce 0%, #4338ca 50%, #2563eb 100%);
        ",
        
        div(
          class = "d-flex justify-content-between align-items-center",
          
          # Título y subtítulo en blanco puro
          div(
            h2(
              class = "display-5 fw-bold mb-1",
              style = "color: #ffffff !important;",
              "Buzón de Consultas"
            ),
            p(
              class = "lead mb-0",
              style = "color: #ffffff !important;",
              "Gestión y seguimiento de atención al paciente."
            )
          ),
          
          # Icono decorativo grande
          div(
            class = "d-none d-md-block opacity-25",
            style = "font-size: 5rem;",
            fa("envelope-open-text")
          )
        )
      ),
      
      # ---- CUERPO Y TABS ----
      div(
        class = "card-body p-4",
        
        tabsetPanel(
          id = ns("buzon_tabs"),
          type = "pills",
          
          # ------- TAB Pendientes -------
          tabsetPanel(
            id = ns("buzon_tabs"),
            type = "pills",
            
            tabPanel(
              title = HTML("◴ Pendientes"),
              div(
                class = "mt-4 px-2",
                uiOutput(ns("mensajes_lista"))
              )
            ),
            
            tabPanel(
              title = HTML("▤ Historial"),
              div(
                class = "mt-4 px-2",
                uiOutput(ns("mensajes_archivados"))
              )
            )
          )

          # ------- TAB Historial -------
          
        )
      )
    )
  )
}

# ---- UI PRINCIPAL ----
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  contactManagementUI("contact")
)