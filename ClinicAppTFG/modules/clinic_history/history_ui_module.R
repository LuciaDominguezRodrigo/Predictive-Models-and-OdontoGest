historyUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$script(HTML("
        Shiny.addCustomMessageHandler('scrollToNew', function(x) {
          window.scrollTo({ top: document.body.scrollHeight, behavior: 'smooth' });
        });
      ")), 
    
    h2(class = "text-purple mb-4", "Historial Clínico de Pacientes"),
    
    fluidRow(
      column(4,
             div(class = "card shadow-sm p-3 mb-4",
                 shinyWidgets::pickerInput(
                   ns("select_paciente"), 
                   "Seleccionar Paciente:",
                   choices = c("Cargando..." = NA),
                   options = list(
                     `live-search` = TRUE,
                     `none-selected-text` = "Seleccione un paciente..."
                   ),
                   width = "100%"
                 ),
                 hr(),
                 uiOutput(ns("resumen_paciente")),
                 br(),
                 uiOutput(ns("btn_nueva_nota_container")) 
             )
      ),
      
      column(8,
             div(class = "card shadow-sm p-4",
                 h4("Línea de Tiempo / Evolución"),
                 uiOutput(ns("timeline_historial"))
             )
      )
    )
  )
}