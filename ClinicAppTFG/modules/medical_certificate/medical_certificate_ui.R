certificateUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "card shadow-sm p-4",
      
      h3(class = "text-purple", "Mis Justificantes"),
      p("Aquí puedes descargar los justificantes de tus citas finalizadas."),
      hr(),
      
      # Selector de cita
      uiOutput(ns("selector_cita")),
      
      br(),
      
      # Botón único de descarga
      downloadButton(
        ns("download_pdf"),
        "Descargar justificante",
        class = "btn btn-primary"
      )
    )
  )
}