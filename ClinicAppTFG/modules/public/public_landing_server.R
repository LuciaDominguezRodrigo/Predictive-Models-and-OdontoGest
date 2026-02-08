landingServer <- function(id, show_view) {
  moduleServer(id, function(input, output, session) {
    
    # Al hacer clic en el botón de Acceso Profesionales
    observeEvent(input$go_to_login, {
      # 1. Cambiamos la URL visualmente (opcional pero recomendado)
      updateQueryString("?page=login", mode = "push", session = session)
      
      # 2. Cambiamos el estado reactivo para que app.R renderice el Login
      show_view("LOGIN")
    })
    
  })
}