library(shiny)

mainUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3(textOutput(ns("welcome"))),
    verbatimTextOutput(ns("user_info")),
    actionButton(ns("btn_logout"),"Cerrar sesión")
  )
}

mainServer <- function(id, current_user, user_logged) {
  moduleServer(id, function(input, output, session){
    output$welcome <- renderText({
      paste0("Bienvenido ", current_user()$nombre)
    })
    
    output$user_info <- renderPrint({
      current_user()
    })
    
    observeEvent(input$btn_logout,{
      user_logged(FALSE)
      current_user(NULL)
    })
  })
}
