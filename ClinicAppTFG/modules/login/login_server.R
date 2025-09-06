library(shiny)
library(DBI)
library(bcrypt)

loginServer <- function(id, pool, user_logged, current_user) {
  moduleServer(id, function(input, output, session){
    observeEvent(input$btn_login, {
      req(input$usuario, input$contraseña)
      
      usuario_input <- input$usuario
      contraseña_input <- input$contraseña
      
      # Consulta parametrizada
      res <- tryCatch({
        dbGetQuery(pool, "SELECT * FROM usuarios WHERE usuario=?", params=list(usuario_input))
      }, error = function(e) NULL)
      
      if(nrow(res) == 1 && checkpw(contraseña_input, res$contraseña)){
        user_logged(TRUE)
        current_user(res)
      } else {
        output$login_msg <- renderText("❌ Usuario o contraseña incorrectos")
      }
    })
  })
}
