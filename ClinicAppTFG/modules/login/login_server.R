library(shiny)
library(DBI)
library(bcrypt)

loginServer <- function(id, pool, user_logged, current_user, show_view) { 
  moduleServer(id, function(input, output, session){
    
    # ----- LÓGICA DE INGRESO -----
    observeEvent(input$btn_login, {
      req(input$usuario, input$contraseña)
      
      usuario_input     <- input$usuario
      contraseña_input  <- input$contraseña
      
      res <- tryCatch({
        dbGetQuery(pool,
                   "SELECT * FROM usuarios WHERE usuario=?",
                   params=list(usuario_input))
      }, error=function(e) NULL)
      
      if (!is.null(res) &&
          nrow(res) == 1 &&
          checkpw(contraseña_input, res$password_hash)) {
        
        user_logged(TRUE)
        current_user(res)
        
      } else {
        output$login_msg <- renderText("❌ Usuario o contraseña incorrectos")
      }
    })
    
    # ---- IR A RECUPERACIÓN ----
    observeEvent(input$forgot_password, { 
      show_view(TRUE)     # TRUE = solicitar correo
    })
    
  })
}
