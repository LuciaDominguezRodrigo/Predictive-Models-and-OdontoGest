# modules/login/login_server.R
library(shiny)
library(DBI)
library(bcrypt)

loginServer <- function(id, pool, user_logged, current_user, show_view) { 
  moduleServer(id, function(input, output, session){
    
    observeEvent(input$btn_login, {
      # req() asegura que los inputs no estén vacíos antes de seguir
      req(input$usuario, input$contraseña)
      
      usuario_input     <- input$usuario
      contraseña_input  <- input$contraseña
      
      # Buscamos al usuario en la base de datos
      res <- tryCatch({
        dbGetQuery(pool,
                   "SELECT * FROM usuarios WHERE usuario=?",
                   params=list(usuario_input))
      }, error=function(e) NULL)
      
      # Verificamos si existe y si la contraseña coincide
      if (!is.null(res) &&
          nrow(res) == 1 &&
          checkpw(contraseña_input, res$password_hash)) {
        
        current_user(res)
        user_logged(TRUE)
        
      } else {
        # Si falla, lanzamos el error
        output$login_msg <- renderText("❌ Usuario o contraseña incorrectos")
        # Opcional: limpiar el input de contraseña por seguridad
        runjs(sprintf("document.getElementById('%s').value = '';", session$ns("contraseña")))
      }
    })
    
    observeEvent(input$forgot_password, { 
      session$sendCustomMessage("clear_user", "")
      show_view(TRUE)
    })
  })
}