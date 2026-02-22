# modules/login/login_server.R
library(shiny)
library(DBI)
library(bcrypt)

loginServer <- function(id, pool, user_logged, current_user, show_view, update_url) { 
  moduleServer(id, function(input, output, session){
    
    # 1. Creamos una variable reactiva para el mensaje
    mensaje_error <- reactiveVal("")
    
    # 2. El output siempre está "escuchando" a esa variable
    output$login_msg <- renderText({
      mensaje_error()
    })
    
    # --- Lógica de Login ---
    observeEvent(input$btn_login, {
      req(input$usuario, input$contraseña)
      
      # Limpiamos mensaje anterior al intentar loguear
      mensaje_error("")
      
      usuario_input     <- input$usuario
      contraseña_input  <- input$contraseña
      
      res <- tryCatch({
        dbGetQuery(pool,
                   "SELECT * FROM usuarios WHERE usuario=?",
                   params=list(usuario_input))
      }, error=function(e) NULL)
      
      if (!is.null(res) && nrow(res) == 1 && checkpw(contraseña_input, res$password_hash)) {
        if (!is.null(res$banneado) && res$banneado[1] == 1) {
          current_user(res)
          user_logged(TRUE)
        } else if (!is.null(res$banneado) && res$banneado[1] == 0) {
          # CORRECTO: Actualizamos el valor reactivo
          mensaje_error("❌ Usuario baneado. Contacte con administración")
        }
      } else {
        # CORRECTO: Actualizamos el valor reactivo
        mensaje_error("❌ Usuario o contraseña incorrectos")
        shinyjs::runjs(sprintf("document.getElementById('%s').value = '';", session$ns("contraseña")))
      }
    })
    
    # --- Lógica de Navegación ---
    
    # 1. Olvidé contraseña
    observeEvent(input$forgot_password, { 
      session$sendCustomMessage("clear_user", "reset_password")
      show_view("RESET") # Usamos el string para el app.R actualizado
    })
    
    # 2. Botón "Volver al Inicio" (Superior)
    observeEvent(input$back_to_public, {
      update_url("home")
      show_view("LANDING")
    })
    
    # 3. Enlace "Entrar como invitado" (Inferior)
    observeEvent(input$guest_access, {
      update_url("home")
      show_view("LANDING")
    })
  })
}