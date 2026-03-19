# --- resetPasswordServer.R ---

# Constantes de Correo
MI_PASS_APP <- "qffj vlrz kmzq jpwz" 
MI_CORREO <- "clinicapptfg@gmail.com"
SMTP_HOST <- "smtp.gmail.com"
SMTP_PORT <- 465 

# Función auxiliar para generar un token seguro
generar_token_seguro <- function() {
  raw_token <- sodium::random(32)
  token <- sodium::bin2hex(raw_token)
  return(token)
}

# Función de envío de Correo
enviar_correo_restablecimiento <- function(destinatario, nombre_usuario, token_restablecimiento) {
  PUERTO_LOCAL <- "3841" 
  URL_BASE_APP <- paste0("http://127.0.0.1:", PUERTO_LOCAL) 
  
  enlace_restablecimiento <- paste0(URL_BASE_APP, "/?page=reset_confirm&token=", token_restablecimiento)
  
  cuerpo_html <- paste0(
    "<html><body>",
    "<h2>¡Hola, ", nombre_usuario, "!</h2>",
    "<p>Hemos recibido una solicitud para restablecer la contraseña de tu cuenta en ClinicAppTFG.</p>",
    "<p>Haz clic en el siguiente botón. Este enlace caducará en 30 minutos.</p>",
    "<a href='", enlace_restablecimiento, "' style='background-color: #6a0dad; color: white; padding: 10px 20px; text-align: center; text-decoration: none; display: inline-block; border-radius: 5px;'>",
    "Restablecer Contraseña</a>",
    "<p style='margin-top: 20px;'>Si no solicitaste este cambio, ignora este correo.</p>",
    "</body></html>"
  )
  
  tryCatch({
    mailR::send.mail(
      from = MI_CORREO,
      to = destinatario,
      subject = "Solicitud de Restablecimiento de Contraseña para ClinicAppTFG",
      html = TRUE, 
      body = cuerpo_html,
      smtp = list(
        host.name = SMTP_HOST,
        port = SMTP_PORT,
        user.name = MI_CORREO,
        passwd = MI_PASS_APP, 
        ssl = TRUE
      ),
      authenticate = TRUE,
      send = TRUE
    )
    return(TRUE)
  }, error = function(e) {
    message("Error al enviar correo: ", e$message) 
    return(FALSE)
  })
}

# SERVIDOR DEL MÓDULO
resetPasswordServer <- function(id, pool, show_view, update_url) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$btn_reset, {
      req(pool)
      req(input$usuario_reset)
      
      # 1. UI: Bloquear botón y mostrar spinner
      shinyjs::disable("btn_reset")
      shinyjs::html("btn_label", '<div class="loading-spinner"></div> Enviando...')
      output$reset_msg_ui <- renderUI({ NULL }) # Limpiar mensajes previos
      
      usuario_input <- input$usuario_reset
      
      # 2. Buscar usuario
      res <- tryCatch({
        dbGetQuery(pool,
                   "SELECT id, email, nombre 
                    FROM usuarios 
                    WHERE LOWER(usuario) = LOWER(?) OR LOWER(email) = LOWER(?)",
                   params = list(usuario_input, usuario_input))
      }, error = function(e) {
        print(paste("Error en DB Query:", e$message)) 
        return(NULL)
      })
      
      # Depuración en consola
      print("--- Intento de recuperación ---")
      print(res)
      
      # 3. Procesar si el usuario existe
      if (!is.null(res) && nrow(res) == 1) {
        user_info <- res[1, ]
        token <- generar_token_seguro()
        expiracion <- as.POSIXct(Sys.time() + 30 * 60, tz = "UTC") 
        
        # Guardar Token en DB
        update_result <- tryCatch({
          dbExecute(pool,
                    "UPDATE usuarios SET reset_token = ?, token_expiry = ? WHERE id = ?",
                    params = list(
                      token, 
                      format(expiracion, "%Y-%m-%d %H:%M:%S"), 
                      user_info$id[[1]] 
                    ))
        }, error = function(e) {
          print(paste("Error al actualizar token:", e$message))
          0
        })
        
        # Enviar Correo si la DB se actualizó correctamente
        if (update_result == 1) {
          envio_exitoso <- enviar_correo_restablecimiento(
            destinatario = user_info$email,
            nombre_usuario = user_info$nombre,
            token_restablecimiento = token
          )
          if(envio_exitoso) print("Correo enviado con éxito") else print("Fallo en el envío del correo")
        }
      }
      
      # 4. Respuesta visual final (Siempre positiva por seguridad)
      output$reset_msg_ui <- renderUI({
        div(
          style = "padding: 15px; border-radius: 12px; background-color: #f0fdf4; 
                   border: 1px solid #bbf7d0; color: #166534; font-weight: 500; 
                   text-align: center; margin-top: 20px; animation: fadeIn 0.8s;",
          "Si los datos coinciden con una cuenta activa, recibirás un correo de recuperación en breve."
        )
      })
      
      # 5. Restaurar UI
      shinyjs::enable("btn_reset")
      shinyjs::html("btn_label", "Enviar enlace")
      
      updateTextInput(session, "usuario_reset", value = "")
    })
    
    # Navegación volver
    observeEvent(input$back_login, {
      updateQueryString("?page=login", mode = "push", session = session)
      show_view("LOGIN") 
      update_url("login")
    })
  })
}