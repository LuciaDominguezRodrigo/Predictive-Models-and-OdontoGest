# --- resetPasswordServer.R ---

# Constantes de Correo
MI_PASS_APP <- "qffj vlrz kmzq jpwz" 
MI_CORREO <- "clinicapptfg@gmail.com"
SMTP_HOST <- "smtp.gmail.com"
# Puerto 465 SSL. Si falla, probar 587 con ssl=FALSE y tls=TRUE.
SMTP_PORT <- 465 

# Función auxiliar para generar un token seguro
generar_token_seguro <- function() {
  raw_token <- sodium::random(32)
  token <- sodium::bin2hex(raw_token)
  return(token)
}

# Función de envío de Correo
enviar_correo_restablecimiento <- function(destinatario, nombre_usuario, token_restablecimiento) {
  
  # Configuración Localhost 
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
    send.mail(
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
        ssl = TRUE,
        tls = FALSE # Normalmente no es necesario si ssl=TRUE
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


# SERVIDOR DEL MÓDULO (Lógica de solicitud de correo)
resetPasswordServer <- function(id, pool, show_view,  update_url) {
  moduleServer(id, function(input, output, session){
    
    observeEvent(input$btn_reset, {
      req(pool)
      req(input$usuario_reset)
      usuario_input <- input$usuario_reset
      
      # 1. Buscar usuario
      res <- tryCatch({
        dbGetQuery(pool,
                   "SELECT id, email, nombre 
              FROM usuarios 
              WHERE LOWER(usuario) = LOWER(?) OR LOWER(email) = LOWER(?)",
                   params = list(usuario_input, usuario_input))
      }, error = function(e) {
        print(paste("Error en DB:", e$message)) 
        return(NULL)
      })
      
      # --- INICIO DE LÍNEAS DE DEPURACIÓN TEMPORAL ---
      print("--- Resultado de la consulta de usuario ---")
      print(res)
      print("---------------------------------------------")
      # --- FIN DE LÍNEAS DE DEPURACIÓN TEMPORAL ---
      
      # 2. Si el usuario existe, procesar
      if (!is.null(res) && nrow(res) == 1) {
        user_info <- res[1, ]
        
        token <- generar_token_seguro()
        # 30 minutos de validez
        expiracion <- as.POSIXct(Sys.time() + 30 * 60, tz = "UTC") 
        
        # 3. Guardar Token y Expiración en la DB
        update_result <- tryCatch({
          dbExecute(pool,
                    # Usamos 'id' en el WHERE
                    "UPDATE usuarios SET reset_token = ?, token_expiry = ? WHERE id = ?",
                    params = list(
                      token, 
                      format(expiracion, "%Y-%m-%d %H:%M:%S"), 
                      #  Usamos 'id' y forzamos longitud 1 con [[1]]
                      user_info$id[[1]] 
                    )
          )
        }, error = function(e) {
          message("Error al actualizar token en DB: ", e$message)
          0
        })
        
        # 4. Enviar Correo si la DB se actualizó
        if (update_result == 1) {
          envio_exitoso <- enviar_correo_restablecimiento(
            destinatario = user_info$email, # Usar 'email' de la DB
            nombre_usuario = user_info$nombre,
            token_restablecimiento = token
          )
          
          if (envio_exitoso) {
            output$reset_msg <- renderText({
              paste0("✅ Se ha enviado un correo de recuperación a ", user_info$email, ". Revisa tu bandeja de entrada.")
            })
          } else {
            output$reset_msg <- renderText("⚠️ Error al enviar el correo. Contacta al soporte técnico.")
          }
          
        } else {
          output$reset_msg <- renderText("❌ Error interno. Inténtalo de nuevo.")
        }
        
      } else {
        # Mensaje genérico para evitar revelar si el usuario existe o no
        output$reset_msg <- renderText("✅ Si el usuario existe, recibirás un correo de recuperación en breve.") 
      }
    })
    
    observeEvent(input$back_login, {
      # Limpiamos la query de la URL
      updateQueryString("?page=login", mode = "push", session = session)
      
      # Cambiamos el estado a LOGIN (el string exacto que espera app.R)
      show_view("LOGIN") 
      
      # Actualizamos el historial del navegador si es necesario
      update_url("login")
    })
    
  })
}