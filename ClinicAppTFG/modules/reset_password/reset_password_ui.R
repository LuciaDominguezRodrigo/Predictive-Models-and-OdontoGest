# --- modules/reset_password/resetPasswordUI.R ---

resetPasswordUI <- function(id) {
  # Crear un namespace para aislar los IDs de este módulo
  ns <- NS(id)
  
  tagList(
    div(class="reset-box",
        h2("Recuperar Contraseña (Paso 1/2)"),
        p("Introduce tu **nombre de usuario** o el **correo electrónico** asociado a tu cuenta para recibir el enlace de restablecimiento."),
        
        # Campo de entrada, etiqueta adaptada para aceptar usuario O correo
        textInput(ns("usuario_reset"), "Usuario o Correo Electrónico"),
        
        # Botón para iniciar el proceso
        actionButton(ns("btn_reset"), "Enviar correo de recuperación", class = "btn-primary"),
        
        # Mensaje de retroalimentación (éxito o error)
        verbatimTextOutput(ns("reset_msg")),
        
        br(),
        
        # Enlace para volver a la pantalla de login
        actionLink(ns("back_login"), "Volver al login")
    )
  )
}