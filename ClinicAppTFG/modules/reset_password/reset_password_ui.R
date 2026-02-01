resetPasswordUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class="reset-box",
        h2("Recuperar Contraseña"),
        p("Introduce tu correo electrónico para recibir el enlace de restablecimiento."),
        
        textInput(ns("usuario_reset"), "Usuario o Correo Electrónico"),
        
        actionButton(ns("btn_reset"), "Enviar correo", class = "btn-login-custom"),
        
        # CAMBIO AQUÍ: Usamos uiOutput en lugar de verbatim
        uiOutput(ns("reset_msg_ui")),
        
        br(),
        actionLink(ns("back_login"), "Volver al login")
    )
  )
}