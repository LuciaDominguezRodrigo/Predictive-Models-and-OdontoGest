resetConfirmUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class="confirm-box",
        h2("Nueva Contraseña"),
        # Solo dejamos esto. El Server se encargará de rellenarlo.
        uiOutput(ns("form_reset")),
        
        # Este es el contenedor para el cuadradito verde/rojo de aviso
        uiOutput(ns("confirm_msg_ui")),
        
        div(style="text-align: center;",
            actionLink(ns("go_login"), "Volver al inicio de sesión")
        )
    )
  )
}