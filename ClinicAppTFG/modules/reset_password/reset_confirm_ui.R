# --- resetConfirmUI.R ---
resetConfirmUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(class="confirm-box",
        h2("Restablecer Contraseña (Paso 2/2)"),
        uiOutput(ns("form_reset")) 
       
    )
  )
}
