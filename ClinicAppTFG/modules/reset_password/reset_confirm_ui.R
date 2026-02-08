resetConfirmUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      # Contenedor centrado idéntico al Login
      style = "min-height: 100vh; display: flex; align-items: center; justify-content: center; 
               background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%); padding: 20px;",
      
      div(
        class = "panel panel-default",
        style = "width: 100%; max-width: 450px; border-radius: 25px; border: none; 
                 box-shadow: 0 20px 25px -5px rgba(0, 0, 0, 0.1); overflow: hidden;",
        
        div(
          class = "panel-body",
          style = "padding: 50px;",
          
          # Título y subtítulo
          div(
            class = "text-center",
            style = "margin-bottom: 40px;",
            h2(style = "font-weight: 800; color: #6a0dad; margin-bottom: 10px;", "Nueva Contraseña"),
            p(style = "color: #64748b; font-size: 1.1rem;", "Define tu nueva clave de acceso")
          ),
          
          # El contenedor del formulario (mantenemos el UI Output)
          div(
            style = "margin-bottom: 30px;",
            uiOutput(ns("form_reset"))
          ),
          
          # Enlace para volver abajo
          div(
            class = "text-center",
            style = "margin-top: 20px;",
            actionLink(
              ns("go_login"), 
              label = "Cancelar y volver",
              style = "color: #94a3b8; font-weight: 500; text-decoration: none; font-size: 1rem;"
            )
          )
        )
      )
    )
  )
}