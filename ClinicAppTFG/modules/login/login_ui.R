loginUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      # Contenedor principal centrado
      style = "min-height: 100vh; display: flex; align-items: center; justify-content: center; 
               background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%); padding: 20px;",
      
      div(
        class = "panel panel-default",
        style = "width: 100%; max-width: 380px; border-radius: 25px; border: none; 
                 box-shadow: 0 20px 25px -5px rgba(0, 0, 0, 0.1); overflow: hidden;",
        
        div(
          class = "panel-body",
          style = "padding: 40px; text-align: center;", # Añadido text-align center para ayudar al centrado
          
          # Título
          div(
            style = "margin-bottom: 30px;",
            h2(style = "font-weight: 800; color: #6a0dad; margin-bottom: 5px; font-size: 1.8rem;", "Login"),
            p(style = "color: #64748b; font-size: 1rem;", "Introduce tus credenciales")
          ),
          
          # Campo Usuario (Alineado a la izquierda para los labels)
          div(
            class = "form-group",
            style = "margin-bottom: 20px; text-align: left;",
            tags$label("Usuario", style = "font-weight: 700; color: #475569; text-transform: uppercase; font-size: 0.75rem; letter-spacing: 0.05em;"),
            tags$input(
              id = ns("usuario"),
              type = "text",
              class = "form-control",
              placeholder = "Tu usuario",
              style = "height: 45px; border-radius: 12px; border: 1px solid #e2e8f0; background-color: #f8fafc;"
            )
          ),
          
          # Campo Contraseña
          div(
            class = "form-group",
            style = "margin-bottom: 25px; text-align: left;",
            tags$label("Contraseña", style = "font-weight: 700; color: #475569; text-transform: uppercase; font-size: 0.75rem; letter-spacing: 0.05em;"),
            tags$input(
              id = ns("contraseña"),
              type = "password",
              class = "form-control",
              placeholder = "••••••••",
              style = "height: 45px; border-radius: 12px; border: 1px solid #e2e8f0; background-color: #f8fafc;"
            )
          ),
          
          # Botón Login (Centrado y con tamaño ajustado)
          actionButton(
            ns("btn_login"),
            "Ingresar al Sistema",
            class = "btn btn-primary",
            style = "background-color: #6a0dad; border: none; height: 50px; width: 100%; border-radius: 15px; 
                     font-size: 1.1rem; font-weight: 700; transition: transform 0.2s; 
                     box-shadow: 0 4px 15px rgba(106, 13, 173, 0.2); margin: 0 auto;"
          ),
          
          # Mensaje de error
          div(
            style = "margin-top: 20px; color: #ef4444; font-weight: 500; min-height: 20px;",
            textOutput(ns("login_msg"))
          ),
          
          hr(style = "margin: 25px 0; border-color: #f1f5f9;"),
          
          div(
            style = "display: flex; flex-direction: column; gap: 10px;",
            actionLink(ns("forgot_password"), "Olvidé mi contraseña", style = "color: #94a3b8; font-size: 0.9rem;"),
            actionLink(ns("guest_access"), "Ver consejos de salud dental", style = "color: #2563eb; font-weight: 700; font-size: 0.9rem;")
          )
        )
      )
    )
  )
}