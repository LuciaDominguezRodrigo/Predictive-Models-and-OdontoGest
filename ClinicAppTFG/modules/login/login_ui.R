loginUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      # Contenedor principal centrado (Bootstrap)
      style = "min-height: 100vh; display: flex; align-items: center; justify-content: center; 
               background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%); padding: 20px;",
      
      div(
        class = "panel panel-default",
        style = "width: 100%; max-width: 450px; border-radius: 25px; border: none; 
                 box-shadow: 0 20px 25px -5px rgba(0, 0, 0, 0.1); overflow: hidden;",
        
        div(
          class = "panel-body",
          style = "padding: 50px;",
          
          # Título
          div(
            class = "text-center",
            style = "margin-bottom: 40px;",
            h2(style = "font-weight: 800; color: #6a0dad; margin-bottom: 10px;", "Login"),
            p(style = "color: #64748b; font-size: 1.1rem;", "Introduce tus credenciales")
          ),
          
          # Campo Usuario
          div(
            class = "form-group",
            style = "margin-bottom: 25px;",
            tags$label("Usuario", style = "font-weight: 700; color: #475569; text-transform: uppercase; font-size: 0.8rem; letter-spacing: 0.05em;"),
            tags$input(
              id = ns("usuario"),
              type = "text",
              class = "form-control",
              placeholder = "Tu usuario",
              style = "height: 50px; border-radius: 12px; border: 1px solid #e2e8f0; background-color: #f8fafc;"
            )
          ),
          
          # Campo Contraseña
          div(
            class = "form-group",
            style = "margin-bottom: 30px;",
            tags$label("Contraseña", style = "font-weight: 700; color: #475569; text-transform: uppercase; font-size: 0.8rem; letter-spacing: 0.05em;"),
            tags$input(
              id = ns("contraseña"),
              type = "password",
              class = "form-control",
              placeholder = "••••••••",
              style = "height: 50px; border-radius: 12px; border: 1px solid #e2e8f0; background-color: #f8fafc;"
            )
          ),
          
          # Botón Login
          actionButton(
            ns("btn_login"),
            "Ingresar al Sistema",
            class = "btn btn-primary btn-block",
            style = "background-color: #6a0dad; border: none; height: 55px; border-radius: 15px; 
                     font-size: 1.2rem; font-weight: 700; transition: transform 0.2s; box-shadow: 0 4px 15px rgba(106, 13, 173, 0.2);"
          ),
          
          # Mensaje de error
          div(
            class = "text-center",
            style = "margin-top: 20px; color: #ef4444; font-weight: 500; min-height: 24px;",
            textOutput(ns("login_msg"))
          ),
          
          # Enlaces inferiores
          hr(style = "margin: 30px 0; border-color: #f1f5f9;"),
          
          div(
            class = "text-center",
            style = "display: flex; flex-direction: column; gap: 15px;",
            actionLink(
              ns("forgot_password"),
              label = "Olvidé mi contraseña",
              style = "color: #94a3b8; font-weight: 500; text-decoration: none;"
            ),
            actionLink(
              ns("guest_access"),
              label = "Ver consejos de salud dental",
              style = "color: #2563eb; font-weight: 700; text-decoration: none;"
            )
          )
        )
      )
    )
  )
}