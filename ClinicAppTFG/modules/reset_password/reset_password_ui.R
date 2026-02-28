resetPasswordUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Necesario para habilitar/deshabilitar el botón
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
        @keyframes fadeIn { from { opacity: 0; } to { opacity: 1; } }
        @keyframes spin { to { transform: rotate(360deg); } }
        .loading-spinner {
          display: inline-block;
          width: 18px; height: 18px;
          border: 3px solid rgba(255,255,255,.3);
          border-radius: 50%;
          border-top-color: #fff;
          vertical-align: middle;
          margin-right: 8px;
          animation: spin 1s ease-in-out infinite;
        }
      "))
    ),
    div(
      style = "min-height: 100vh; display: flex; align-items: center; justify-content: center; 
               background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%); padding: 20px;",
      
      div(
        class = "panel panel-default",
        style = "width: 100%; max-width: 450px; border-radius: 25px; border: none; 
                 box-shadow: 0 20px 25px -5px rgba(0, 0, 0, 0.1); overflow: hidden;",
        
        div(
          class = "panel-body",
          style = "padding: 50px;",
          
          div(
            class = "text-center",
            style = "margin-bottom: 40px;",
            h2(style = "font-weight: 800; color: #6a0dad; margin-bottom: 15px;", "Recuperar Contraseña"),
            p(style = "color: #64748b; font-size: 1.1rem;", "Escribe tu correo para restablecer tu cuenta")
          ),
          
          div(
            class = "form-group",
            style = "margin-bottom: 30px;",
            tags$label("Usuario o Correo Electrónico", 
                       style = "font-weight: 700; color: #475569; text-transform: uppercase; font-size: 0.8rem; letter-spacing: 0.05em; display: block; margin-bottom: 10px;"),
            tags$input(
              id = ns("usuario_reset"),
              type = "text",
              class = "form-control",
              placeholder = "ejemplo@correo.com",
              style = "height: 50px; border-radius: 12px; border: 1px solid #e2e8f0; background-color: #f8fafc;"
            )
          ),
          
          # Botón con ID interno para el texto (para el spinner)
          actionButton(
            ns("btn_reset"),
            span(id = ns("btn_label"), "Enviar enlace"),
            class = "btn btn-primary btn-block",
            style = "background-color: #6a0dad; border: none; height: 55px; border-radius: 15px; 
                     font-size: 1.2rem; font-weight: 700; transition: transform 0.2s; box-shadow: 0 4px 15px rgba(106, 13, 173, 0.2);"
          ),
          
          div(
            class = "text-center",
            style = "margin-top: 30px;",
            actionLink(
              ns("back_login"),
              label = "Volver al inicio de sesión",
              style = "color: #94a3b8; font-weight: 500; text-decoration: none; font-size: 1rem;"
            )
          ),
          
          # Aquí se renderizará el mensaje estético
          div(
            style = "margin-top: 20px;",
            uiOutput(ns("reset_msg_ui"))
          )
        )
      )
    )
  )
}