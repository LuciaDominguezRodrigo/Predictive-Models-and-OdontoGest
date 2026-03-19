resetConfirmUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = "
        min-height: 100vh; 
        display: flex; 
        align-items: center; 
        justify-content: center; 
        background: linear-gradient(135deg, #eef2ff 0%, #f8fafc 100%);
        padding: 20px;
      ",
      
      div(
        style = "
          width: 100%; 
          max-width: 420px; 
          background: white;
          border-radius: 20px;
          box-shadow: 0 10px 30px rgba(0,0,0,0.08);
          padding: 40px 35px;
        ",
        
        div(class="text-center mb-3",
            icon("key", style="font-size: 28px; color: #7e57c2;")
        ),
        
        # TÍTULO
        div(
          class = "text-center",
          style = "margin-bottom: 25px;",
          
          h3(style = "
               font-weight: 700; 
               color: #1e293b; 
               margin-bottom: 6px;
             ", 
             "Nueva contraseña"),
          
          p(style = "
              color: #64748b; 
              font-size: 0.95rem;
              margin: 0;
            ", 
            "Introduce una nueva contraseña segura")
        ),
        
        # FORM
        div(
          style = "margin-bottom: 20px;",
          uiOutput(ns("form_reset"))
        ),
        
        # LINK ABAJO
        div(
          class = "text-center",
          style = "margin-top: 10px;",
          
          actionLink(
            ns("go_login"), 
            label = "Volver al inicio de sesión",
            style = "
              color: #7e57c2; 
              font-weight: 500; 
              font-size: 0.9rem;
              text-decoration: none;
            "
          )
        )
      )
    )
  )
}