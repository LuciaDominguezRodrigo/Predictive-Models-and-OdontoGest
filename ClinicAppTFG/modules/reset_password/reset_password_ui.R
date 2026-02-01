resetPasswordUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "w-screen h-screen flex items-center justify-center bg-gradient-to-br from-gray-50 to-gray-100",
      
      div(
        class = "bg-white w-[450px] p-12 rounded-3xl shadow-2xl border border-gray-100/50",
        
        div(
          class = "text-center mb-10",
          h2(class = "text-4xl font-bold text-clinicPurple tracking-tight", "Recuperar Contraseña"),
          p(class = "text-gray-500 text-lg mt-4 px-2", "Escribe tu correo para restablecer tu cuenta")
        ),
        
        div(
          class = "mb-8",
          tags$label("Usuario o Correo Electrónico", `for` = ns("usuario_reset"), 
                     class = "block text-sm font-bold text-gray-600 uppercase tracking-wider mb-3 ml-1"),
          tags$input(
            id = ns("usuario_reset"),
            type = "text",
            placeholder = "ejemplo@correo.com",
            class = "w-full px-5 py-4 text-lg rounded-xl border border-gray-200 bg-gray-50 focus:bg-white focus:ring-4 focus:ring-purple-100 focus:border-clinicPurple outline-none transition-all duration-200"
          )
        ),
        
        actionButton(
          ns("btn_reset"),
          "Enviar enlace",
          class = "w-full py-5 rounded-xl text-white text-xl font-bold tracking-wide bg-clinicPurple hover:bg-purple-700 active:scale-[0.98] transition-all duration-200 shadow-lg shadow-purple-200"
        ),
        
        div(
          class = "mt-8 text-center",
          actionLink(
            ns("back_login"),
            label = "Volver al inicio de sesión",
            class = "text-lg text-gray-400 hover:text-clinicPurple transition-colors font-medium"
          )
        ),
        
        # Contenedor de mensaje de éxito/error
        uiOutput(ns("reset_msg_ui"), class = "mt-6 text-center text-lg")
      )
    )
  )
}