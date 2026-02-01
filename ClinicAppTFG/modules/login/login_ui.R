loginUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "w-screen h-screen flex items-center justify-center bg-gradient-to-br from-gray-50 to-gray-100",
      
      div(
        class = "bg-white w-[450px] p-12 rounded-3xl shadow-2xl border border-gray-100/50",
        
        # Título - Tamaño aumentado a text-4xl
        div(
          class = "text-center mb-10",
          h2(class = "text-4xl font-bold text-clinicPurple tracking-tight", "Login Clínica"),
          p(class = "text-gray-500 text-lg mt-3", "Bienvenido, por favor ingresa tus datos")
        ),
        
        # Campo Usuario
        div(
          class = "mb-8",
          tags$label("Usuario", `for` = ns("usuario"), 
                     class = "block text-sm font-bold text-gray-600 uppercase tracking-wider mb-3 ml-1"),
          tags$input(
            id = ns("usuario"),
            type = "text",
            placeholder = "Introduce tu usuario",
            # Aumentado: py-4 (padding) y text-lg (fuente)
            class = "w-full px-5 py-4 text-lg rounded-xl border border-gray-200 bg-gray-50 focus:bg-white focus:ring-4 focus:ring-purple-100 focus:border-clinicPurple outline-none transition-all duration-200"
          )
        ),
        
        # Campo Contraseña
        div(
          class = "mb-10",
          tags$label("Contraseña", `for` = ns("contraseña"), 
                     class = "block text-sm font-bold text-gray-600 uppercase tracking-wider mb-3 ml-1"),
          tags$input(
            id = ns("contraseña"),
            type = "password",
            placeholder = "••••••••",
            class = "w-full px-5 py-4 text-lg rounded-xl border border-gray-200 bg-gray-50 focus:bg-white focus:ring-4 focus:ring-purple-100 focus:border-clinicPurple outline-none transition-all duration-200"
          )
        ),
        
        actionButton(
          ns("btn_login"),
          "Ingresar al Sistema",
          class = "w-full py-5 rounded-xl text-white text-xl font-bold tracking-wide bg-clinicPurple hover:bg-purple-700 active:scale-[0.98] transition-all duration-200 shadow-lg shadow-purple-200"
        ),
        
        # Mensaje de error 
        div(
          class = "mt-6 text-center text-base text-red-500 font-medium min-h-[24px]",
          textOutput(ns("login_msg"))
        ),
        
        # Olvidé contraseña 
        div(
          class = "mt-8 text-center",
          actionLink(
            ns("forgot_password"),
            label = "Olvidé mi contraseña",
            class = "text-lg text-gray-400 hover:text-clinicPurple transition-colors font-medium"
          )
        )
      )
    )
  )
}