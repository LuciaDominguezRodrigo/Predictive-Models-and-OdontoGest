loginUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Fondo con un degradado sutil para mayor profundidad
    div(
      class = "w-screen h-screen flex items-center justify-center bg-gradient-to-br from-gray-50 to-gray-100",
      
      # Caja del login - Más estilizada
      div(
        class = "bg-white w-[400px] p-10 rounded-3xl shadow-2xl border border-gray-100/50",
        
        # Título - Con tracking-tight para elegancia
        div(
          class = "text-center mb-10",
          h2(class = "text-3xl font-bold text-clinicPurple tracking-tight", "Login Clínica"),
          p(class = "text-gray-400 text-sm mt-2", "Bienvenido, por favor ingresa tus datos")
        ),
        
        # Usuario
        div(
          class = "mb-6",
          tags$label("Usuario", `for` = ns("usuario"), class = "block text-xs font-semibold text-gray-500 uppercase tracking-wider mb-2 ml-1"),
          tags$input(
            id = ns("usuario"),
            type = "text",
            placeholder = "Introduce tu usuario",
            class = "w-full px-4 py-3 rounded-xl border border-gray-200 bg-gray-50 focus:bg-white focus:ring-2 focus:ring-purple-200 focus:border-clinicPurple outline-none transition-all duration-200"
          )
        ),
        
        # Contraseña
        div(
          class = "mb-8",
          tags$label("Contraseña", `for` = ns("contraseña"), class = "block text-xs font-semibold text-gray-500 uppercase tracking-wider mb-2 ml-1"),
          tags$input(
            id = ns("contraseña"),
            type = "password",
            placeholder = "••••••••",
            class = "w-full px-4 py-3 rounded-xl border border-gray-200 bg-gray-50 focus:bg-white focus:ring-2 focus:ring-purple-200 focus:border-clinicPurple outline-none transition-all duration-200"
          )
        ),
        
        # Botón - Más alto y con tipografía compensada
        actionButton(
          ns("btn_login"),
          "Ingresar al Sistema",
          class = "w-full py-4 rounded-xl text-white font-bold tracking-wide bg-clinicPurple hover:bg-purple-700 active:scale-[0.98] transition-all duration-200 shadow-lg shadow-purple-200"
        ),
        
        # Mensaje de error (si existe)
        div(
          class = "mt-4 text-center text-sm text-red-500 font-medium min-h-[20px]",
          verbatimTextOutput(ns("login_msg"))
        ),
        
        # Olvidé contraseña - Más discreto
        div(
          class = "mt-8 text-center",
          actionLink(
            ns("forgot_password"),
            label = "Olvidé mi contraseña",
            class = "text-sm text-gray-400 hover:text-clinicPurple transition-colors font-medium"
          )
        )
      )
    )
  )
}