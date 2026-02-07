userManagementUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # 1. Importamos Tailwind CSS y retocamos los inputs de Shiny
    tags$head(
      tags$script(src = "https://cdn.tailwindcss.com"),
      tags$style(HTML("
    
    div(class = "grid grid-cols-1 md:grid-cols-2 gap-6",
        div(
          tags$label("Nombre y Apellidos", class = "block text-sm font-bold text-gray-600 mb-2"),
          textInput(ns("nombre"), NULL, placeholder = "Nombre completo", width = "100%")
        ),
        div(
          tags$label("Email", class = "block text-sm font-bold text-gray-600 mb-2"),
          textInput(ns("email"), NULL, placeholder = "correo@ejemplo.com", width = "100%")
        ),
        div(
          tags$label("Nombre de Usuario (para login)", class = "block text-sm font-bold text-gray-600 mb-2"),
          textInput(ns("usuario"), NULL, placeholder = "usuario123", width = "100%")
        ),
        div(
          tags$label("Contraseña Temporal", class = "block text-sm font-bold text-gray-600 mb-2"),
          passwordInput(ns("password"), NULL, placeholder = "••••••••", width = "100%")
        ),
        div(
          tags$label("Rol del usuario", class = "block text-sm font-bold text-gray-600 mb-2"),
          selectInput(ns("tipo_usuario"), NULL, 
          class = "mt-10 pt-6 border-t border-gray-100 flex justify-end",
          actionButton(
            ns("btn_save_user"),
            "Registrar en el Sistema",
            class = "bg-purple-600 hover:bg-purple-700 text-white px-8 py-3 rounded-2xl font-bold text-lg shadow-lg shadow-purple-200 transition-all active:scale-95"
          )
        )
      )
    )
  )
}