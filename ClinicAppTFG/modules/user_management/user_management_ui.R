userManagementUI <- function(id) {
  ns <- NS(id)
  
  div(
    class = "max-w-4xl mx-auto bg-white p-8 rounded-2xl shadow-sm border border-gray-100",
    h3(class = "text-2xl font-bold text-clinicPurple mb-6", "Dar de alta nuevo personal o paciente"),
    
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
                      choices = c("Paciente" = "paciente", 
                                  "Recepción" = "recepcion", 
                                  "Doctor/Personal Clínico" = "doctor"), 
                      width = "100%")
        )
    ),
    
    div(class = "mt-8 flex justify-end",
        actionButton(
          ns("btn_save_user"),
          "Registrar en el Sistema",
          class = "bg-clinicPurple text-white px-10 py-3 rounded-xl font-bold hover:shadow-lg hover:bg-purple-700 transition-all"
        )
    )
  )
}