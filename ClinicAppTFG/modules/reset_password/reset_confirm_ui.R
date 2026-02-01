resetConfirmUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "w-screen h-screen flex items-center justify-center bg-gradient-to-br from-gray-50 to-gray-100",
      
      div(
        class = "bg-white w-[450px] p-12 rounded-3xl shadow-2xl border border-gray-100/50",
        
        # Título y subtítulo
        div(
          class = "text-center mb-10",
          h2(class = "text-4xl font-bold text-clinicPurple tracking-tight", "Nueva Contraseña"),
          p(class = "text-gray-500 text-lg mt-3", "Define tu nueva clave de acceso")
        ),
        
        # El contenedor del formulario
        div(
          class = "flex flex-col space-y-6", 
          uiOutput(ns("form_reset"))
        ),
        
        # Enlace para volver abajo
        div(
          class = "mt-10 text-center",
          actionLink(
            ns("go_login"), 
            label = "Cancelar y volver",
            class = "text-lg text-gray-400 hover:text-red-500 transition-colors font-medium"
          )
        )
      )
    )
  )
}