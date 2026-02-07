userManagementUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # 1. Importamos Tailwind CSS y retocamos los inputs de Shiny
    tags$head(
      tags$script(src = "https://cdn.tailwindcss.com"),
      tags$style(HTML("
        /* Ajustes para que los inputs de Shiny no rompan el diseño */
        .shiny-input-container { margin-bottom: 0px !important; }
        .form-control { 
          border-radius: 0.75rem !important; 
          border: 1px solid #e5e7eb !important; 
          padding: 0.75rem 1rem !important;
          height: auto !important;
          box-shadow: none !important;
        }
        .form-control:focus {
          border-color: #6d28d9 !important; /* Color morado al enfocar */
          ring: 2px solid #ddd6fe !important;
        }
        .selectize-input { 
          border-radius: 0.75rem !important; 
          padding: 0.75rem 1rem !important; 
          border: 1px solid #e5e7eb !important;
        }
      "))
    ),
    
    # 2. El Contenedor Principal
    div(
      class = "min-h-screen bg-gray-50 py-12 px-4", # Fondo gris clarito para que resalte el blanco
      div(
        class = "max-w-2xl mx-auto bg-white p-10 rounded-3xl shadow-xl border border-gray-100",
        
        # Encabezado con un icono o detalle visual
        div(
          class = "mb-8",
          span(class = "bg-purple-100 text-purple-700 px-3 py-1 rounded-full text-xs font-bold uppercase tracking-wider", "Administración"),
          h3(class = "text-3xl font-extrabold text-gray-800 mt-2", "Alta de Usuario"),
          p(class = "text-gray-500", "Completa los datos para registrar personal o pacientes.")
        ),
        
        # Grid del Formulario
        div(
          class = "grid grid-cols-1 md:grid-cols-2 gap-x-6 gap-y-5",
          
          div(
            class = "col-span-2 md:col-span-1",
            tags$label("Nombre y Apellidos", class = "block text-sm font-semibold text-gray-700 mb-1"),
            textInput(ns("nombre"), NULL, placeholder = "Ej. Juan Pérez", width = "100%")
          ),
          
          div(
            class = "col-span-2 md:col-span-1",
            tags$label("Correo Electrónico", class = "block text-sm font-semibold text-gray-700 mb-1"),
            textInput(ns("email"), NULL, placeholder = "correo@ejemplo.com", width = "100%")
          ),
          
          div(
            class = "col-span-2 md:col-span-1",
            tags$label("Usuario de acceso", class = "block text-sm font-semibold text-gray-700 mb-1"),
            textInput(ns("usuario"), NULL, placeholder = "usuario123", width = "100%")
          ),
          
          div(
            class = "col-span-2 md:col-span-1",
            tags$label("Contraseña Temporal", class = "block text-sm font-semibold text-gray-700 mb-1"),
            passwordInput(ns("password"), NULL, placeholder = "••••••••", width = "100%")
          ),
          
          div(
            class = "col-span-2",
            tags$label("Rol en el sistema", class = "block text-sm font-semibold text-gray-700 mb-1"),
            selectInput(ns("tipo_usuario"), NULL, 
                        choices = c("Paciente" = "paciente", 
                                    "Recepción" = "recepcion", 
                                    "Doctor/Personal Clínico" = "doctor"), 
                        width = "100%")
          )
        ),
        
        # Botón de Acción
        div(
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