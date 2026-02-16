contactManagementUI <- function(id) {
  ns <- NS(id)
  div(class = "w-full min-h-screen bg-purple-50 p-4 md:p-8",
      div(class = "w-full bg-white shadow-2xl rounded-[2.5rem] overflow-hidden border border-purple-100",
          
          # Encabezado corregido (Texto Blanco)
          div(class = "bg-gradient-to-r from-purple-700 via-purple-800 to-indigo-900 p-8 md:p-12 text-white",
              div(class = "flex flex-col md:flex-row justify-between items-center gap-6",
                  div(class = "text-center md:text-left",
                      h3(class = "text-4xl md:text-5xl font-black tracking-tight text-white", "Buzón de Consultas"),
                      p(class = "text-purple-100 mt-2 text-lg font-light", "Gestión y seguimiento de atención al paciente.")
                  ),
                  icon("paper-plane", class = "text-6xl md:text-8xl opacity-20 hidden md:block")
              )
          ),
          
          # Sistema de Pestañas (Tabs)
          div(class = "p-4 md:p-10",
              tabsetPanel(
                id = ns("buzon_tabs"),
                type = "pills", # Estilo moderno de botones
                
                # Pestaña 1: Pendientes
                tabPanel(
                  title = div(class="px-4 py-2 font-bold", "📥 Pendientes"),
                  div(class = "mt-8",
                      uiOutput(ns("mensajes_lista"))
                  )
                ),
                
                # Pestaña 2: Historial
                tabPanel(
                  title = div(class="px-4 py-2 font-bold", "📂 Historial"),
                  div(class = "mt-8",
                      uiOutput(ns("mensajes_archivados"))
                  )
                )
              )
          )
      )
  )
}