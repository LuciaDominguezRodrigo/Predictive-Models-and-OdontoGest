mainUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Cambiamos "container mx-auto" por "w-full px-8" para ocupar todo el ancho
    div(class = "w-full px-8 py-6", 
        div(class = "flex justify-between items-center mb-8 pb-4 border-b",
            h2(class = "text-3xl font-bold text-gray-800", textOutput(ns("welcome"), inline = TRUE)),
            actionButton(ns("btn_logout"), "Cerrar sesión", 
                         class = "bg-red-500 hover:bg-red-600 text-white px-6 py-2 rounded-xl shadow-md transition-all")),
        
        # El contenedor de las pestañas ahora se expandirá al ancho total del div padre
        uiOutput(ns("dynamic_tabs"))
    )
  )
}

mainServer <- function(id, current_user, user_logged, pool) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$welcome <- renderText({ paste0("Bienvenido, ", current_user()$nombre) })
    
    output$dynamic_tabs <- renderUI({
      req(current_user())
      
      # 1. Pestaña Perfil (General)
      tabs <- list(
        tabPanel("Mi Perfil", value = "perfil",
                 div(class = "mt-6 p-4 bg-gray-50 rounded-xl",
                     verbatimTextOutput(ns("user_info"))))
      )
      
      # 2. Pestaña Usuarios (Admin y Recepción)
      if (current_user()$tipo_usuario %in% c('admin', 'recepcion')) {
        tabs[[length(tabs) + 1]] <- tabPanel(
          "Alta de Usuarios", value = "alta_usuarios",
          div(class = "mt-6", userManagementUI(ns("create_user_mod")))
        )
      }
      
      # 3. Pestaña Buzón (Solo Admin)
      if (current_user()$tipo_usuario %in% c('admin', 'recepcion')) {
        tabs[[length(tabs) + 1]] <- tabPanel(
          "Buzón", value = "buzon",
          div(class = "mt-6", contactManagementUI(ns("contact_mod")))
        )
      }
      
      # 4. Generar el panel con todas las pestañas acumuladas
      do.call(tabsetPanel, c(list(id = ns("main_tabs")), tabs))
    })
    
    # Servidores de módulos
    userManagementServer("create_user_mod", pool, current_user)
    contactManagementServer("contact_mod", pool)
    observeEvent(input$btn_logout,{
      user_logged(FALSE)
      current_user(NULL)
    })
  })
}