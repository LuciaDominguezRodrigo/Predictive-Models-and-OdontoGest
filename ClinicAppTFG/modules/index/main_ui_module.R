mainUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "container mx-auto p-6",
        div(class = "flex justify-between items-center mb-8 pb-4 border-b",
            h2(class = "text-2xl font-bold text-gray-800", textOutput(ns("welcome"), inline = TRUE)),
            actionButton(ns("btn_logout"), "Cerrar sesión", 
                         class = "bg-red-500 text-white px-4 py-2 rounded-lg")),
        
        # Usamos un contenedor que Shiny siempre reconozca
        uiOutput(ns("dynamic_tabs"))
    )
  )
}

mainServer <- function(id, current_user, user_logged, pool) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$welcome <- renderText({ paste0("Bienvenido, ", current_user()$nombre) })
    output$user_info <- renderPrint({ current_user() })
    
    # Renderizamos TODO el tabsetPanel dinámicamente según el rol
    output$dynamic_tabs <- renderUI({
      req(current_user())
      
      # Lista básica de pestañas (la que todos ven)
      tabs <- list(
        tabPanel("Mi Perfil", value = "perfil",
                 div(class = "mt-6 p-4 bg-gray-50 rounded-xl",
                     verbatimTextOutput(ns("user_info"))))
      )
      
      # Si es admin o recepcion, AÑADIMOS la pestaña a la lista
      if (current_user()$tipo_usuario %in% c('admin', 'recepcion')) {
        tabs[[length(tabs) + 1]] <- tabPanel(
          "Alta de Usuarios", 
          value = "alta_usuarios",
          div(class = "mt-6", userManagementUI(ns("create_user_mod")))
        )
      }
      
      # Retornamos el tabsetPanel completo con la lista de pestañas generada
      do.call(tabsetPanel, c(list(id = ns("main_tabs")), tabs))
    })
    
    # Importante: el servidor del módulo debe estar fuera del renderUI
    userManagementServer("create_user_mod", pool)
    
    observeEvent(input$btn_logout,{
      user_logged(FALSE)
      current_user(NULL)
    })
  })
}