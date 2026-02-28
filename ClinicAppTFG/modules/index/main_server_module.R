mainServer <- function(id, current_user, user_logged, pool) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # 1. Estado de la pestaña activa (Reactive Value)
    active_tab <- reactiveVal("perfil")
    
    output$welcome <- renderText({ paste0("Bienvenido, ", current_user()$nombre) })
    
    # 2. Renderizar los links del menú (Evita el aviso de Navigation Containers)
    output$dynamic_menu_items <- renderUI({
      req(current_user())
      
      # Definimos los botones disponibles según el rol
      opciones <- list(list(id = "perfil", label = "Mi Perfil"))
      
      if (current_user()$tipo_usuario %in% c('admin', 'recepcion')) {
        opciones[[2]] <- list(id = "alta_usuarios", label = "Alta de Usuarios")
        opciones[[3]] <- list(id = "buzon", label = "Buzón")
      }
      
      # Creamos los links dinámicamente
      lapply(opciones, function(opc) {
        es_activo <- if(active_tab() == opc$id) " active bg-purple-active" else ""
        
        actionLink(ns(paste0("btn_", opc$id)), 
                   opc$label, 
                   class = paste0("nav-custom-link", es_activo))
      })
    })
    
    # 3. Observadores para los clicks (Cambian la variable active_tab)
    observeEvent(input$btn_perfil, { active_tab("perfil") })
    observeEvent(input$btn_alta_usuarios, { active_tab("alta_usuarios") })
    observeEvent(input$btn_buzon, { active_tab("buzon") })
    
    # 4. Renderizado del contenido central
    output$tab_content <- renderUI({
      req(active_tab())
      
      # Envolvemos en un div con la clase de animación fadeIn
      div(class = "tab-pane-effect",
          switch(active_tab(),
                 "perfil" = div(class="bg-white p-4 rounded shadow-sm border", profileUI(ns("profile_mod"))),
                 "alta_usuarios" = div(class="bg-white p-4 rounded shadow-sm border", userManagementUI(ns("create_user_mod"))),
                 "buzon" = div(class="bg-white p-4 rounded shadow-sm border", contactManagementUI(ns("contact_mod")))
          )
      )
    })
    
    # 5. Inicialización de sub-módulos
    profileServer("profile_mod", pool, current_user)
    userManagementServer("create_user_mod", pool, current_user)
    contactManagementServer("contact_mod", pool)
    
    # Logout
    observeEvent(input$btn_logout,{
      user_logged(FALSE)
      current_user(NULL)
    })
  })
}