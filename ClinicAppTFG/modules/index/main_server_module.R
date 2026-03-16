mainServer <- function(id, current_user, user_logged, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Estado de la pestaña activa
    active_tab <- reactiveVal("perfil")
    
    output$welcome <- renderText({ 
      req(current_user())
      paste0("Bienvenido, ", current_user()$nombre) 
    })
    
    # 2. Renderizar los links del menú
    # 2. Renderizar los links del menú
    output$dynamic_menu_items <- renderUI({
      req(current_user())
      
      # Opción base para todos
      opciones <- list(list(id = "perfil", label = "Mi Perfil"))
      
      # Permisos para gestión de usuarios y buzón (solo admin y recepcion)
      if (current_user()$tipo_usuario %in% c('admin', 'recepcion')) {
        opciones <- append(opciones, list(
          list(id = "alta_usuarios", label = "Alta de Usuarios"),
          list(id = "buzon", label = "Buzón")
        ))
      }
      
      # Permisos para CITAS (admin, recepcion, doctor e higienista)
      # Nota: El usuario 'paciente' queda excluido según tu requerimiento
      if (current_user()$tipo_usuario %in% c('admin', 'recepcion', 'doctor', 'higienista', 'paciente')) {
        opciones <- append(opciones, list(
          list(id = "citas", label = "Citas / Agenda")
        ))
      }
      
      lapply(opciones, function(opc) {
        es_activo <- if(active_tab() == opc$id) " active bg-purple-active" else ""
        
        actionLink(ns(paste0("btn_", opc$id)), 
                   opc$label, 
                   class = paste0("nav-custom-link", es_activo))
      })
    })    
    # 3. Observadores para los clicks
    observeEvent(input$btn_perfil, { active_tab("perfil") })
    observeEvent(input$btn_alta_usuarios, { active_tab("alta_usuarios") })
    observeEvent(input$btn_buzon, { active_tab("buzon") })
    observeEvent(input$btn_citas, { active_tab("citas") })
    
    # 4. Renderizado del contenido central (MEJORADO)
    output$tab_content <- renderUI({
      # req() asegura que no intente renderizar si no hay login o pestaña
      req(user_logged(), current_user(), active_tab())
      
      tab <- active_tab()
      
      div(class = "tab-pane-effect",
          switch(tab,
                 "perfil" = div(class="bg-white p-4 rounded shadow-sm border", 
                                profileUI(ns("profile_mod"))),
                 "alta_usuarios" = div(class="bg-white p-4 rounded shadow-sm border", 
                                       userManagementUI(ns("create_user_mod"))),
                 "buzon" = div(class="bg-white p-4 rounded shadow-sm border", 
                               contactManagementUI(ns("contact_mod"))),
                 "citas" = div(class="bg-white p-4 rounded shadow-sm border", 
                               appointmentUI(ns("appointment_mod")))
          )
      )
    })
    
    # 5. CONFIGURACIÓN CRÍTICA: Forzar renderizado inmediato
    # Esto evita que el contenido "desaparezca" al loguear por primera vez
    outputOptions(output, "tab_content", suspendWhenHidden = FALSE)
    outputOptions(output, "dynamic_menu_items", suspendWhenHidden = FALSE)
    
    # 6. Inicialización de sub-módulos
    profileServer("profile_mod", pool, current_user)
    userManagementServer("create_user_mod", pool, current_user)
    contactManagementServer("contact_mod", pool)
    appointmentServer("appointment_mod", pool, current_user)
    
    # Logout
    observeEvent(input$btn_logout, {
      user_logged(FALSE)
      current_user(NULL)
    })
  })
}