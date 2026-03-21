mainServer <- function(id, current_user, user_logged, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Estado de la pestaña (inicial NULL para evitar arrastres)
    active_tab <- reactiveVal(NULL)
    
    observeEvent(user_logged(), {
      if (isTRUE(user_logged())) {
        active_tab("perfil")
      }
    }, ignoreInit = TRUE)
    
    output$welcome <- renderText({ 
      req(current_user())
      paste0("Bienvenido, ", current_user()$nombre) 
    })
    
    # ---------------- MENU ----------------
    output$dynamic_menu_items <- renderUI({
      req(current_user(), user_logged() == TRUE)
      
      opciones <- list(list(id = "perfil", label = "Mi Perfil"))
      
      if (current_user()$tipo_usuario %in% c('admin', 'recepcion')) {
        opciones <- append(opciones, list(
          list(id = "alta_usuarios", label = "Alta de Usuarios"),
          list(id = "buzon", label = "Buzón")
        ))
      }
      
      # Dentro de output$dynamic_menu_items en main_server_module.R
      if (current_user()$tipo_usuario == 'laboratorio') {
        opciones <- append(opciones, list(
          list(id = "pedidos_lab", label = "Pedidos Pendientes")
        ))
      }
      
      if (current_user()$tipo_usuario %in% c('admin', 'doctor')) {
        opciones <- append(opciones, list(
          list(id = "gestion_lab", label = "Gestión Laboratorios")
        ))
      }
      
      if (current_user()$tipo_usuario %in% c('admin', 'doctor', 'higienista')) {
        opciones <- append(opciones, list(
          list(id = "historial", label = "Historial Clínico")
        ))
      }
      
      if (current_user()$tipo_usuario %in% c('admin', 'recepcion', 'doctor', 'higienista', 'paciente')) {
        opciones <- append(opciones, list(
          list(id = "citas", label = "Citas / Agenda")
        ))
      }
      
      if (current_user()$tipo_usuario %in% c('paciente')) {
        opciones <- append(opciones, list(
          list(id = "justificantes", label = "Mis justificantes")
        ))
      }
      
      if (current_user()$tipo_usuario %in% c('admin', 'doctor')) {
        opciones <- append(opciones, list(
          list(id = "gestion_stock", label = "Stock Inteligente (IA)")
        ))
      }
      
      if (current_user()$tipo_usuario %in% c('admin', 'doctor')) {
        opciones <- append(opciones, list(
          list(id = "diagnostico_ia", label = "Diagnóstico IA")
        ))
      }
      
      lapply(opciones, function(opc) {
        es_activo <- if(!is.null(active_tab()) && active_tab() == opc$id) {
          " active bg-purple-active"
        } else ""
        
        actionLink(
          ns(paste0("btn_", opc$id)), 
          opc$label, 
          class = paste0("nav-custom-link", es_activo)
        )
      })
    })
    
    # ---------------- CLICKS ----------------
    observeEvent(input$btn_perfil, { active_tab("perfil") })
    observeEvent(input$btn_alta_usuarios, { active_tab("alta_usuarios") })
    observeEvent(input$btn_buzon, { active_tab("buzon") })
    observeEvent(input$btn_citas, { active_tab("citas") })
    observeEvent(input$btn_historial, { active_tab("historial") })
    observeEvent(input$btn_justificantes, { active_tab("justificantes") })
    observeEvent(input$btn_pedidos_lab, { active_tab("pedidos_lab") })
    observeEvent(input$btn_gestion_lab, { active_tab("gestion_lab") })
    observeEvent(input$btn_gestion_stock, { active_tab("gestion_stock") })
    observeEvent(input$btn_diagnostico_ia, { active_tab("diagnostico_ia") })
    
    # ---------------- CONTENIDO ----------------
    output$tab_content <- renderUI({
      req(user_logged() == TRUE)
      req(current_user())
      req(active_tab())
      
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
                               appointmentUI(ns("appointment_mod"))),
                 
                 "historial" = div(class="bg-white p-4 rounded shadow-sm border",
                                   historyUI(ns("history_mod"))),
                 
                 "justificantes" = div(class="bg-white p-4 rounded shadow-sm border",
                                       certificateUI(ns("justificantes"))),
                 
                 "pedidos_lab"   = div(class="bg-white p-4 rounded shadow-sm border",
                                       labUI(ns("pedidos_lab"))),
                 
                 "gestion_lab"   = div(class="bg-white p-4 rounded shadow-sm border",
                                       labUI(ns("pedidos_lab"))), 
                 
                 "gestion_stock" = div(class="bg-white p-4 rounded shadow-sm border", 
                                       stockUI(ns("stock_mod"))),
                 "diagnostico_ia" = div(class="bg-white p-4 rounded shadow-sm border", 
                                        diagnosticoUI(ns("diag_mod")))
          )
      )
    })
    
    outputOptions(output, "tab_content", suspendWhenHidden = FALSE)
    outputOptions(output, "dynamic_menu_items", suspendWhenHidden = FALSE)
    
    # ---------------- MÓDULOS ----------------
    profileServer("profile_mod", pool, current_user)
    userManagementServer("create_user_mod", pool, current_user)
    contactManagementServer("contact_mod", pool)
    appointmentServer("appointment_mod", pool, current_user)
    historyServer("history_mod", pool, current_user, active_tab) 
    certificateServer("justificantes",pool,current_user)
    labServer("pedidos_lab",pool,current_user)
    stockServer("stock_mod", pool, current_user)
    diagnosticoServer("diag_mod", pool, current_user)
    
    
    # ---------------- LOGOUT ----------------
    observeEvent(input$btn_logout, {
      user_logged(FALSE)
      current_user(NULL)
      active_tab(NULL) 
    })
    
  })
}