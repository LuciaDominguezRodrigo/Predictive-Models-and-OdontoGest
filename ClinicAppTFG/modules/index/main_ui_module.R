mainUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "main_module.css")
    ),
    
    # Navbar estándar (sin scripts de scroll)
    tags$nav(class = "navbar navbar-expand-lg navbar-light navbar-custom shadow-sm",
             div(class = "container-fluid px-lg-4",
                 div(class = "navbar-brand d-flex flex-column",
                     span(class = "fw-bold text-purple mb-0 h4", textOutput(ns("welcome"), inline = TRUE)),
                     span(class = "text-muted small", "Panel de Control v2.0")
                 ),
                 
                 tags$button(
                   class = "navbar-toggler",
                   type = "button",
                   `data-bs-toggle` = "collapse",
                   `data-bs-target` = paste0("#", ns("navbarMainContent")),
                   span(class = "navbar-toggler-icon")
                 ),
                 
                 div(class = "collapse navbar-collapse", id = ns("navbarMainContent"),
                     div(class = "navbar-nav mx-auto mb-2 mb-lg-0",
                         uiOutput(ns("dynamic_menu_items"))
                     ),
                     
                     div(class = "d-flex",
                         actionButton(ns("btn_logout"), "Cerrar sesión", 
                                      class = "btn btn-outline-danger btn-sm rounded-pill px-4 mt-2 mt-lg-0")
                     )
                 )
             )
    ),
    
    div(class = "container py-4 content-wrapper",
        uiOutput(ns("tab_content"))
    )
  )
}

mainServer <- function(id, current_user, user_logged, pool) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Estado de la pestaña activa
    active_tab <- reactiveVal("perfil")
    
    output$welcome <- renderText({ 
      req(current_user())
      paste0("Bienvenido, ", current_user()$nombre) 
    })
    
    # Renderizar links del menú
    output$dynamic_menu_items <- renderUI({
      req(current_user())
      
      opciones <- list(list(id = "perfil", label = "Mi Perfil"))
      
      if (current_user()$tipo_usuario %in% c('admin', 'recepcion')) {
        opciones[[2]] <- list(id = "alta_usuarios", label = "Alta de Usuarios")
        opciones[[3]] <- list(id = "buzon", label = "Buzón")
      }
      
      lapply(opciones, function(opc) {
        es_activo <- if(active_tab() == opc$id) " active bg-purple-active" else ""
        
        actionLink(ns(paste0("btn_", opc$id)), 
                   opc$label, 
                   class = paste0("nav-custom-link", es_activo))
      })
    })
    
    # Observadores de clicks
    observeEvent(input$btn_perfil, { active_tab("perfil") })
    observeEvent(input$btn_alta_usuarios, { active_tab("alta_usuarios") })
    observeEvent(input$btn_buzon, { active_tab("buzon") })
    
    # Contenido dinámico
    output$tab_content <- renderUI({
      req(active_tab())
      
      div(class = "tab-pane-effect",
          switch(active_tab(),
                 "perfil" = div(class="bg-white p-4 rounded shadow-sm border", profileUI(ns("profile_mod"))),
                 "alta_usuarios" = div(class="bg-white p-4 rounded shadow-sm border", userManagementUI(ns("create_user_mod"))),
                 "buzon" = div(class="bg-white p-4 rounded shadow-sm border", contactManagementUI(ns("contact_mod")))
          )
      )
    })
    
    # Inicialización de sub-módulos
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