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
                     span(class = "text-muted small", "Panel de Control")
                 ),
                 
                 tags$button(
                   class = "navbar-toggler",
                   type = "button",
                   `data-bs-toggle` = "collapse",
                   `data-bs-target` = paste0("#", ns("navbarMainContent")),
                   span(class = "navbar-toggler-icon")
                 ),
                 
                 div(class = "collapse navbar-collapse d-lg-flex", id = ns("navbarMainContent"),                     
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