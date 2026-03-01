profileUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    # Cargar el CSS propio del módulo
    tags$link(rel = "stylesheet", type = "text/css", href = "profile_module.css"),
    
    div(class = "container mt-5",
        div(class = "profile-card mx-auto", style = "max-width: 700px;",
            
            # FOTO + BOTONES
            div(class = "text-center mb-4",
                uiOutput(ns("profile_img_container")),
                div(class = "mt-3 d-flex justify-content-center gap-2", 
                    actionButton(ns("btn_edit_photo"), "Subir Archivo", class = "btn btn-purple-outline btn-sm"),
                    actionButton(ns("btn_edit_photo_url"), "Desde URL", class = "btn btn-purple-outline btn-sm")
                )
            ),
            
            h4(class = "text-center profile-title", "Información de mi Perfil"),
            
            div(class = "profile-row",
                div(tags$strong("Nombre:"), textOutput(ns("txt_nombre"), inline = TRUE)),
                actionButton(ns("btn_edit_nom"), "Editar", class = "btn btn-purple-outline btn-sm")
            ),
            
            div(class = "profile-row",
                div(tags$strong("Email:"), textOutput(ns("txt_email"), inline = TRUE)),
                actionButton(ns("btn_edit_em"), "Editar", class = "btn btn-purple-outline btn-sm")
            ),
            
            div(class = "profile-row",
                div(tags$strong("Teléfono:"), textOutput(ns("txt_tel"), inline = TRUE)),
                actionButton(ns("btn_edit_tlf"), "Editar", class = "btn btn-purple-outline btn-sm")
            )
        )
    )
  )
}