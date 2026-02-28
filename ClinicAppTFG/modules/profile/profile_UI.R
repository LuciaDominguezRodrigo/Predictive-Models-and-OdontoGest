profileUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "container mt-5",
        div(class = "card shadow-sm border-0 mx-auto", style = "max-width: 700px;",
            div(class = "card-body p-5",
                
                # --- CABECERA: FOTO Y BOTONES DE CAMBIO ---
                div(class = "text-center mb-5",
                    uiOutput(ns("profile_img_container")),
                    div(class = "mt-3 d-flex justify-content-center gap-2", 
                        actionButton(ns("btn_edit_photo"), "Subir Archivo", class = "btn btn-sm btn-outline-secondary"),
                        actionButton(ns("btn_edit_photo_url"), "Desde URL", class = "btn btn-sm btn-outline-info")
                    )
                ),
                
                h4(class = "text-center mb-4 text-primary", "Información de mi Perfil"),
                hr(),
                
                # --- CUERPO: LISTA DE DATOS ---
                div(class = "py-3 d-flex justify-content-between align-items-center border-bottom",
                    div(tags$strong("Nombre: "), textOutput(ns("txt_nombre"), inline = TRUE)),
                    actionButton(ns("btn_edit_nom"), "Editar", class = "btn btn-outline-primary btn-sm")
                ),
                div(class = "py-3 d-flex justify-content-between align-items-center border-bottom",
                    div(tags$strong("Email: "), textOutput(ns("txt_email"), inline = TRUE)),
                    actionButton(ns("btn_edit_em"), "Editar", class = "btn btn-outline-primary btn-sm")
                ),
                div(class = "py-3 d-flex justify-content-between align-items-center border-bottom",
                    div(tags$strong("Teléfono: "), textOutput(ns("txt_tel"), inline = TRUE)),
                    actionButton(ns("btn_edit_tlf"), "Editar", class = "btn btn-outline-primary btn-sm")
                )
            )
        )
    )
  )
}