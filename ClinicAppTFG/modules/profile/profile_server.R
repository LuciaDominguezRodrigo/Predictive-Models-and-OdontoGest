library(base64enc)
library(httr)

profileServer <- function(id, pool, current_user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ============================================================
    # 1. RENDERIZADO DE LA INTERFAZ (FOTO Y TEXTOS)
    # ============================================================
    
    # Renderizado de la foto circular en la parte superior
    output$profile_img_container <- renderUI({
      req(current_user())
      blob <- current_user()$foto_blob
      
      # unlist() es necesario para que base64enc procese correctamente el vector raw
      img_src <- if (is.null(blob) || length(unlist(blob)) == 0) {
        "img/default_user.png" 
      } else {
        base64enc::dataURI(unlist(blob), mime = "image/png")
      }
      
      tags$img(src = img_src, 
               class = "rounded-circle border shadow-sm mb-3", 
               style = "width: 160px; height: 160px; object-fit: cover; display: block; margin: 0 auto;")
    })
    
    # Textos que se actualizan automáticamente al cambiar el usuario
    output$txt_nombre <- renderText({ req(current_user()); current_user()$nombre })
    output$txt_email  <- renderText({ req(current_user()); current_user()$email })
    output$txt_tel    <- renderText({ req(current_user()); current_user()$telefono })
    
    # ============================================================
    # 2. DISPARADORES DE MODALES (APERTURA)
    # ============================================================
    
    # Modal para subir archivo local
    observeEvent(input$btn_edit_photo, {
      showModal(modalDialog(
        title = "Actualizar Foto de Perfil",
        fileInput(ns("new_photo_input"), "Selecciona una imagen (JPG/PNG)", accept = c('image/png', 'image/jpeg')),
        footer = tagList(modalButton("Cancelar"), actionButton(ns("do_save_photo"), "Guardar")),
        easyClose = TRUE
      ))
    })
    
    # Modal para cargar desde URL
    observeEvent(input$btn_edit_photo_url, {
      showModal(modalDialog(
        title = "Cargar Foto desde URL",
        textInput(ns("new_photo_url"), "Pega el enlace de la imagen:", placeholder = "https://..."),
        footer = tagList(modalButton("Cancelar"), actionButton(ns("do_save_photo_url"), "Cargar")),
        easyClose = TRUE
      ))
    })
    
    # Modales de edición de datos personales
    observeEvent(input$btn_edit_nom, {
      showModal(modalDialog(title = "Editar Nombre", 
                            textInput(ns("input_new_nom"), "Nombre:", value = current_user()$nombre),
                            footer = tagList(modalButton("Cancelar"), actionButton(ns("do_save_nom"), "Actualizar"))))
    })
    
    observeEvent(input$btn_edit_em, {
      showModal(modalDialog(title = "Editar Email", 
                            textInput(ns("input_new_em"), "Email:", value = current_user()$email),
                            footer = tagList(modalButton("Cancelar"), actionButton(ns("do_save_em"), "Actualizar"))))
    })
    
    observeEvent(input$btn_edit_tlf, {
      showModal(modalDialog(title = "Editar Teléfono", 
                            textInput(ns("input_new_tlf"), "Teléfono:", value = current_user()$telefono),
                            footer = tagList(modalButton("Cancelar"), actionButton(ns("do_save_tlf"), "Actualizar"))))
    })
    
    # ============================================================
    # 3. LÓGICA DE GUARDADO (ACCIONES)
    # ============================================================
    
    # Guardar Foto desde Archivo Local
    observeEvent(input$do_save_photo, {
      req(input$new_photo_input)
      tryCatch({
        blob_data <- readBin(input$new_photo_input$datapath, "raw", file.info(input$new_photo_input$datapath)$size)
        
        # Guardar en base de datos usando list(list(blob)) para evitar errores de tipo
        dbExecute(pool, "UPDATE usuarios SET foto_blob = ? WHERE id = ?", list(list(blob_data), current_user()$id))
        
        # Actualización de la sesión reactiva: list(blob) evita el error de filas
        u <- current_user()
        u$foto_blob <- list(blob_data) 
        current_user(u)
        
        removeModal()
        showNotification("Foto actualizada correctamente", type = "message")
      }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
    })
    
    # Guardar Foto desde URL
    observeEvent(input$do_save_photo_url, {
      req(input$new_photo_url)
      tryCatch({
        res <- GET(input$new_photo_url)
        if (status_code(res) != 200) stop("No se pudo descargar la imagen de la URL.")
        
        blob_data <- content(res, "raw")
        dbExecute(pool, "UPDATE usuarios SET foto_blob = ? WHERE id = ?", list(list(blob_data), current_user()$id))
        
        u <- current_user()
        u$foto_blob <- list(blob_data)
        current_user(u)
        
        removeModal()
        showNotification("Foto cargada desde URL", type = "message")
      }, error = function(e) showNotification("URL no válida o inaccesible", type = "error"))
    })
    
    # Guardar cambios de texto (Nombre, Email, Teléfono)
    observeEvent(input$do_save_nom, {
      req(input$input_new_nom)
      dbExecute(pool, "UPDATE usuarios SET nombre = ? WHERE id = ?", list(input$input_new_nom, current_user()$id))
      u <- current_user()
      u$nombre <- input$input_new_nom
      current_user(u)
      removeModal()
    })
    
    observeEvent(input$do_save_em, {
      req(input$input_new_em)
      dbExecute(pool, "UPDATE usuarios SET email = ? WHERE id = ?", list(input$input_new_em, current_user()$id))
      u <- current_user()
      u$email <- input$input_new_em
      current_user(u)
      removeModal()
    })
    
    observeEvent(input$do_save_tlf, {
      req(input$input_new_tlf)
      dbExecute(pool, "UPDATE usuarios SET telefono = ? WHERE id = ?", list(input$input_new_tlf, current_user()$id))
      u <- current_user()
      u$telefono <- input$input_new_tlf
      current_user(u)
      removeModal()
    })
    
  })
}