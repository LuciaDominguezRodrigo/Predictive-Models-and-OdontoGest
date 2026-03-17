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
      user_id <- current_user()$id
      
      # 1. Hacemos una query específica para obtener el BLOB limpio de la DB
      # Usamos dbGetQuery para traer el dato justo en este momento
      foto_data <- tryCatch({
        res <- dbGetQuery(pool, "SELECT foto_blob FROM usuarios WHERE id = ?", params = list(user_id))
        
        # Accedemos a la primera fila, primera columna
        if (nrow(res) > 0) {
          dato <- res$foto_blob[[1]]
          # Si el driver lo devuelve como un objeto 'blob', lo convertimos a raw
          if (inherits(dato, "blob")) {
            as.vector(dato, mode = "raw")
          } else {
            dato
          }
        } else {
          NULL
        }
      }, error = function(e) {
        message("Error consultando foto: ", e$message)
        NULL
      })
      
      # 2. Lógica de renderizado final
      img_src <- if (!is.null(foto_data) && (is.raw(foto_data) || is.list(foto_data))) {
        # Si es una lista (a veces ocurre), intentamos aplanar
        final_raw <- if(is.list(foto_data)) unlist(foto_data) else foto_data
        
        # Verificación final antes de codificar
        if (length(final_raw) > 0) {
          base64enc::dataURI(final_raw, mime = "image/png")
        } else {
          "img/default_user.png"
        }
      } else {
        "img/default_user.png"
      }
      
      tags$img(src = img_src, class = "profile-photo mb-3")
    })
    
    
    
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