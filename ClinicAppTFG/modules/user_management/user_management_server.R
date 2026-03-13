userManagementServer <- function(id, pool, user_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- 1. LÓGICA DE PROTECCIÓN Y RENDERIZADO DE TABLA ---
    output$admin_panel_ui <- renderUI({
      req(user_session())
      # Solo admin o recepcion pueden ver esta área
      if (!user_session()$tipo_usuario %in% c("admin", "recepcion")) return(NULL)
      
      div(
        class = "admin-card mx-auto mt-5",
        style = "max-width: 900px;",
        div(
          class = "mb-4",
          h3(class = "fw-bold text-dark", "Gestión de Estado de Usuarios"),
          p(class = "text-muted", "Active, desactive o elimine usuarios de la plataforma.")
        ),
        uiOutput(ns("tabla_usuarios_container"))
      )
    })
    
    # --- 2. LISTADO DE USUARIOS (REACTIVO) ---
    usuarios_df <- reactivePoll(3000, session,
                                checkFunc = function() {
                                  # Detecta cambios en: número de filas, estado de baneo o cambio de foto
                                  res <- dbGetQuery(pool, "
          SELECT COUNT(*) + SUM(banneado) + IFNULL(SUM(LENGTH(foto_blob)), 0) 
          FROM usuarios
        ")
                                  return(res)
                                },
                                valueFunc = function() {
                                  dbGetQuery(pool, "SELECT id, usuario, nombre, tipo_usuario, banneado, foto_blob FROM usuarios")
                                }
    )
    
    output$tabla_usuarios_container <- renderUI({
      df <- usuarios_df()
      current_user <- user_session()
      
      # SEGURIDAD: Nadie puede verse a sí mismo para evitar autoborrado/autobaneo
      df <- df[df$id != current_user$id, ]
      
      # SEGURIDAD: Recepción solo ve pacientes
      if (current_user$tipo_usuario == "recepcion") {
        df <- df[df$tipo_usuario == "paciente", ]
      }
      
      if (nrow(df) == 0) return(p(class="text-muted italic", "No hay otros usuarios registrados bajo su nivel de acceso."))
      
      tagList(
        lapply(1:nrow(df), function(i) {
          user <- df[i, ]
          esta_activo <- as.integer(user$banneado) == 1
          
          # Procesamiento de Foto
          foto_data <- user$foto_blob
          img_src <- if (is.null(foto_data) || length(unlist(foto_data)) == 0) {
            "img/default_user.png"
          } else {
            base64enc::dataURI(unlist(foto_data), mime = "image/png")
          }
          
          div(
            class = "d-flex align-items-center justify-content-between p-3 mb-2 border rounded shadow-sm bg-white",
            div(
              class = "d-flex align-items-center",
              tags$img(src = img_src, class = "rounded-circle border me-3", style = "width: 50px; height: 50px; object-fit: cover;"),
              div(
                div(
                  span(class = "fw-bold text-dark", user$nombre),
                  tags$small(class = "text-muted ms-2", paste0("(@", user$usuario, ")"))
                ),
                span(class = paste("badge", if(esta_activo) "bg-success" else "bg-danger"), 
                     if(esta_activo) "Activo" else "Baneado")
              )
            ),
            
            div(
              # Botón de Baneo (Solo visible para Admin)
              if (current_user$tipo_usuario == "admin") {
                actionButton(
                  ns(paste0("btn_toggle_", user$id)),
                  label = if(esta_activo) "Banear" else "Desbanear",
                  class = paste("btn btn-sm me-2", if(esta_activo) "btn-outline-danger" else "btn-outline-success"),
                  onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("user_to_toggle"), user$id)
                )
              },
              
              # Botón de Eliminación (Admin y Recepción)
              actionButton(
                ns(paste0("btn_del_", user$id)),
                label = NULL,
                icon = icon("trash"),
                class = "btn btn-sm btn-outline-danger",
                onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("user_to_delete"), user$id)
              )
            )
          )
        })
      )
    })
    
    # --- 3. PROCESO DE BANEO/DESBANEO (Solo Admin) ---
    observeEvent(input$user_to_toggle, {
      id_target <- input$user_to_toggle
      res <- dbGetQuery(pool, "SELECT banneado FROM usuarios WHERE id = ?", params = list(id_target))
      if(nrow(res) == 0) return()
      
      nuevo_estado <- if(as.integer(res$banneado) == 1) 0 else 1
      
      tryCatch({
        dbExecute(pool, "UPDATE usuarios SET banneado = ? WHERE id = ?", params = list(nuevo_estado, id_target))
        showNotification(if(nuevo_estado == 1) "Usuario activado" else "Usuario deshabilitado", type = "message")
      }, error = function(e) showNotification("Error al cambiar estado", type = "error"))
    })
    
    # --- 4. PROCESO DE ELIMINACIÓN (Con Confirmación) ---
    observeEvent(input$user_to_delete, {
      id_target <- input$user_to_delete
      user_info <- dbGetQuery(pool, "SELECT nombre FROM usuarios WHERE id = ?", params = list(id_target))
      
      showModal(modalDialog(
        title = "Confirmar eliminación",
        paste0("¿Está seguro de que desea eliminar permanentemente a ", user_info$nombre, "? Esta acción no se puede deshacer."),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("confirm_delete_btn"), "Eliminar permanentemente", class = "btn-danger")
        ),
        easyClose = TRUE
      ))
    })
    
    observeEvent(input$confirm_delete_btn, {
      id_target <- input$user_to_delete
      removeModal()
      
      tryCatch({
        poolWithTransaction(pool, function(conn) {
          # Eliminar de tabla pacientes si aplica (por email para asegurar relación)
          dbExecute(conn, "DELETE FROM pacientes WHERE email = (SELECT email FROM usuarios WHERE id = ?)", params = list(id_target))
          # Eliminar de la tabla usuarios
          dbExecute(conn, "DELETE FROM usuarios WHERE id = ?", params = list(id_target))
        })
        showNotification("Usuario eliminado del sistema", type = "warning")
      }, error = function(e) showNotification("Error al eliminar", type = "error"))
    })
    
    # --- 5. LÓGICA DE ALTA DE USUARIO ---
    observeEvent(input$btn_save_user, {
      req(pool)
      user_info <- user_session()
      
      nombre <- input$nombre; email <- input$email; usuario <- input$usuario; pass <- input$password; tel <- input$telefono; tipo <- input$tipo_usuario
      if (!grepl("^[^@]+@[^@]+\\.[^@]+$", email)) {
        showNotification("El formato del correo electrónico no es válido", type = "error")
        return()
      }
      if (nombre == "" || email == "" || usuario == "" || pass == "") {
        showNotification("Por favor, rellene todos los campos obligatorios.", type = "error")
        return()
      }
      
      if (tipo == "admin") {
        showNotification("No se pueden crear administradores", type = "error")
        return()
      }
      
      if (user_info$tipo_usuario == "paciente") {
        showNotification("No tienes permisos para crear usuarios", type = "error")
        return()
      }
      
      # Bloqueo: Recepcionistas no crean otros roles
      if (user_info$tipo_usuario == "recepcion" && tipo != "paciente") {
        showNotification("No tienes permisos para crear este tipo de usuario.", type = "error")
        return()
      }
      
      exists <- dbGetQuery(pool, "SELECT id FROM usuarios WHERE email = ?", params = list(email))
      
      if (nrow(exists) > 0) {
        showNotification("Este usuario ya está registrado", type = "error")
        return()
      }
      
      hash_pass <- bcrypt::hashpw(pass)
      
      tryCatch({
        poolWithTransaction(pool, function(pool) {
          dbExecute(pool, 
                    "INSERT INTO usuarios (usuario, nombre, password_hash, email, telefono, tipo_usuario, banneado) 
             VALUES (?, ?, ?, ?, ?, ?, 1)",
                    params = list(usuario, nombre, hash_pass, email, tel, tipo))
          
          if (tipo == "paciente") {
            dbExecute(pool, "INSERT INTO pacientes (nombre, email, telefono) VALUES (?, ?, ?)",
                      params = list(nombre, email, tel))
          }
        })
        showNotification("Usuario registrado exitosamente")
        updateTextInput(session, "nombre", value = ""); updateTextInput(session, "email", value = "")
        updateTextInput(session, "usuario", value = ""); updateTextInput(session, "password", value = "")
      }, error = function(e) showNotification(paste("Error:", e$message), type = "error"))
    })
  })
}