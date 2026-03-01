library(base64enc)

userManagementServer <- function(id, pool, user_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- 1. LÓGICA DE PROTECCIÓN Y RENDERIZADO DE TABLA ---
    output$admin_panel_ui <- renderUI({
      req(user_session())
      if (user_session()$tipo_usuario != "admin") return(NULL)
      
      div(
        class = "admin-card mx-auto mt-5",
        style = "max-width: 900px;",
        div(
          class = "mb-4",
          h3(class = "fw-bold text-dark", "Gestión de Estado de Usuarios"),
          p(class = "text-muted", "Active o desactive el acceso de los usuarios a la plataforma.")
        ),
        # Contenedor donde se volcará la lista de usuarios
        uiOutput(ns("tabla_usuarios_container"))
      )
    })
    
    # --- 2. LISTADO DE USUARIOS (REACTIVO CON FOTO_BLOB) ---
    # --- 2. LISTADO DE USUARIOS (REACTIVO CON DETECCIÓN DE CAMBIOS EN FOTO) ---
    usuarios_df <- reactivePoll(3000, session,
                                checkFunc = function() {
                                  # Esta consulta detecta cambios si:
                                  # 1. Cambia el número de usuarios (COUNT)
                                  # 2. Alguien es baneado/desbaneado (SUM(banneado))
                                  # 3. Alguien cambia su foto (SUM(LENGTH(foto_blob))) - ESTO ES LO NUEVO
                                  res <- dbGetQuery(pool, "
          SELECT COUNT(*) + SUM(banneado) + IFNULL(SUM(LENGTH(foto_blob)), 0) 
          FROM usuarios
        ")
                                  return(res)
                                },
                                valueFunc = function() {
                                  dbGetQuery(pool, "SELECT id, usuario, nombre, tipo_usuario, banneado, foto_blob FROM usuarios WHERE tipo_usuario != 'admin'")
                                }
    )
    
    output$tabla_usuarios_container <- renderUI({
      df <- usuarios_df()
      if (nrow(df) == 0) return(p(class="text-muted italic", "No hay otros usuarios registrados."))
      
      tagList(
        lapply(1:nrow(df), function(i) {
          user <- df[i, ]
          esta_activo <- as.integer(user$banneado) == 1
          
          # --- PROCESAMIENTO DE LA IMAGEN DE USUARIO ---
          foto_data <- user$foto_blob
          img_src <- if (is.null(foto_data) || length(unlist(foto_data)) == 0) {
            "img/default_user.png" # Imagen por defecto si no tiene
          } else {
            # Convertimos el binario de la DB a formato legible por el navegador
            base64enc::dataURI(unlist(foto_data), mime = "image/png")
          }
          
          div(
            class = "d-flex align-items-center justify-content-between p-3 mb-2 border rounded shadow-sm bg-white",
            div(
              class = "d-flex align-items-center",
              # Miniatura de la foto
              tags$img(src = img_src, 
                       class = "rounded-circle border me-3", 
                       style = "width: 50px; height: 50px; object-fit: cover;"),
              
              div(
                div(
                  span(class = "fw-bold text-dark", user$nombre),
                  tags$small(class = "text-muted ms-2", paste0("(@", user$usuario, ")"))
                ),
                span(class = paste("badge", if(esta_activo) "bg-success" else "bg-danger"), 
                     if(esta_activo) "Activo" else "Baneado")
              )
            ),
            
            # Botón de acción
            actionButton(
              ns(paste0("btn_toggle_", user$id)),
              label = if(esta_activo) "Banear" else "Desbanear",
              class = paste("btn btn-sm", if(esta_activo) "btn-outline-danger" else "btn-outline-success"),
              onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("user_to_toggle"), user$id)
            )
          )
        })
      )
    })
    
    # --- 3. PROCESO DE BANEO/DESBANEO ---
    observeEvent(input$user_to_toggle, {
      req(pool)
      id_target <- input$user_to_toggle
      req(id_target)
      
      res <- dbGetQuery(pool, "SELECT banneado FROM usuarios WHERE id = ?", params = list(id_target))
      if(nrow(res) == 0) return()
      
      nuevo_estado <- if(as.integer(res$banneado) == 1) 0 else 1
      
      tryCatch({
        dbExecute(pool, "UPDATE usuarios SET banneado = ? WHERE id = ?", 
                  params = list(nuevo_estado, id_target))
        
        showNotification(
          if(nuevo_estado == 1) "Usuario activado" else "Usuario deshabilitado",
          type = if(nuevo_estado == 1) "message" else "warning"
        )
      }, error = function(e) {
        showNotification("Error al actualizar la base de datos", type = "error")
      })
    })
    
    # --- 4. LÓGICA DE ALTA DE USUARIO ---
    observeEvent(input$btn_save_user, {
      req(pool)
      
      user_info <- user_session()
      if (is.null(user_info) || !user_info$tipo_usuario %in% c("admin", "recepcion")) {
        showNotification("No tienes permisos para crear usuarios.", type = "error")
        return()
      }
      
      nombre   <- input$nombre
      email    <- input$email
      usuario  <- input$usuario
      pass     <- input$password
      tel      <- input$telefono
      tipo     <- input$tipo_usuario
      
      if (nombre == "" || email == "" || usuario == "" || pass == "") {
        showNotification("Por favor, rellene todos los campos obligatorios.", type = "error")
        return()
      }
      
      email_valido <- grepl("^[^@]+@[^@]+\\.[^@]+$", email)
      if (!email_valido) {
        showNotification("El formato del correo electrónico no es válido.", type = "warning")
        return()
      }
      
      if (tipo == "admin") {
        showNotification("No se pueden crear administradores adicionales.", type = "error")
        return()
      }
      
      exists <- dbGetQuery(pool, "SELECT id FROM usuarios WHERE usuario = ? OR email = ?", 
                           params = list(usuario, email))
      
      if (nrow(exists) > 0) {
        showNotification("El nombre de usuario o email ya está registrado.", type = "warning")
        return()
      }
      
      hash_pass <- bcrypt::hashpw(pass)
      
      tryCatch({
        poolWithTransaction(pool, function(conn) {
          dbExecute(conn, 
                    "INSERT INTO usuarios (usuario, nombre, password_hash, email, telefono, tipo_usuario, banneado) 
                     VALUES (?, ?, ?, ?, ?, ?, 1)",
                    params = list(usuario, nombre, hash_pass, email, tel, tipo)
          )
          
          if (tipo == "paciente") {
            dbExecute(conn, "INSERT INTO pacientes (nombre, email, telefono) VALUES (?, ?, ?)",
                      params = list(nombre, email, tel))
          }
        })
        
        showNotification("Usuario registrado exitosamente", type = "message")
        
        # Limpiar campos
        updateTextInput(session, "nombre", value = "")
        updateTextInput(session, "email", value = "")
        updateTextInput(session, "usuario", value = "")
        updateTextInput(session, "telefono", value = "")
        updateTextInput(session, "password", value = "")
        
      }, error = function(e) {
        showNotification(paste("Error en el registro:", e$message), type = "error")
      })
    })
  })
}