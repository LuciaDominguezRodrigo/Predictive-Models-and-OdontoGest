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
    
    # --- 2. LISTADO DE USUARIOS (REACTIVO) ---
    usuarios_df <- reactivePoll(3000, session,
                                checkFunc = function() {
                                  res <- dbGetQuery(pool, "SELECT SUM(banneado) + COUNT(*) FROM usuarios")
                                  return(res)
                                },
                                valueFunc = function() {
                                  dbGetQuery(pool, "SELECT id, usuario, nombre, tipo_usuario, banneado FROM usuarios WHERE tipo_usuario != 'admin'")
                                }
    )
    
    output$tabla_usuarios_container <- renderUI({
      df <- usuarios_df()
      if (nrow(df) == 0) return(p(class="text-muted italic", "No hay otros usuarios registrados."))
      
      tagList(
        lapply(1:nrow(df), function(i) {
          user <- df[i, ]
          esta_activo <- as.integer(user$banneado) == 1
          
          div(
            class = "d-flex align-items-center justify-content-between p-3 mb-2 border rounded shadow-sm bg-white",
            div(
              span(class = paste("badge me-3", if(esta_activo) "bg-success" else "bg-danger"), 
                   if(esta_activo) "Activo" else "Baneado"),
              span(class = "fw-bold text-dark", user$nombre),
              tags$small(class = "text-muted ms-2", paste0("(@", user$usuario, ")"))
            ),
            actionButton(
              ns(paste0("btn_toggle_", user$id)),
              label = if(esta_activo) "Banear" else "Desbanear",
              class = paste("btn btn-sm", if(esta_activo) "btn-outline-danger" else "btn-outline-success"),
              # IMPORTANTE: Esto envía el ID al servidor mediante JS para evitar duplicados
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
      
      #validamos que el usuario tenga el perfil correcto para acceder a la creación de usuarios
      user_info <- user_session()
      if (is.null(user_info) || !user_info$tipo_usuario %in% c("admin", "recepcion")) {
        showNotification("No tienes permisos para crear usuarios.", type = "error")
        return()
      }
      # Recogemos inputs
      nombre   <- input$nombre
      email    <- input$email
      usuario  <- input$usuario
      pass     <- input$password
      tel      <- input$telefono
      tipo     <- input$tipo_usuario
      
      # 1. Validación de campos vacíos
      if (nombre == "" || email == "" || usuario == "" || pass == "") {
        showNotification("Por favor, rellene todos los campos obligatorios.", type = "error")
        return()
      }
      
      # 2. Validación de Email (Regex estándar)
      email_valido <- grepl("^[^@]+@[^@]+\\.[^@]+$", email)
      if (!email_valido) {
        showNotification("El formato del correo electrónico no es válido.", type = "warning")
        return()
      }
      
      if (tipo == "admin") {
        showNotification("No se pueden crear administradores adicionales.", type = "error")
        return()
      }
      
      # 3. Verificar duplicados
      exists <- dbGetQuery(pool, "SELECT id FROM usuarios WHERE usuario = ? OR email = ?", 
                           params = list(usuario, email))
      
      if (nrow(exists) > 0) {
        showNotification("El nombre de usuario o email ya está registrado.", type = "warning")
        return()
      }
      
      # 4. HASHEO DE CONTRASEÑA (Usando bcrypt como en tu db_init)
      hash_pass <- bcrypt::hashpw(pass)
      
      tryCatch({
        poolWithTransaction(pool, function(conn) {
          # Insertar en tabla usuarios (incluyendo teléfono)
          dbExecute(conn, 
                    "INSERT INTO usuarios (usuario, nombre, password_hash, email, telefono, tipo_usuario, banneado) 
                 VALUES (?, ?, ?, ?, ?, ?, 1)",
                    params = list(usuario, nombre, hash_pass, email, tel, tipo)
          )
          
          # Si es paciente, insertar también en tabla pacientes
          if (tipo == "paciente") {
            dbExecute(conn, "INSERT INTO pacientes (nombre, email, telefono) VALUES (?, ?, ?)",
                      params = list(nombre, email, tel))
          }
        })
        
        showNotification("Usuario registrado exitosamente", type = "message")
        
        # Limpiar inputs
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