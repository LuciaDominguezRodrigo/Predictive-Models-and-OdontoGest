userManagementServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$btn_save_user, {
      req(input$nombre, input$usuario, input$password, input$tipo_usuario)
      
      # Validación de seguridad: no permitir crear admins
      if (input$tipo_usuario == "admin") {
        showNotification("No se pueden crear perfiles de Administrador.", type = "error")
        return()
      }
      
      # Comprobar si existe el usuario
      exists <- dbGetQuery(pool, "SELECT id FROM usuarios WHERE usuario = ?", params = list(input$usuario))
      
      if (nrow(exists) > 0) {
        showNotification("Error: El nombre de usuario ya está registrado.", type = "warning")
        return()
      }
      
      # Hashear password e insertar usando poolWithTransaction
      hash <- hashpw(input$password)
      
      tryCatch({
        # USAR poolWithTransaction en lugar de dbWithTransaction
        poolWithTransaction(pool, function(conn) {
          
          # 1. Insertar en tabla usuarios
          dbExecute(conn, 
                    "INSERT INTO usuarios (usuario, nombre, password_hash, email, tipo_usuario) VALUES (?, ?, ?, ?, ?)",
                    params = list(input$usuario, input$nombre, hash, input$email, input$tipo_usuario)
          )
          
          # 2. Si es paciente, creamos también su registro en la tabla pacientes
          if (input$tipo_usuario == "paciente") {
            dbExecute(conn, 
                      "INSERT INTO pacientes (nombre, email) VALUES (?, ?)",
                      params = list(input$nombre, input$email)
            )
          }
        })
        
        showNotification("Usuario y ficha creados correctamente", type = "message")
        
        # Limpiar formulario
        updateTextInput(session, "usuario", value = "")
        updateTextInput(session, "nombre", value = "")
        updateTextInput(session, "password", value = "")
        updateTextInput(session, "email", value = "")
        
      }, error = function(e) {
        showNotification(paste("Error en la base de datos:", e$message), type = "error")
      })
    })
  })
}