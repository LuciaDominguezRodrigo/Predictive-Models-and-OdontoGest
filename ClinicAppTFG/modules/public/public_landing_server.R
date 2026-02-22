landingServer <- function(id, show_view, pool) { # <--- Añadido 'pool'
  moduleServer(id, function(input, output, session) {
    
    # Navegación al Login
    observeEvent(input$go_to_login, {
      updateQueryString("?page=login", mode = "push", session = session)
      show_view("LOGIN")
    })
    
    # Lógica del Formulario
    observeEvent(input$send_contact, {
      req(pool)
      # Validaciones simples
      if(input$contact_name == "" || input$contact_email == "" || input$contact_msg == "") {
        showNotification("Por favor, rellena todos los campos.", type = "warning")
        return()
      }
      
      # Guardar en Base de Datos
      tryCatch({
        dbExecute(pool, 
                  "INSERT INTO contacto (nombre, email, mensaje, leido) VALUES (?, ?, ?,?)", 
                  list(input$contact_name, input$contact_email, input$contact_msg, FALSE))
        
        # Feedback y limpieza
        showModal(modalDialog(
          title = "¡Mensaje Enviado!",
          "Gracias por contactar con Clínica Bienestar. Le responderemos pronto.",
          footer = modalButton("Cerrar"),
          easyClose = TRUE
        ))
        
        updateTextInput(session, "contact_name", value = "")
        updateTextInput(session, "contact_email", value = "")
        updateTextAreaInput(session, "contact_msg", value = "")
        
      }, error = function(e) {
        showNotification("Error al guardar el mensaje.", type = "error")
      })
    })
  })
}