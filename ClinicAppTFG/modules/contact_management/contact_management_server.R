library(emayili)


contactManagementServer <- function(id, pool, .test_refresh = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- TEST REFRESH COUNTER (solo para tests) ---
    refresh <- reactiveVal(0)
    if (.test_refresh) {
      observe({
        mensajes_pendientes()   # fuerza la reevaluación
        refresh(refresh() + 1)  # contador visible en testServer()
      })
    }
    
    # --- 1. CONFIGURACIÓN SMTP ---
    smtp_server <- server(
      host = "smtp.gmail.com",
      port = 465,
      username = "clinicapptfg@gmail.com",
      password = "qffj vlrz kmzq jpwz"
    )
    
    # --- 2. DATOS REACTIVOS ---
    mensajes_pendientes <- reactivePoll(
      3000, session,
      checkFunc = function() {
        dbGetQuery(pool, 
                   "SELECT COUNT(*) FROM contacto WHERE leido = FALSE")[1, 1]
      },
      valueFunc = function() {
        dbGetQuery(pool,
                   "SELECT * FROM contacto WHERE leido = FALSE ORDER BY fecha DESC")
      }
    )
    
    mensajes_historial <- reactivePoll(
      3000, session,
      checkFunc = function() {
        dbGetQuery(pool,
                   "SELECT COUNT(*) FROM contacto WHERE leido = TRUE")[1, 1]
      },
      valueFunc = function() {
        dbGetQuery(pool,
                   "SELECT * FROM contacto WHERE leido = TRUE ORDER BY fecha_respuesta DESC")
      }
    )
    
    # --- 3. UI LISTA PENDIENTES ---
    output$mensajes_lista <- renderUI({
      df <- mensajes_pendientes()
      if (nrow(df) == 0) {
        return(div(
          class = "text-center py-5 bg-light rounded-4 border border-2 border-dashed text-muted",
          icon("clipboard-check", class = "display-1 mb-3 opacity-50"),
          h4(class = "fw-bold", "¡Buzón vacío!")
        ))
      }
      
      tagList(lapply(seq_len(nrow(df)), function(i) {
        msg <- df[i, ]
        
        div(
          class = "card mb-4 shadow-sm border-0 rounded-4",
          style = "
        border-left: 6px solid #7e22ce;
        box-shadow: 0 4px 18px rgba(0,0,0,0.08);
      ",
          
          div(
            class = "card-body p-4",
            
            # Header del mensaje
            div(
              class = "d-flex justify-content-between align-items-start mb-3",
              
              div(
                h4(class = 'fw-bold text-dark mb-0', msg$nombre),
                span(class = 'text-primary fw-bold small', msg$email)
              ),
              
              # Badge NUEVO en estilo morado moderno
              span(
                class = "badge px-3 py-2",
                style = "
              background: linear-gradient(135deg, #7e22ce, #6b21a8);
              color: white;
              font-weight: 600;
              border-radius: 12px;
              letter-spacing: .5px;
            ",
                "NUEVO"
              )
            ),
            
            # Menaje
            div(
              class = "p-3 bg-light rounded-3 border mb-3 text-secondary fst-italic",
              paste0('"', msg$mensaje, '"')
            ),
            
            # Botones morados
            div(
              class = "d-flex gap-2",
              
              # Botón responder
              actionButton(
                ns(paste0("btn_rep_", msg$id)), "Responder",
                onclick = sprintf(
                  "Shiny.setInputValue('%s', %d, {priority:'event'})",
                  ns("target_reply"), msg$id
                ),
                class = "btn text-white fw-bold py-2 flex-grow-1",
                style = "
              background-color: #7e22ce;
              border: none;
              border-radius: 10px;
            "
              ),
              
              # Botón archivar
              actionButton(
                ns(paste0("btn_arc_", msg$id)), "Archivar",
                onclick = sprintf(
                  'Shiny.setInputValue("%s", %d, {priority:"event"})',
                  ns("target_archive"), msg$id
                ),
                class = "btn fw-bold py-2",
                style = "
              color: #6b21a8;
              border: 2px solid #c084fc;
              border-radius: 10px;
              background-color: white;
            "
              )
            )
          )
        )
      }))
    })    
    # --- 4. UI HISTORIAL ---
    output$mensajes_archivados <- renderUI({
      df <- mensajes_historial()
      if (nrow(df) == 0)
        return(
          p(class = "text-muted text-center py-5", "No hay mensajes en el historial.")
        )
      
      tagList(lapply(seq_len(nrow(df)), function(i) {
        msg <- df[i, ]
        div(
          class = "card mb-3 border-light shadow-sm",
          div(
            class = "card-body p-4",
            div(
              class = "d-flex justify-content-between align-items-center mb-2",
              h5(class = "fw-bold text-dark mb-0", msg$nombre),
              tags$small(class = "text-muted",
                         paste("Respondido:", msg$fecha_respuesta))
            ),
            div(
              class = "mb-3 text-muted small border-start ps-3",
              p(class = "mb-0", msg$mensaje)
            ),
            div(
              class = "p-3 bg-success bg-opacity-10 border-start border-success border-4 rounded-3",
              tags$b(class = "text-success small d-block mb-1", "RESPUESTA:"),
              p(class = "text-dark mb-0", msg$respuesta)
            )
          )
        )
      }))
    })
    
    # --- 5. LÓGICA ---
    
    rv_active_msg <- reactiveValues(
      id = NULL,
      email = NULL,
      nombre = NULL
    )
    
    # ARCHIVAR
    observeEvent(input$target_archive, {
      req(pool)
      tryCatch({
        dbExecute(pool,
                  "UPDATE contacto 
                   SET leido = TRUE, fecha_respuesta = NOW(), respuesta = 'Archivado manualmente'
                   WHERE id = ?",
                  params = list(input$target_archive))
        
        showNotification("Mensaje movido al historial",
                         type = "message")
      },
      error = function(e) {
        showNotification("Error al archivar", type = "error")
      })
    })
    
    # ABRIR MODAL
    observeEvent(input$target_reply, {
      df <- mensajes_pendientes()
      msg <- df[df$id == input$target_reply, ]
      
      if (nrow(msg) == 0)
        return()
      
      rv_active_msg$id <- msg$id
      rv_active_msg$email <- msg$email
      rv_active_msg$nombre <- msg$nombre
      
      showModal(modalDialog(
        title = div(class = "text-primary fw-bold",
                    paste("Responder a", msg$nombre)),
        textAreaInput(
          ns("email_body"),
          "Mensaje de respuesta:",
          rows = 6,
          width = "100%",
          placeholder = "Escribe aquí..."
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("confirm_send"),
                       "Enviar y Archivar",
                       class = "btn btn-primary fw-bold px-4")
        ),
        size = "l",
        easyClose = FALSE
      ))
    })
    
    # ENVIAR EMAIL
    observeEvent(input$confirm_send, {
      req(rv_active_msg$id, input$email_body)
      
  

      shinyjs::disable("confirm_send")
      
      tryCatch({
        email_msg <- envelope() %>%
          from("clinicapptfg@gmail.com") %>%
          to(rv_active_msg$email) %>%
          subject("Re: Consulta ClinicApp") %>%
          text(input$email_body)
        
        # AQUÍ ESTABA EL FALLO (Añadido verbose = FALSE y cambiado a email_msg)
        smtp_server(email_msg, verbose = FALSE)
        
        # AQUÍ ESTABA EL SEGUNDO FALLO (Faltaba params = )
        dbExecute(pool,
                  "UPDATE contacto 
                   SET leido = TRUE, respuesta = ?, fecha_respuesta = NOW()
                   WHERE id = ?",
                  params = list(input$email_body, rv_active_msg$id))
        
        removeModal()
        showNotification("Respuesta enviada y mensaje archivado",
                         type = "message")
      },
      error = function(e) {
        # Si falla, volvemos a habilitar el botón
        shinyjs::enable("confirm_send")
        showNotification(
          paste("Error al enviar correo:", e$message),
          type = "error"
        )
      })
    })
    
    # --- EXPOSICIÓN PARA TESTS ---
    if (.test_refresh)
      return(list(refresh = refresh,
                  mensajes_pendientes = mensajes_pendientes,
                  rv_active_msg = rv_active_msg))
  })
}