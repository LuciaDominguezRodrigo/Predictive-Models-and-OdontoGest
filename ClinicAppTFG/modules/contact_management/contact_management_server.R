contactManagementServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Trigger para refrescar datos
    refresh <- reactiveVal(0)
    
    # --- Configuración SMTP ---
    # Nota: En producción, considera mover esto a variables de entorno
    smtp_server <- server(
      host = "smtp.gmail.com",
      port = 465,
      username = "clinicapptfg@gmail.com",
      password = "qffj vlrz kmzq jpwz"
    )
    
    # --- Datos de Mensajes ---
    mensajes_pendientes <- reactive({
      refresh()
      req(pool)
      dbGetQuery(pool, "SELECT * FROM contacto WHERE leido = FALSE ORDER BY fecha DESC")
    })
    
    mensajes_historial <- reactive({
      refresh()
      req(pool)
      dbGetQuery(pool, "SELECT * FROM contacto WHERE leido = TRUE ORDER BY fecha_respuesta DESC")
    })
    
    # --- UI: Lista de Pendientes ---
    output$mensajes_lista <- renderUI({
      df <- mensajes_pendientes()
      if (nrow(df) == 0) {
        return(div(class="text-center py-20 bg-gray-50 rounded-[2rem] border-2 border-dashed border-gray-200",
                   icon("clipboard-check", class="text-purple-300 text-6xl mb-4"),
                   h4(class="text-gray-400 text-xl font-bold", "¡Buzón vacío!")))
      }
      
      tagList(
        lapply(1:nrow(df), function(i) {
          msg <- df[i, ]
          div(class = "w-full p-6 md:p-8 mb-6 rounded-[1.5rem] border-l-[10px] border-purple-600 bg-white shadow-lg flex flex-col gap-4",
              div(class = "flex justify-between items-start",
                  div(
                    span(class = "block text-2xl font-black text-purple-900", msg$nombre),
                    span(class = "text-indigo-500 font-bold text-base", msg$email)
                  ),
                  span(class = "bg-purple-100 text-purple-700 text-xs font-black px-4 py-1 rounded-full", "NUEVO")
              ),
              div(class = "p-5 bg-purple-50 rounded-xl border border-purple-100 text-lg text-gray-700 italic",
                  paste0("\"", msg$mensaje, "\"")),
              div(class = "flex gap-3",
                  # Usamos IDs consistentes: prefijo_ID
                  actionButton(ns(paste0("btn_reply_", msg$id)), "Responder", 
                               onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("target_reply"), msg$id),
                               class = "flex-1 bg-purple-600 text-white font-bold py-3 rounded-xl shadow-md"),
                  actionButton(ns(paste0("btn_archive_", msg$id)), "Archivar", 
                               onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("target_archive"), msg$id),
                               class = "px-6 bg-gray-100 text-gray-500 font-bold py-3 rounded-xl")
              )
          )
        })
      )
    })
    
    # --- UI: Historial ---
    output$mensajes_archivados <- renderUI({
      df_arch <- mensajes_historial()
      if (nrow(df_arch) == 0) return(p(class="text-gray-400 text-center py-10", "No hay mensajes en el historial."))
      
      tagList(
        lapply(1:nrow(df_arch), function(i) {
          msg <- df_arch[i, ]
          div(class = "w-full p-6 mb-4 rounded-2xl bg-white border border-gray-200 shadow-sm",
              div(class = "flex flex-wrap justify-between mb-3 gap-2",
                  span(class = "text-xl font-bold text-gray-800", msg$nombre),
                  span(class = "text-gray-400 text-sm italic", paste("Respondido el:", msg$fecha_respuesta))
              ),
              div(class = "mb-3 text-gray-600 border-l-2 border-gray-100 pl-4",
                  p(class = "italic text-sm", msg$mensaje)),
              div(class = "p-4 bg-green-50 rounded-xl border-l-4 border-green-500",
                  span(class = "text-xs font-bold uppercase text-green-700", "Respuesta:"),
                  p(class = "text-gray-800", msg$respuesta))
          )
        })
      )
    })
    
    # --- Lógica de Acciones (Single Observers) ---
    
    # 1. ARCHIVAR
    observeEvent(input$target_archive, {
      id_to_archive <- input$target_archive
      dbExecute(pool, "UPDATE contacto SET leido = TRUE, fecha_respuesta = NOW(), respuesta = 'Archivado manualmente' WHERE id = ?", list(id_to_archive))
      refresh(refresh() + 1)
      showNotification("Mensaje archivado", type = "message")
    })
    
    # 2. ABRIR MODAL RESPUESTA
    rv_active_msg <- reactiveValues(id = NULL, email = NULL, nombre = NULL)
    
    observeEvent(input$target_reply, {
      id_to_reply <- input$target_reply
      # Obtenemos los datos del mensaje específico
      df <- mensajes_pendientes()
      msg_data <- df[df$id == id_to_reply, ]
      
      rv_active_msg$id <- msg_data$id
      rv_active_msg$email <- msg_data$email
      rv_active_msg$nombre <- msg_data$nombre
      
      showModal(modalDialog(
        title = div(class="text-2xl font-black text-purple-900", paste("Responder a", msg_data$nombre)),
        textAreaInput(ns("email_body"), NULL, rows = 6, width = "100%", placeholder = "Escribe tu respuesta..."),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("confirm_send"), "Enviar Respuesta", class = "bg-purple-600 text-white font-bold px-6 py-2 rounded-lg")
        ),
        size = "l", easyClose = FALSE
      ))
    })
    
    # 3. ENVIAR EMAIL Y ACTUALIZAR
    observeEvent(input$confirm_send, {
      req(input$email_body, rv_active_msg$id)
      
      tryCatch({
        email <- envelope() %>%
          from("clinicapptfg@gmail.com") %>%
          to(rv_active_msg$email) %>%
          subject("Re: Consulta ClinicApp") %>%
          text(input$email_body)
        
        smtp_server(email)
        
        dbExecute(pool, 
                  "UPDATE contacto SET leido = TRUE, respuesta = ?, fecha_respuesta = NOW() WHERE id = ?", 
                  list(input$email_body, rv_active_msg$id))
        
        removeModal()
        showNotification("Respuesta enviada correctamente", type = "message")
        refresh(refresh() + 1)
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
  })
}