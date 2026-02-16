contactManagementServer <- function(id, pool) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0)
    
    # --- Configuración SMTP ---
    smtp <- server(
      host = "smtp.gmail.com",
      port = 465,
      username = "clinicapptfg@gmail.com",
      password = "qffj vlrz kmzq jpwz"
    )
    
    # --- Mensajes pendientes ---
    mensajes_data <- reactive({
      invalidateLater(5000)
      refresh()
      req(pool)
      dbGetQuery(pool, "SELECT * FROM contacto WHERE leido = FALSE ORDER BY fecha DESC")
    })
    
    # --- Renderizado de lista de pendientes ---
    output$mensajes_lista <- renderUI({
      df <- mensajes_data()
      if (nrow(df) == 0) {
        return(div(
          class="text-center py-20 bg-gray-50 rounded-[2rem] border-2 border-dashed border-gray-200",
          icon("clipboard-check", class="text-purple-300 text-6xl mb-4"),
          h4(class="text-gray-400 text-xl font-bold", "¡Buzón vacío! Todo gestionado")
        ))
      }
      
      div(class = "flex flex-col gap-6",
          lapply(1:nrow(df), function(i) {
            msg <- df[i, ]
            div(class = "w-full p-6 md:p-8 rounded-[1.5rem] border-l-[10px] border-purple-600 bg-white shadow-lg flex flex-col gap-4",
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
                    actionButton(ns(paste0("reply_", msg$id)), "Responder",
                                 class = "flex-1 bg-purple-600 text-white font-bold py-3 rounded-xl shadow-md"),
                    actionButton(ns(paste0("archive_", msg$id)), "Archivar",
                                 class = "px-6 bg-gray-100 text-gray-500 font-bold py-3 rounded-xl")
                )
            )
          })
      )
    })
    
    # --- Renderizado de historial ---
    output$mensajes_archivados <- renderUI({
      refresh()
      df_arch <- dbGetQuery(pool, "SELECT * FROM contacto WHERE leido = TRUE ORDER BY fecha_respuesta DESC")
      if (nrow(df_arch) == 0) return(p(class="text-gray-400 text-center py-10", "No hay mensajes en el historial."))
      
      div(class = "flex flex-col gap-4",
          lapply(1:nrow(df_arch), function(i) {
            msg <- df_arch[i, ]
            div(class = "w-full p-6 rounded-2xl bg-white border border-gray-200 shadow-sm",
                div(class = "flex flex-wrap justify-between mb-3 gap-2",
                    span(class = "text-xl font-bold text-gray-800", msg$nombre),
                    span(class = "text-gray-400 text-sm italic",
                         paste("Respondido el:", if(!is.na(msg$fecha_respuesta)) msg$fecha_respuesta else "Fecha desconocida"))
                ),
                div(class = "mb-3 text-gray-600 border-l-2 border-gray-100 pl-4",
                    span(class = "text-xs font-bold uppercase text-gray-400", "Consulta original:"),
                    p(class = "italic", msg$mensaje)),
                div(class = "p-4 bg-green-50 rounded-xl border-l-4 border-green-500",
                    span(class = "text-xs font-bold uppercase text-green-700", "Respuesta enviada:"),
                    p(class = "text-gray-800", if(is.na(msg$respuesta) || msg$respuesta == "") "Archivado sin respuesta" else msg$respuesta))
            )
          })
      )
    })
    
    rv_current_msg <- reactiveValues(id = NULL, email = NULL, nombre = NULL)
    
    # --- Observadores dinámicos seguros ---
    # --- Observadores dinámicos seguros sin duplicados ---
    observe({
      df <- mensajes_data()
      ids_actuales <- df$id
      
      lapply(ids_actuales, function(id_local) {
        # --- Archivar ---
        btn_archive <- paste0("archive_", id_local)
        # Solo crear el observer si no existe
        if (!is.null(input[[btn_archive]]) && !exists(btn_archive, envir = .GlobalEnv)) {
          assign(btn_archive, TRUE, envir = .GlobalEnv)
          observeEvent(input[[btn_archive]], {
            dbExecute(pool,
                      "UPDATE contacto SET leido = TRUE, fecha_respuesta = NOW(), respuesta = 'Archivado manualmente' WHERE id = ?",
                      list(id_local))
            refresh(refresh() + 1)
            showNotification("Mensaje movido al historial.", type = "message")
          }, ignoreInit = TRUE)
        }
        
        # --- Responder ---
        btn_reply <- paste0("reply_", id_local)
        if (!is.null(input[[btn_reply]]) && !exists(btn_reply, envir = .GlobalEnv)) {
          assign(btn_reply, TRUE, envir = .GlobalEnv)
          observeEvent(input[[btn_reply]], {
            row <- df[df$id == id_local, ]
            req(row$email)
            rv_current_msg$id <- id_local
            rv_current_msg$email <- as.character(row$email) # Siempre string
            rv_current_msg$nombre <- row$nombre
            
            showModal(modalDialog(
              title = div(class="text-2xl font-black text-purple-900", paste("Responder a", row$nombre)),
              textAreaInput(ns("email_body"), NULL, rows = 8, width = "100%", placeholder = "Escribe aquí la respuesta..."),
              footer = tagList(
                modalButton("Cancelar"),
                actionButton(ns("confirm_send"), "Enviar y Archivar",
                             class = "bg-purple-600 text-white font-bold px-6 py-2 rounded-lg")
              ),
              size = "l", easyClose = FALSE
            ))
          }, ignoreInit = TRUE)
        }
      })
    })
    
    # --- Envío final ---
    observeEvent(input$confirm_send, {
      req(input$email_body, rv_current_msg$id, rv_current_msg$email)
      c_body <- as.character(input$email_body)
      recipient <- as.character(rv_current_msg$email)
      
      tryCatch({
        email <- envelope() %>%
          from("clinicapptfg@gmail.com") %>%
          to(recipient) %>%
          subject("Re: Consulta ClinicApp") %>%
          text(c_body)
        
        smtp(email)
        
        dbExecute(pool,
                  "UPDATE contacto SET leido = TRUE, respuesta = ?, fecha_respuesta = NOW() WHERE id = ?",
                  list(c_body, rv_current_msg$id))
        
        removeModal()
        showNotification("Respuesta enviada y guardada.", type = "message")
        refresh(refresh() + 1)
      }, error = function(e) {
        showNotification(paste("Error al enviar:", e$message), type = "error")
      })
    })
    
    return(list(
      rv_current_msg = rv_current_msg
    ))
  })
}
