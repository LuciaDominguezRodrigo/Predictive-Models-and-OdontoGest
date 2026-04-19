# ==============================================================================
# PROYECTO: ClinicAppTFG | MÓDULO: lab_server.R
# DESCRIPCIÓN: Gestión de trabajos con flujo circular y UI mejorada.
# ==============================================================================

labServer <- function(id, pool, current_user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    refresh <- reactiveVal(0)
    
    # 1. UI: Botón de Nuevo Pedido (Solo Médicos/Admin)
    output$create_btn_container <- renderUI({
      req(current_user())
      if (current_user()$tipo_usuario %in% c('admin', 'doctor')) {
        actionButton(ns("btn_nuevo_pedido"), "Nuevo Trabajo Protésico", 
                     class = "btn-purple", icon = icon("plus"))
      }
    })
    
    # 2. Lógica de creación (Doctor)
    observeEvent(input$btn_nuevo_pedido, {
      labs <- dbGetQuery(pool, "SELECT id, nombre FROM usuarios WHERE tipo_usuario = 'laboratorio'")
      showModal(modalDialog(
        title = "Nuevo Pedido al Laboratorio",
        selectInput(ns("lab_id"), "Seleccionar Laboratorio", setNames(labs$id, labs$nombre)),
        textInput(ns("paciente"), "Nombre del Paciente"),
        textAreaInput(ns("desc"), "Instrucciones del trabajo", rows = 3, placeholder = "Ej: Corona circonio pieza 46, color A3..."),
        footer = tagList(
          modalButton("Cancelar"), 
          actionButton(ns("save_p"), "Enviar Pedido", class = "btn-purple")
        )
      ))
    })
    
    observeEvent(input$save_p, {
      req(input$paciente, input$desc)
      dbExecute(pool, "INSERT INTO pedidos_laboratorio (doctor_id, laboratorio_id, paciente_nombre, descripcion, estado) VALUES (?, ?, ?, ?, 'pendiente')",
                params = list(current_user()$id, input$lab_id, input$paciente, input$desc))
      removeModal(); refresh(refresh() + 1)
      showNotification("Pedido enviado al laboratorio", type = "message")
    })
    
    # 3. TABLA DE PEDIDOS
    output$tabla_pedidos <- renderTable({
      refresh()
      req(current_user())
      
      query <- "SELECT p.id, u.nombre as Proveedor, p.paciente_nombre as Paciente, 
                 p.descripcion as Detalle, p.estado, p.tipo_pedido
          FROM pedidos_laboratorio p 
          JOIN usuarios u ON p.laboratorio_id = u.id"

      # Sustituir la lógica de filtrado por esta
      if(current_user()$tipo_usuario == 'laboratorio') {
          # Solo prótesis para el protésico
          df <- dbGetQuery(pool, paste(query, "WHERE p.laboratorio_id = ? AND p.tipo_pedido = 'protesis'"), 
                          params = list(current_user()$id))
      } else if(current_user()$tipo_usuario == 'comercial') {
          # Solo material para el comercial
          df <- dbGetQuery(pool, paste(query, "WHERE p.laboratorio_id = ? AND p.tipo_pedido = 'material'"), 
                          params = list(current_user()$id))
      } else {
          # El admin/doctor ve todo ordenado por fecha
          df <- dbGetQuery(pool, paste0(query, " ORDER BY p.id DESC"))
      }
      
      if(nrow(df) > 0) {
        # Formateo de Estados (Badges)
        df$estado <- sapply(df$estado, function(e) {
          switch(e,
                 "pendiente"  = '<span class="badge bg-warning text-dark">Entrante</span>',
                 "en_proceso" = '<span class="badge bg-info">En Proceso</span>',
                 "enviado"    = '<span class="badge bg-primary">Enviado</span>',
                 "aceptado"   = '<span class="badge bg-success">Aceptado (Cerrado)</span>',
                 "devuelto"   = '<span class="badge bg-danger">Devuelto</span>', e
          )
        })
        
        # ACCIONES DINÁMICAS CON UI MEJORADA
        df$Acciones <- sapply(1:nrow(df), function(i) {
          id_p <- df$id[i]
          est_real <- dbGetQuery(pool, "SELECT estado FROM pedidos_laboratorio WHERE id = ?", params = list(id_p))$estado
          
          # --- ACCIONES LABORATORIO ---
      # --- ACCIONES PROVEEDOR (LABORATORIO O COMERCIAL) ---
          if(current_user()$tipo_usuario %in% c('laboratorio', 'comercial')) {
            
            # Si el pedido está pendiente o ha sido devuelto por la clínica
            if(est_real %in% c("pendiente", "devuelto")) {
              btn_label <- if(est_real == "devuelto") "Re-procesar" else "Aceptar y Empezar"
              
              # IMPORTANTE: Añadimos {priority: 'event'} para que detecte el clic siempre
              return(as.character(actionButton(
                ns(paste0("proc_", id_p)), 
                btn_label, 
                class = "btn-sm btn-outline-info", 
                onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("lab_proceso"), id_p)
              )))
            }
            
            # Si el pedido ya está en manos del proveedor pero no se ha enviado
            if(est_real == "en_proceso") {
              return(as.character(actionButton(
                ns(paste0("env_", id_p)), 
                "Registrar Envío", 
                class = "btn-sm btn-primary", 
                onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("lab_envio"), id_p)
              )))
            }
          }
          
          # --- ACCIONES MÉDICO (Botones separados y con estilo) ---
          # --- ACCIONES MÉDICO (Validación de recepción) ---
          # Solo Médicos, Higienistas o Admins pueden Aceptar/Devolver
          # Se excluyen explícitamente tanto a 'laboratorio' como a 'comercial'
          if(!(current_user()$tipo_usuario %in% c('laboratorio', 'comercial')) && est_real == "enviado") {
            return(paste0(
              as.character(actionButton(ns(paste0("ok_", id_p)), "Aceptar", 
                                        class="btn-sm btn-success", style="margin-right: 8px;",
                                        icon = icon("check"),
                                        onclick=sprintf("Shiny.setInputValue('%s', %d)", ns("doc_aceptar"), id_p))),
              as.character(actionButton(ns(paste0("no_", id_p)), "Devolver", 
                                        class="btn-sm btn-danger", 
                                        icon = icon("undo"),
                                        onclick=sprintf("Shiny.setInputValue('%s', %d)", ns("doc_devolver"), id_p)))
            ))
          }
          return('<span class="text-muted">Sin acciones</span>')
        })
      }
      return(df)
    }, sanitize.text.function = function(x) x)
    
    # 4. LÓGICA DE ESTADOS
    
    observeEvent(input$lab_proceso, {
      dbExecute(pool, "UPDATE pedidos_laboratorio SET estado = 'en_proceso' WHERE id = ?", params = list(input$lab_proceso))
      refresh(refresh() + 1)
    })
    
    observeEvent(input$lab_envio, {
      showModal(modalDialog(
        title = "Información de Transporte",
        textInput(ns("courier"), "Agencia de Transportes", placeholder = "Ej: MRW, Nacex..."),
        textInput(ns("track"), "Nº de Seguimiento (Tracking)"),
        footer = tagList(
          modalButton("Cancelar"), 
          actionButton(ns("confirm_envio"), "Confirmar Salida", class="btn-primary")
        )
      ))
    })
    
    observeEvent(input$confirm_envio, {
      dbExecute(pool, "UPDATE pedidos_laboratorio SET estado = 'enviado', empresa_transporte = ?, numero_seguimiento = ? WHERE id = ?",
                params = list(input$courier, input$track, input$lab_envio))
      removeModal(); refresh(refresh() + 1)
      showNotification("Pedido marcado como enviado", type = "message")
    })
    
    observeEvent(input$doc_aceptar, {
      dbExecute(pool, "UPDATE pedidos_laboratorio SET estado = 'aceptado' WHERE id = ?", params = list(input$doc_aceptar))
      refresh(refresh() + 1)
      showNotification("Trabajo aceptado y finalizado", type = "message")
    })
    
    observeEvent(input$doc_devolver, {
      showModal(modalDialog(
        title = "Formulario de Devolución",
        textAreaInput(ns("mod_text"), "Motivo de la devolución / Ajustes necesarios:", rows = 4),
        footer = tagList(
          modalButton("Cerrar"), 
          actionButton(ns("confirm_dev"), "Enviar a Laboratorio", class="btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirm_dev, {
      req(input$mod_text)
      dbExecute(pool, "UPDATE pedidos_laboratorio SET estado = 'devuelto', notas_laboratorio = ? WHERE id = ?",
                params = list(input$mod_text, input$doc_devolver))
      removeModal(); refresh(refresh() + 1)
      showNotification("Trabajo devuelto al laboratorio", type = "warning")
    })
  })
}