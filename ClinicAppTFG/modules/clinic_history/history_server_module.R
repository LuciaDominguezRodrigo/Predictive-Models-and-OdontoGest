historyServer <- function(id, pool, current_user, active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---------------- REACTIVOS ----------------
    notas_refresh <- reactiveVal(0)
    ultima_nota_id <- reactiveVal(NULL)
    
    # ---------------- CARPETA UPLOADS ----------------
    UPLOAD_DIR <- "www/uploads"
    if (!dir.exists(UPLOAD_DIR)) {
      dir.create(UPLOAD_DIR, recursive = TRUE)
    }
    
    # ---------------- 1. CARGA DE PACIENTES ----------------
    observeEvent(active_tab(), {
      if (active_tab() != "historial") return()
      
      df_pacientes <- DBI::dbGetQuery(pool, "
        SELECT id, nombre 
        FROM usuarios 
        WHERE tipo_usuario = 'paciente'
        ORDER BY nombre
      ")
      
      if (nrow(df_pacientes) > 0) {
        choices <- setNames(df_pacientes$id, df_pacientes$nombre)
        
        shinyWidgets::updatePickerInput(
          session,
          inputId = "select_paciente",
          choices = c("Seleccione un paciente..." = NA, choices),
          selected = NA
        )
      } else {
        shinyWidgets::updatePickerInput(
          session,
          inputId = "select_paciente",
          choices = c("Sin pacientes disponibles" = NA),
          selected = NA
        )
      }
    }, ignoreInit = FALSE)
    
    # ---------------- 2. BOTÓN NUEVA NOTA (RENDER) ----------------
    output$btn_nueva_nota_container <- renderUI({
      req(current_user())
      # Solo perfiles autorizados pueden ver el botón
      if (current_user()$tipo_usuario %in% c("admin", "doctor", "higienista")) {
        actionButton(ns("nueva_nota"), "➕ Nueva Nota Médica", class = "btn-purple w-100")
      }
    })
    
    # ---------------- 3. MODAL DE NUEVA NOTA ----------------
    observeEvent(input$nueva_nota, {
      if (is.null(input$select_paciente) || is.na(input$select_paciente)) {
        showNotification("Debe seleccionar un paciente primero", type = "warning")
        return()
      }
      
      showModal(modalDialog(
        title = "Nueva Nota Médica",
        textAreaInput(ns("contenido_nota"), "Contenido de la nota", rows = 6, placeholder = "Escriba la evolución clínica..."),
        fileInput(ns("archivo_nota"), "Adjuntar documento o imagen (Opcional)"),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("guardar_nota"), "Guardar Nota", class = "btn-purple")
        ),
        size = "m"
      ))
    })
    
    # ---------------- 4. PROCESO DE GUARDADO ----------------
    observeEvent(input$guardar_nota, {
      req(input$select_paciente, input$contenido_nota)
      
      user <- current_user()
      archivo_path <- NA
      nombre_archivo <- NA
      
      # Manejo de Archivos Adjuntos
      if (!is.null(input$archivo_nota)) {
        ext <- tolower(tools::file_ext(input$archivo_nota$name))
        nombre_archivo <- paste0(
          "pac_", input$select_paciente, "_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".", ext
        )
        
        ruta_destino <- file.path(UPLOAD_DIR, nombre_archivo)
        ok <- file.copy(input$archivo_nota$datapath, ruta_destino)
        
        if (!ok) {
          showNotification("Error al guardar el archivo en el servidor", type = "error")
          return()
        }
        
        # Ruta relativa para el navegador (apunta a www/uploads pero se accede vía uploads/)
        archivo_path <- file.path("uploads", nombre_archivo)
      }
      
      tryCatch({
        DBI::dbExecute(pool, "
          INSERT INTO notas_clinicas 
          (paciente_id, profesional_id, fecha, contenido, archivo_path, nombre_archivo)
          VALUES (?, ?, NOW(), ?, ?, ?)
        ", params = list(
          input$select_paciente,
          user$id,
          input$contenido_nota,
          archivo_path,
          nombre_archivo
        ))
        
        # Obtener el ID para resaltar la nota
        res_id <- DBI::dbGetQuery(pool, "SELECT LAST_INSERT_ID() as id")
        ultima_nota_id(res_id$id)
        
        # Actualizar UI
        notas_refresh(notas_refresh() + 1)
        removeModal()
        showNotification("Nota guardada correctamente", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error en DB:", e$message), type = "error")
      })
    })
    
    # ---------------- 5. UI: RESUMEN DEL PACIENTE ----------------
    output$resumen_paciente <- renderUI({
      req(input$select_paciente)
      if (is.na(input$select_paciente)) return(p(class="text-muted", "Seleccione un paciente para ver su información."))
      
      pacc <- DBI::dbGetQuery(pool,
                              "SELECT nombre, email, telefono FROM usuarios WHERE id = ?",
                              params = list(input$select_paciente)
      )
      
      if (nrow(pacc) == 0) return(NULL)
      
      div(class = "mt-2 p-3 border rounded bg-light",
          span(class = "badge bg-info mb-2", "Paciente Activo"),
          h5(class = "text-purple", pacc$nombre),
          p(class="mb-1", tags$strong("Email: "), pacc$email),
          p(class="mb-0", tags$strong("Tel: "), pacc$telefono)
      )
    })
    
    # ---------------- 6. UI: TIMELINE DINÁMICO ----------------
    output$timeline_historial <- renderUI({
      req(input$select_paciente)
      notas_refresh() # Dependencia para refrescar
      
      if (is.na(input$select_paciente)) {
        return(div(class="text-center p-5", p("Por favor, seleccione un paciente de la lista.")))
      }
      
      # Query unificada de Citas y Notas
      eventos <- DBI::dbGetQuery(pool, "
        (SELECT id, fecha_inicio AS fecha, tipo_servicio AS titulo, observaciones AS detalle, 
                'Cita' AS tipo, NULL AS archivo_path
         FROM citas WHERE paciente_id = ?)
        UNION ALL
        (SELECT id, fecha AS fecha, 'Nota Médica' AS titulo, contenido AS detalle, 
                'Nota' AS tipo, archivo_path
         FROM notas_clinicas WHERE paciente_id = ?)
        ORDER BY fecha DESC
      ", params = list(input$select_paciente, input$select_paciente))
      
      if (nrow(eventos) == 0) {
        return(div(class="text-center p-5", p("No existen registros históricos para este paciente.")))
      }
      
      tagList(
        lapply(seq_len(nrow(eventos)), function(i) {
          
          color <- if (eventos$tipo[i] == "Cita") "border-purple" else "border-info"
          
          # Efecto visual si es la nota recién creada
          es_nueva <- !is.null(ultima_nota_id()) && 
            eventos$tipo[i] == "Nota" && 
            eventos$id[i] == ultima_nota_id()
          
          clase_extra <- if (es_nueva) "bg-warning-subtle p-3 rounded" else "p-2"
          
          div(class = paste0("border-start border-4 ps-3 mb-4 ", color, " ", clase_extra),
              span(class = "text-muted small", format(as.POSIXct(eventos$fecha[i]), "%d/%m/%Y %H:%M")),
              h5(class="mt-1", eventos$titulo[i]),
              p(class="text-dark", eventos$detalle[i]),
              
              # Lógica de Previsualización de Archivos
              if (!is.na(eventos$archivo_path[i]) && eventos$archivo_path[i] != "") {
                ext <- tolower(tools::file_ext(eventos$archivo_path[i]))
                
                if (ext %in% c("png", "jpg", "jpeg")) {
                  tags$div(class="mt-2",
                           tags$img(src = eventos$archivo_path[i], 
                                    class="img-thumbnail", 
                                    style = "max-width:250px; cursor:zoom-in;",
                                    onclick = paste0("window.open('", eventos$archivo_path[i], "')"))
                  )
                } else if (ext == "pdf") {
                  tags$iframe(src = eventos$archivo_path[i], 
                              width = "100%", height = "300px", 
                              style = "border:none; margin-top:10px;")
                } else {
                  tags$a(href = eventos$archivo_path[i], target = "_blank", 
                         class="btn btn-sm btn-outline-secondary mt-2",
                         "📎 Descargar Documento")
                }
              },
              
              div(class="mt-2",
                  span(class = paste0("badge ", if(eventos$tipo[i]=="Cita") "bg-purple" else "bg-info"), 
                       eventos$tipo[i])
              )
          )
        })
      )
    })
    
  })
}