library(rmarkdown)
library(pagedown)

historyServer <- function(id, pool, current_user, active_tab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    options(shiny.maxRequestSize = 30 * 1024^2)
    
    # ---------------- 0. ESTILOS + JS SPINNER ----------------
    insertUI(selector = "head", where = "beforeEnd", ui = tagList(
      
      tags$style(HTML(paste0("
        .", ns("btn-pdf-refinado"), " {
          color: #dc3545 !important;
          background-color: transparent !important;
          border: 2px solid #dc3545 !important;
          transition: all 0.3s ease !important;
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }
        .", ns("btn-pdf-refinado"), ":hover {
          color: white !important;
          background-color: #dc3545 !important;
          box-shadow: 0 4px 8px rgba(220, 53, 69, 0.3);
        }

        .btn-adjunto-mini {
          display: inline-flex;
          align-items: center;
          gap: 6px;
          padding: 4px 10px;
          background-color: #dc3545;
          color: white !important;
          border-radius: 4px;
          font-size: 11px;
          font-weight: 500;
          text-decoration: none;
          transition: opacity 0.2s;
        }
        .btn-adjunto-mini:hover {
          opacity: 0.8;
          color: white !important;
        }
      "))),
      
      tags$script(HTML(paste0("
        $(document).on('click', '#", ns("descargar_pdf"), "', function() {
          var btn = $(this);
          btn.prop('disabled', true);
          btn.data('original-text', btn.html());
          btn.html('<i class=\"fa fa-spinner fa-spin\"></i> Generando informe clínico...');
          
          setTimeout(function() {
            btn.prop('disabled', false);
            btn.html(btn.data('original-text'));
          }, 10000);
        });
      ")))
    ))
    
    # ---------------- 1. REACTIVOS ----------------
    notas_refresh <- reactiveVal(0)
    
    # ---------------- 2. UPLOADS ----------------
    UPLOAD_DIR <- "www/uploads"
    if (!dir.exists(UPLOAD_DIR)) {
      dir.create(UPLOAD_DIR, recursive = TRUE)
    }
    
    # ---------------- 3. PACIENTES ----------------
    observeEvent(active_tab(), {
      if (active_tab() != "historial") return()
      
      df_pacientes <- DBI::dbGetQuery(pool, "
        SELECT id, nombre FROM usuarios 
        WHERE tipo_usuario = 'paciente' ORDER BY nombre
      ")
      
      if (nrow(df_pacientes) > 0) {
        choices <- setNames(df_pacientes$id, df_pacientes$nombre)
        shinyWidgets::updatePickerInput(
          session, "select_paciente",
          choices = c("Seleccione un paciente..." = NA, choices),
          selected = NA
        )
      }
    }, ignoreInit = FALSE)
    
    # ---------------- 4. BOTONES ----------------
    output$btn_nueva_nota_container <- renderUI({
      req(current_user(), input$select_paciente)
      
      if (current_user()$tipo_usuario %in% c("admin", "doctor", "higienista")) {
        tagList(
          actionButton(ns("nueva_nota"), "➕ Nueva Nota Médica", class = "btn-purple w-100 mb-2"),
          downloadButton(ns("descargar_pdf"), "📄 Descargar Historial PDF", 
                         class = paste("btn w-100", ns("btn-pdf-refinado")))
        )
      }
    })
    
    # ---------------- 5. NUEVA NOTA ----------------
    observeEvent(input$nueva_nota, {
      showModal(modalDialog(
        title = "Nueva Nota Médica",
        textAreaInput(ns("contenido_nota"), "Contenido de la nota", rows = 6),
        fileInput(ns("archivo_nota"), "Adjuntar archivos", multiple = TRUE),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("guardar_nota"), "Guardar", class = "btn-purple")
        ),
        size = "m"
      ))
    })
    
    observeEvent(input$guardar_nota, {
      req(input$select_paciente, input$contenido_nota)
      
      user <- current_user()
      rutas_relativas <- c()
      fecha_actual <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      if (!is.null(input$archivo_nota)) {
        archivos <- input$archivo_nota
        for (i in 1:nrow(archivos)) {
          ext <- tolower(tools::file_ext(archivos$name[i]))
          nombre_gen <- paste0(
            "pac_", input$select_paciente, "_",
            format(Sys.time(), "%Y%m%d_%H%M%S"), "_", i, ".", ext
          )
          ruta_destino <- file.path(UPLOAD_DIR, nombre_gen)
          
          if (file.copy(archivos$datapath[i], ruta_destino)) {
            rutas_relativas <- c(rutas_relativas, file.path("uploads", nombre_gen))
          }
        }
      }
      
      archivo_path_db <- if (length(rutas_relativas) > 0) {
        paste(rutas_relativas, collapse = ",")
      } else NA
      
      DBI::dbExecute(pool, "
        INSERT INTO notas_clinicas (paciente_id, profesional_id, fecha, contenido, archivo_path)
        VALUES (?, ?, ?, ?, ?)
      ", params = list(
        input$select_paciente,
        user$id,
        fecha_actual,
        input$contenido_nota,
        archivo_path_db
      ))
      
      notas_refresh(notas_refresh() + 1)
      removeModal()
      showNotification("Nota guardada", type = "message")
    })
    
    # ---------------- 6. PDF ----------------
    output$descargar_pdf <- downloadHandler(
      filename = function() {
        paste0("Historial_", input$select_paciente, "_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        req(input$select_paciente)
        
        info_pac <- DBI::dbGetQuery(pool,
                                    "SELECT nombre FROM usuarios WHERE id = ?",
                                    params = list(input$select_paciente)
        )
        
        eventos <- DBI::dbGetQuery(pool, "
          (SELECT fecha_inicio AS fecha, tipo_servicio AS titulo, observaciones AS detalle, 'Cita' AS tipo FROM citas WHERE paciente_id = ?)
          UNION ALL
          (SELECT fecha AS fecha, 'Nota Médica' AS titulo, contenido AS detalle, 'Nota' AS tipo FROM notas_clinicas WHERE paciente_id = ?)
          ORDER BY fecha DESC
        ", params = list(input$select_paciente, input$select_paciente))
        
        temp_rmd  <- file.path(tempdir(), "reporte.Rmd")
        temp_html <- file.path(tempdir(), "reporte.html")
        
        writeLines(c(
          "---",
          "title: 'Informe Historial Clínico'",
          "output: html_document",
          "params:",
          "  paciente: NA",
          "  eventos: NA",
          "---",
          
          "```{r setup, include=FALSE}",
          "library(knitr)",
          "eventos <- params$eventos",
          "```",
          
          "## Paciente",
          "`r params$paciente`",
          
          "## Historial",
          
          "```{r, echo=FALSE}",
          "knitr::kable(eventos[,c('fecha','tipo','titulo','detalle')])",
          "```"
          
        ), temp_rmd)
        
        rmarkdown::render(
          temp_rmd,
          output_file = temp_html,
          params = list(
            paciente = info_pac$nombre,
            eventos = eventos
          ),
          envir = new.env(parent = globalenv())
        )
        
        Sys.sleep(2)
        
        pagedown::chrome_print(
          input = temp_html,
          output = file,
          wait = 5,
          timeout = 60,
          options = list(printBackground = TRUE)
        )
      }
    )
    
    # ---------------- 7. TIMELINE ----------------
    output$timeline_historial <- renderUI({
      req(input$select_paciente)
      notas_refresh()
      
      eventos <- DBI::dbGetQuery(pool, "
        (SELECT id, fecha_inicio AS fecha, tipo_servicio AS titulo, observaciones AS detalle, 'Cita' AS tipo, NULL AS archivo_path FROM citas WHERE paciente_id = ?)
        UNION ALL
        (SELECT id, fecha AS fecha, 'Nota Médica' AS titulo, contenido AS detalle, 'Nota' AS tipo, archivo_path FROM notas_clinicas WHERE paciente_id = ?)
        ORDER BY fecha DESC
      ", params = list(input$select_paciente, input$select_paciente))
      
      tagList(
        lapply(seq_len(nrow(eventos)), function(i) {
          
          color_borde <- if (eventos$tipo[i] == "Cita") "border-purple" else "border-info"
          
          div(class = paste0("border-start border-4 ps-3 mb-4 ", color_borde),
              span(class = "text-muted small",
                   format(as.POSIXct(eventos$fecha[i]), "%d/%m/%Y %H:%M")),
              h5(class="mt-1", eventos$titulo[i]),
              p(class="text-dark", eventos$detalle[i]),
              
              if (!is.na(eventos$archivo_path[i]) && eventos$archivo_path[i] != "") {
                rutas <- strsplit(eventos$archivo_path[i], ",")[[1]]
                
                div(class="d-flex flex-wrap gap-2 mt-2", 
                    lapply(rutas, function(r) {
                      ext <- tolower(tools::file_ext(r))
                      
                      if (ext %in% c("png", "jpg", "jpeg")) {
                        tags$img(
                          src = r,
                          class="img-thumbnail",
                          style="width:80px;height:80px;object-fit:cover;cursor:pointer;",
                          onclick = paste0("window.open('", r, "')")
                        )
                      } else {
                        tags$a(
                          href = r,
                          target = "_blank",
                          class="btn-adjunto-mini",
                          tags$i(class="fa fa-file-pdf"),
                          basename(r)
                        )
                      }
                    })
                )
              },
              
              div(class="mt-2",
                  span(class = paste0("badge ",
                                      if(eventos$tipo[i]=="Cita") "bg-purple" else "bg-info"),
                       eventos$tipo[i])
              )
          )
        })
      )
    })
    
  })
}