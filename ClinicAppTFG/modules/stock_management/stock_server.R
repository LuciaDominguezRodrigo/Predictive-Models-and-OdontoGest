# ==============================================================================
# PROYECTO: ClinicAppTFG | MÓDULO: stock_server.R
# ==============================================================================
library(DT)
library(ggplot2)
library(tidyr)
library(openxlsx)

stockServer <- function(id, pool, current_user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Almacén reactivo para guardar los datos de la predicción y sus ediciones
    v_datos_tabla <- reactiveVal(NULL)
    
    # 1. EVENTO: Botón Predecir
    observeEvent(input$btn_predecir, {
      # Cargar datos reales de la BBDD
      info_bbdd <- cargar_datos_entrenamiento(pool)
      
      if(nrow(info_bbdd$data) == 0) {
        shinyalert::shinyalert("Error", "No hay datos en 'historico_stock' para entrenar.", type = "error")
        return()
      }
      
      # Notificación informativa para el TFG
      shinyalert::shinyalert(
        title = "Cerebro IA Conectado",
        text = paste0(
          "Registros analizados: ", info_bbdd$stats$n_registros, "\n",
          "Periodo: ", info_bbdd$stats$fecha_min, " a ", info_bbdd$stats$fecha_max, "\n",
          "Experiencia: ", info_bbdd$stats$meses, " meses."
        ),
        type = "success"
      )
      
      # Entrenar modelo con lógica de app.R
      modelo_log <- entrenar_modelo_ia(info_bbdd$data)
      
      # Preparar datos para predecir todos los productos existentes
      productos_unicos <- unique(info_bbdd$data$producto)
      entrada_ia <- data.frame(
        producto = productos_unicos,
        stock_inicio = 50,      # Valor base (podría venir de otra tabla de stock actual)
        pacientes = input$num_pacientes, 
        pedidos_realizados = 100 # Valor base de referencia
      )
      
      # Ejecutar predicción
      prediccion <- predecir_stock(modelo_log, entrada_ia)
      
      # Guardar en el valor reactivo para que la tabla y el gráfico se actualicen
      v_datos_tabla(prediccion)
    })
    
    # 2. RENDER: Tabla Dinámica (DT)
    output$tabla_editable_ia <- DT::renderDT({
      req(v_datos_tabla())
      datatable(
        v_datos_tabla(),
        editable = list(target = "cell", disable = list(columns = c(1,2,3))), # Solo editable la col 4
        colnames = c("Material", "Stock Actual", "Pacientes Prev.", "Pedido Sugerido (IA)"),
        options = list(dom = 't', paging = FALSE, language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')),
        selection = 'none'
      )
    })
    
    # 3. EVENTO: Capturar la edición manual en la tabla
    observeEvent(input$tabla_editable_ia_cell_edit, {
      info <- input$tabla_editable_ia_cell_edit
      df <- v_datos_tabla()
      
      # Actualizar el valor editado (asegurando que sea numérico)
      df[info$row, info$col] <- as.numeric(info$value)
      v_datos_tabla(df) 
    })
    
    # 4. RENDER: Gráfico (Corregido para evitar el error de columnas inexistentes)
    output$grafico_ia <- renderPlot({
      req(v_datos_tabla())
      df <- v_datos_tabla()
      
      # Pivotar datos para ggplot (usamos los nombres exactos que devuelve predecir_stock)
      df_long <- df %>%
        pivot_longer(cols = c(stock_actual, sugerencia_ia), 
                     names_to = "Tipo", values_to = "Cantidad")
      
      ggplot(df_long, aes(x = producto, y = Cantidad, fill = Tipo)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("stock_actual" = "#bdc3c7", "sugerencia_ia" = "#8e44ad"),
                          labels = c("Stock Actual", "Pedido Sugerido")) +
        theme_minimal() +
        labs(title = "Contraste: Inventario vs Sugerencia IA", 
             x = "Producto", y = "Unidades", fill = "Leyenda") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # 5. UI: Botón de confirmación (aparece solo si hay datos)
    output$ui_boton_confirmar <- renderUI({
      req(v_datos_tabla())
      actionButton(ns("confirmar_pedido_final"), "Confirmar Pedido Revisado", 
                   class = "btn-success btn-block", icon = icon("paper-plane"))
    })
    
    output$ui_boton_descargar <- renderUI({
      req(v_datos_tabla())
      downloadButton(ns("descargar_excel"), "Descargar Pedido (Excel)", 
                     class = "btn-default btn-block")
    })
    
    # 2. Lógica de generación del archivo Excel
    output$descargar_excel <- downloadHandler(
      filename = function() {
        paste0("Pedido_IA_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        # Obtenemos los datos actuales de la tabla (incluyendo tus ediciones)
        datos_finales <- v_datos_tabla()
        
        # Cambiamos nombres de columnas para que el Excel quede bonito
        colnames(datos_finales) <- c("Producto/Material", "Stock en Clínica", "Pacientes Previstos", "Cantidad a Pedir")
        
        # Creamos el libro de Excel
        wb <- createWorkbook()
        addWorksheet(wb, "Pedido Sugerido")
        
        # Estilo para la cabecera (Negrita y fondo de color)
        headerStyle <- createStyle(fontColour = "#ffffff", fgFill = "#8e44ad", 
                                   halign = "center", textDecoration = "bold")
        
        # Escribimos los datos
        writeData(wb, "Pedido Sugerido", datos_finales, startRow = 1, startCol = 1, headerStyle = headerStyle)
        
        # Ajustamos el ancho de las columnas automáticamente
        setColWidths(wb, "Pedido Sugerido", cols = 1:4, widths = "auto")
        
        # Guardamos el archivo
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    # Añade esto dentro del stockServer en stock_server.R
    
    observeEvent(input$btn_add_fila, {
      req(input$nuevo_prod_nombre) # Evita añadir si el nombre está vacío
      
      # 1. Obtenemos lo que hay ahora en la tabla
      df_actual <- v_datos_tabla()
      
      # 2. Creamos la nueva fila
      # Nota: Ponemos 0 en Stock y Pacientes porque es manual, o lo que tú prefieras
      nueva_fila <- data.frame(
        producto = input$nuevo_prod_nombre,
        stock_actual = 0,
        pacientes = 0,
        sugerencia_ia = input$nuevo_prod_cant
      )
      
      # 3. La unimos y actualizamos el valor reactivo
      df_nuevo <- rbind(df_actual, nueva_fila)
      v_datos_tabla(df_nuevo)
      
      # 4. Limpiamos los campos para el siguiente producto
      updateTextInput(session, "nuevo_prod_nombre", value = "")
      updateNumericInput(session, "nuevo_prod_cant", value = 1)
    })
    # 6. EVENTO: Guardar pedido editado en la BBDD
    observeEvent(input$confirmar_pedido_final, {
      df_final <- v_datos_tabla()
      
      # Crear cadena de texto con el pedido final (incluyendo tus ediciones)
      lineas_pedido <- paste("-", df_final$producto, ":", df_final$sugerencia_ia, "uds.", collapse = "\n")
      texto_bbdd <- paste("PEDIDO IA VALIDADO\nFecha:", Sys.Date(), "\n\nDetalle:\n", lineas_pedido)
      
      dbExecute(pool, 
                "INSERT INTO pedidos_laboratorio (doctor_id, laboratorio_id, paciente_nombre, descripcion, estado, tipo_pedido) 
                 VALUES (?, 2, 'ALMACÉN IA', ?, 'pendiente', 'material')",
                params = list(current_user()$id, texto_bbdd))
      
      shinyalert::shinyalert("¡Pedido Enviado!", "Se ha registrado el pedido en la base de datos.", type = "success")
    })
  })
}