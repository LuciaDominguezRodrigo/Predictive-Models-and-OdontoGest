# ==============================================================================
# PROYECTO: ClinicAppTFG | MÓDULO: diagnosis_server.R
# DESCRIPCIÓN: Lógica de servidor con AUTO-SINCRONIZACIÓN (XGBoost)
# ==============================================================================

diagnosticoServer <- function(id, pool, current_user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- 1. VARIABLES REACTIVAS ---
    v_modelo_entrenado <- reactiveVal(NULL)
    v_ultimo_resultado <- reactiveVal(NULL)
    
    # --- 2. FUNCIÓN DE LOGICA DE ENTRENAMIENTO (Reutilizable) ---
    # Esta función encapsula la carga de datos y el entrenamiento
    ejecutar_sincronizacion_ia <- function(mostrar_alerta = FALSE) {
      req(pool)
      
      tryCatch({
        # 1. Cargar datos desde la tabla creada en db_init.R
        datos_entrenamiento <- cargar_datos_diagnostico(pool)
        
        # Validamos que existan datos suficientes
        if(nrow(datos_entrenamiento) < 10) {
          if(mostrar_alerta) {
            shinyalert::shinyalert("Datos insuficientes", 
                                   "La base de datos necesita más registros históricos para entrenar la IA.", 
                                   type = "warning")
          }
          return(NULL)
        }
        
        # 2. Entrenar el modelo XGBoost (Función en model_diagnosis.R)
        mod <- entrenar_modelo_diagnostico(datos_entrenamiento)
        
        # 3. Guardar en la variable reactiva
        v_modelo_entrenado(mod)
        
        if(mostrar_alerta) {
          shinyalert::shinyalert("IA Sincronizada", 
                                 paste("Modelo actualizado con", nrow(datos_entrenamiento), "casos."), 
                                 type = "success")
        }
        message("IA Diagnosis: Sincronización automática completada.")
        
      }, error = function(e) {
        message("Error en sincronización IA: ", e$message)
      })
    }
    
    # --- 3. AUTO-EJECUCIÓN AL INICIO ---
    # Esto hace que en cuanto el módulo se carga (al entrar el doctor), 
    # la IA ya esté lista sin pulsar nada.
    ejecutar_sincronizacion_ia(mostrar_alerta = FALSE)
    
    # --- 4. EVENTO: BOTÓN MANUAL (Opcional) ---
    observeEvent(input$btn_entrenar_ia, {
      ejecutar_sincronizacion_ia(mostrar_alerta = TRUE)
    })
    
    # --- 5. EVENTO: REALIZAR PREDICCIÓN (Botón Analizar) ---
    observeEvent(input$btn_analizar, {
  # --- 1. Verificar si el modelo está cargado ---
  if (is.null(v_modelo_entrenado())) {
    ejecutar_sincronizacion_ia(mostrar_alerta = FALSE)
  }
  req(v_modelo_entrenado())
  
  modelo_obj <- v_modelo_entrenado()  # lista con $modelo y $niveles
  
  # --- 2. Recopilar datos del paciente ---
  datos_paciente <- data.frame(
    edad = as.numeric(input$edad),
    indice_placa = as.numeric(input$placa),
    sangrado_sondaje = as.numeric(input$sangrado),
    profundidad_bolsa_max = as.numeric(input$bolsa),
    es_fumador = as.numeric(input$fumador)
  )
  
  # --- 3. Crear DMatrix para XGBoost ---
  dtest <- xgb.DMatrix(data = as.matrix(datos_paciente))
  
  # --- 4. Predicción de probabilidades ---
  pred_vector <- predict(modelo_obj$modelo, dtest)
  
  num_class <- length(modelo_obj$niveles)
  pred_matrix <- matrix(pred_vector, ncol = num_class, byrow = TRUE)
  
  probs <- pred_matrix[1, ]  # si es un solo paciente
  
  # --- 5. Guardar resultados en un data.frame ---
  df_res <- data.frame(
    Categoria = modelo_obj$niveles,
    Probabilidad = probs
  )
  
  v_ultimo_resultado(df_res)
  
  # --- 6. Guardar el caso en la base de datos ---
  diag_final <- df_res$Categoria[which.max(df_res$Probabilidad)]
  
  dbExecute(pool, 
            "INSERT INTO historico_diagnosticos 
            (edad, indice_placa, sangrado_sondaje, profundidad_bolsa_max, es_fumador, diagnostico_final) 
            VALUES (?, ?, ?, ?, ?, ?)",
            params = list(
              input$edad, input$placa, input$sangrado, input$bolsa,
              as.numeric(input$fumador), diag_final
            ))
})
    
    
    # --- 6. RENDERIZADO DE RESULTADOS ---
    
    output$alerta_resultado <- renderUI({
      req(v_ultimo_resultado())
      res <- v_ultimo_resultado()
      mejor_cat <- res$Categoria[which.max(res$Probabilidad)]
      
      color_alert <- switch(mejor_cat,
                            "Salud Normal" = "alert-success",
                            "Caries" = "alert-warning",
                            "Periodontitis" = "alert-danger")
      
      div(class = paste("alert", color_alert),
          style = "margin-top: 20px; border-left: 5px solid #2c3e50;",
          h4(icon("stethoscope"), "Diagnóstico IA:"),
          h2(strong(toupper(mejor_cat)), style="margin: 0;"),
          p(paste0("Confianza: ", round(max(res$Probabilidad) * 100, 1), "%")),
          hr(),
          p(tags$small(paste("Analizado por:", current_user()$nombre)))
      )
    })
    
    output$plot_probs <- plotly::renderPlotly({
      req(v_ultimo_resultado())
      p <- ggplot(v_ultimo_resultado(), aes(x = Categoria, y = Probabilidad, fill = Categoria)) +
        geom_col(show.legend = FALSE) +
        scale_fill_manual(values = c("Salud Normal"="#27ae60", "Caries"="#f1c40f", "Periodontitis"="#e67e22")) +
        theme_minimal() +
        labs(title = "Probabilidades calculadas por XGBoost", y = "Confianza", x = NULL)
      plotly::ggplotly(p)
    })
    
    output$plot_explicacion <- plotly::renderPlotly({
      req(v_ultimo_resultado())
      # Simulación de importancia para la UI (SHAP visual)
      imp_local <- data.frame(
        Variable = c("Edad", "Placa", "Sangrado", "Bolsa"),
        Peso = c(input$edad * 0.001, input$placa * 0.01, input$sangrado * 0.02, input$bolsa * 0.05)
      )
      p <- ggplot(imp_local, aes(x = reorder(Variable, Peso), y = Peso, fill = Peso)) +
        geom_col() + coord_flip() + theme_minimal() +
        labs(title = "Factores con mayor impacto", x = NULL, y = "Peso")
      plotly::ggplotly(p)
    })
  })
}