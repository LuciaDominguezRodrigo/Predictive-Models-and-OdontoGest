# =======================================================
# Shiny App: Modelos base para la aplicación de TFG
# Autor: Lucía Domínguez Rodrigo
# Descripción:
# - Modelo Stock: predice pedidos necesarios de materiales
#   en una clínica dental (Regresión Lineal con CV) - Diagnóstico Completo.
# =======================================================

library(shiny)
library(caret)
library(ggplot2)
library(reshape2)
library(dplyr)
library(shinyalert)
library(xgboost)
library(Matrix)
library(car) # Para vif, durbinWatsonTest y crPlots
library(ppcor) # Para correlaciones parciales (pcor)
library(broom) # Para tidy, augment, glance
library(tibble) # Para rownames_to_column
library(sandwich) # Para vcovHC (Errores Estándar Robustos)
library(lmtest) # Para coeftest y waldtest
library(tseries) # Para jarque.bera.test

# ==================== UI ====================
ui <- fluidPage(
  useShinyalert(), 
  titlePanel("Modelos Regresión Lineal y XGBoost -  Modelo de stock (con productos ampliados)"),
  
  tabsetPanel(
    tabPanel("Modelo Stock",
             sidebarLayout(
               sidebarPanel(
                 actionButton("entrenar_stock", "Entrenar Modelo Stock y Diagnósticos"),
                 tags$hr(),
                 h4("Exportar Predicciones del Modelo Final"),
                 # CAMBIO EN LA UI: Renombrado el botón para claridad
                 downloadButton("download_diagnostics_stock", "Descargar Predicciones (CSV)")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Pedido Mes Siguiente (MCO vs Log)", 
                            h3("Predicciones del Modelo Final para todo el Dataset"),
                            p("La siguiente tabla muestra la predicción de pedido para todos los productos en el dataset, comparando el modelo MCO y el Logarítmico."),
                            tableOutput("pedido_mes_siguiente")),
                   tabPanel("Resumen y Métricas (MCO)", 
                            h3("Métricas CV"), verbatimTextOutput("stock_metrics"),
                            tags$hr(),
                            h3("Resumen del Modelo LM (MCO Final)"), verbatimTextOutput("stock_summary"),
                            tags$hr(),
                            h3("Resumen del Modelo LM (Coeficientes Robusto - HC3)"), verbatimTextOutput("stock_robust_summary"),
                            tags$hr(),
                            h3("VIF y Durbin-Watson"), verbatimTextOutput("stock_vif_dw")),
                   tabPanel("Resumen y Métricas (Logarítmico)",
                            h3("Métricas CV (Log)"), verbatimTextOutput("stock_log_metrics"),
                            tags$hr(),
                            h3("Resumen del Modelo LM Logarítmico (Final)"), verbatimTextOutput("stock_log_summary"),
                            tags$hr(),
                            h3("Resumen del Modelo LM Logarítmico (Coeficientes Robusto - HC3)"), verbatimTextOutput("stock_log_robust_summary")),
                   tabPanel("Coeficientes e IC 95% (MCO)", verbatimTextOutput("stock_coefs"), 
                            tags$hr(), h3("Matriz de Varianza-Covarianza"), verbatimTextOutput("stock_vcov")),
                   tabPanel("Análisis de Varianza (ANOVA)", verbatimTextOutput("stock_anova")),
                   tabPanel("Normalidad de Residuales",
                            h3("Pruebas de Normalidad de los Residuales (Modelo MCO)"),
                            verbatimTextOutput("stock_normality_tests")),
                   tabPanel("Comparación de Modelos (AIC/BIC/F-test)",
                            h3("Comparación AIC/BIC"),
                            verbatimTextOutput("stock_aic_bic"),
                            tags$hr(),
                            h3("Comparación mediante F-test"),
                            verbatimTextOutput("stock_f_test")),
                   tabPanel("Correlaciones", 
                            h3("Matriz de Correlación (Variables Numéricas)"), plotOutput("stock_cor_plot", height = "400px", width = "100%"),
                            tags$hr(),
                            h3("Correlaciones Parciales"), verbatimTextOutput("stock_pcor")),
                   tabPanel("Predicción vs Real por Fold", 
                            h3("Predicción vs. Real por Fold (Validación Cruzada) - MCO"),
                            plotOutput("plot_cv_stock", height = "500px", width = "100%")),
                   tabPanel("Gráficos de Residuales (MCO)", 
                            h3("Residuales vs. Ajustados"), plotOutput("plot_res_fit", height = "400px", width = "100%"),
                            tags$hr(),
                            h3("Normalidad de Residuales (Q-Q)"), plotOutput("plot_qq", height = "400px", width = "100%"),
                            tags$hr(),
                            h3("Histograma de Residuales"), plotOutput("plot_hist_res", height = "400px", width = "100%")),
                   tabPanel("Gráficos Parciales (car::crPlots)", 
                            h3("Gráficos de Componentes y Residuales"),
                            plotOutput("plot_cr", height = "700px", width = "100%")),
                   tabPanel("Diagnóstico por Caso e Influencia", 
                            h3("Medidas de Influencia (Top 10 por Distancia de Cook)"),
                            tableOutput("stock_influence_table"),
                            tags$hr(),
                            h3("Ejemplo de DfBetas (Cambio en Coeficientes por Caso)"),
                            verbatimTextOutput("stock_dfbetas_example"))
                 )
               )
             )
    ),
    
    tabPanel("Modelo Diagnóstico",
             sidebarLayout(
               sidebarPanel(
                 actionButton("entrenar_diag", "Entrenar Modelo Diagnóstico (XGBoost)"),
                 tags$hr(),
                 h4("Exportar Predicciones Test"),
                 downloadButton("download_diag_predictions", "Descargar Predicciones (CSV)")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Importancia Variables", plotOutput("varImp_diag", height = "500px", width = "100%")),
                   tabPanel("Matriz de Confusión", 
                            h3("Métricas y Matriz de Confusión (Texto)"),
                            verbatimTextOutput("cm_diag_text"), 
                            tags$hr(),
                            h3("Matriz de Confusión (Gráfico)"),
                            plotOutput("cm_diag_plot", height = "500px", width = "100%"))
                 )
               )
             )
    )
  )
)

# ==================== SERVER ====================
server <- function(input, output, session) {
  
  # ==================== MODELO STOCK (LM CV) ====================
  modelo_stock_cv <- eventReactive(input$entrenar_stock, {
    set.seed(123)
    n <- 1000
    
    # ---------------------------
    # Lista de productos (niveles ampliados)
    # ---------------------------
    product_levels <- c(
      "Guantes","Mascarillas","Agujas","cemento",
      "micro_aplicador",
      "tetric_evoceram_A2","tetric_evoceram_A3","tetric_evoceram_A35",
      "tetric_evoflow_A2","tetric_evoflow_A3","tetric_evoflow_A35",
      "excite_f",
      "theracal_lc",
      "papel_articular",
      "matrices_transparentes",
      "matrices_seccionales_septomatrix",
      "automatrix_reposicion_medium_regular",
      "automatrix_regular_1","automatrix_regular_2","automatrix_regular_3",
      "automatrix_mango_punta_automate_iii",
      "cu_as_madera_400u",
      "acido_jumbo",
      "ufi_gel_reposicion",
      "cajas_retenedores_grandes",
      "alginato_fast_bestdent",
      "silicona_putty",
      "silicona_generica",
      "ketac_cem_easy_mix",
      "temp_bond",
      "irm",
      "variolink_esthetic"
    )
    
    # Simulamos dataset histórico con estos niveles de producto
    dataset_historico <- data.frame(
      producto = factor(sample(product_levels, n, replace = TRUE)),
      stock_inicio = sample(50:500, n, replace = TRUE),
      pacientes = sample(20:80, n, replace = TRUE),
      pedidos_realizados = sample(50:400, n, replace = TRUE)
    )
    
    # Generación de la variable target (mantengo la lógica original con ruido)
    dataset_historico$pedidos_necesarios <- with(dataset_historico,
                                                 pmax(0, round(pedidos_realizados + 0.35*(100 - stock_inicio) + rnorm(n,0,5)))
    )
    
    dataset_historico$stock_ratio <- dataset_historico$stock_inicio / pmax(1, dataset_historico$pedidos_realizados)
    dataset_historico$pacientes_ratio <- dataset_historico$pacientes / pmax(1, dataset_historico$stock_inicio)
    
    # CORRECCIÓN DE FACTOR: Aseguramos que los niveles estén completos (aunque no todos aparezcan)
    dataset_historico$producto <- factor(dataset_historico$producto, levels = product_levels)
    dataset_historico$producto <- droplevels(dataset_historico$producto)
    
    # === Definición del Control para CV 5-fold ===
    ctrl <- trainControl(method = "cv", number = 5, savePredictions = "final") 
    
    # ----------------------------------------------------
    # === 1. Train LM MCO (Modelo original) ===
    # ----------------------------------------------------
    modelo <- train(
      pedidos_necesarios ~ stock_inicio + pacientes + pedidos_realizados + producto + stock_ratio + pacientes_ratio,
      data = dataset_historico,
      method = "lm",
      trControl = ctrl,
      metric = "RMSE"
    )
    
    lm_final <- modelo$finalModel
    
    # ----------------------------------------------------
    # === 2. Train LM Logarítmico (NUEVO) ===
    # ----------------------------------------------------
    dataset_historico$log_pedidos <- log1p(dataset_historico$pedidos_necesarios) # log(1 + x)
    
    modelo_log <- train(
      log_pedidos ~ stock_inicio + pacientes + pedidos_realizados + producto + stock_ratio + pacientes_ratio,
      data = dataset_historico,
      method = "lm",
      trControl = ctrl,
      metric = "RMSE"
    )
    
    lm_log_final <- modelo_log$finalModel
    
    # ----------------------------------------------------
    # === 3. Extracción de Diagnósticos LM MCO ===
    # ----------------------------------------------------
    model_mat <- model.matrix(lm_final)
    model_vars_numeric <- model_mat[, colnames(model_mat) != "(Intercept)", drop = FALSE]
    #mantengo la amtriz, no reduzco a un vector. Selecciono todo menos el intercepto
    
    diag_case <- augment(lm_final) %>%
      mutate(
        mahalanobis = mahalanobis(model_vars_numeric, 
                                  center = colMeans(model_vars_numeric), 
                                  cov = cov(model_vars_numeric)),
        caso = 1:n()
      ) %>%
      dplyr::select(caso, everything())
    
    vif_res <- car::vif(lm_final)
    dw_res <- car::durbinWatsonTest(lm_final)
    
    coefs_ic <- confint(lm_final) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("term") %>% 
      rename(conf.low = `2.5 %`, conf.high = `97.5 %`)
    
    vcov_mat <- vcov(lm_final)
    anova_res <- anova(lm_final)
    
    numeric_vars <- c("stock_inicio", "pacientes", "pedidos_realizados", "stock_ratio", "pacientes_ratio")
    pcor_mat <- tryCatch({
      ppcor::pcor(dataset_historico[, c("pedidos_necesarios", numeric_vars)])$estimate
    }, error = function(e) paste("No se pudo calcular la correlación parcial:", e$message))
    
    dfbetas_res <- as.data.frame(dfbetas(lm_final))
    names(dfbetas_res) <- paste0("DfB_", names(dfbetas_res))
    
    influence_summary <- diag_case %>% 
      dplyr::select(caso, .fitted, .resid, .hat, .cooksd, .std.resid) %>%
      arrange(desc(.cooksd)) %>%
      head(10)
    
    robust_vcov <- vcovHC(lm_final, type = "HC3")
    robust_summary <- coeftest(lm_final, vcov. = robust_vcov)
    
    robust_vcov_log <- vcovHC(lm_log_final, type = "HC3")
    robust_summary_log <- coeftest(lm_log_final, vcov. = robust_vcov_log)
    
    shapiro_test <- shapiro.test(residuals(lm_final))
    jarque_test <- tseries::jarque.bera.test(residuals(lm_final))
    
    aic_comparison <- AIC(lm_final, lm_log_final)
    bic_comparison <- BIC(lm_final, lm_log_final)
    f_test_models <- anova(lm_final, lm_log_final)
    
    list(
      # MCO Original
      modelo = modelo, 
      lm_final = lm_final,
      dataset = dataset_historico, 
      diag_case = diag_case,
      vif_res = vif_res,
      dw_res = dw_res,
      coefs_ic = coefs_ic,
      vcov_mat = vcov_mat,
      anova_res = anova_res,
      pcor_mat = pcor_mat,
      dfbetas_res = dfbetas_res,
      influence = influence_summary,
      robust_summary = robust_summary,
      # Logarítmico (Nuevo)
      modelo_log = modelo_log,
      lm_log_final = lm_log_final,
      robust_summary_log = robust_summary_log,
      shapiro_test = shapiro_test,
      jarque_test = jarque_test,
      aic_comparison = aic_comparison,
      bic_comparison = bic_comparison,
      f_test_models = f_test_models,
      # info adicional
      product_levels = product_levels
    )
  })
  
  
  
  # === Predicción del pedido del mes siguiente (Incluye Logarítmico) ===
  
  output$pedido_mes_siguiente <- renderTable({
    req(modelo_stock_cv())
    
    # 1. Obtener los datos de predicción para todos los casos (igual que en downloadHandler)
    data_full <- modelo_stock_cv()$dataset # Dataset completo
    
    # Recalcular ratios (asegurando consistencia)
    data_full$stock_ratio <- data_full$stock_inicio / pmax(1, data_full$pedidos_realizados)
    data_full$pacientes_ratio <- data_full$pacientes / pmax(1, data_full$stock_inicio)
    
    # Generar predicciones de ambos modelos
    pred_mco <- predict(modelo_stock_cv()$modelo, newdata = data_full)
    pred_log <- predict(modelo_stock_cv()$modelo_log, newdata = data_full)
    
    # Calcular el Intervalo de Confianza (basado en RMSE de CV)
    rmse_mean <- mean(modelo_stock_cv()$modelo$resample$RMSE)
    t_val <- qt(0.975, df = nrow(data_full) - length(coef(modelo_stock_cv()$lm_final)))
    margin_error <- t_val * rmse_mean
    ic_bajo <- pmax(0, pred_mco - margin_error)
    ic_alto <- pred_mco + margin_error
    
    # Crear data frame de predicciones completas
    predicciones_completas <- data_full %>%
      dplyr::select(producto, stock_inicio, pacientes, pedidos_realizados, pedidos_necesarios) %>%
      rename(Real = pedidos_necesarios) %>%
      mutate(
        Prediccion_MCO = pred_mco, # Dejamos como números para el cálculo del promedio
        Prediccion_Log = expm1(pred_log), # Dejamos como números para el cálculo del promedio
        IC_Bajo_95_MCO = ic_bajo,
        IC_Alto_95_MCO = ic_alto
      )
    
    # 2. AGREGACIÓN: Agrupar por 'producto' y calcular el promedio (media)
    data_output_agregada <- predicciones_completas %>%
      group_by(producto) %>%
      summarise(
        # Promedio del Real (consumo histórico promedio del producto)
        Real_Promedio = round(mean(Real)),
        # Promedio de las Predicciones (el valor que querías)
        Prediccion_MCO_Promedio = round(mean(Prediccion_MCO)),
        Prediccion_Log_Promedio = round(mean(Prediccion_Log)),
        # Promedio del Intervalo de Confianza
        IC_Bajo_95_Promedio = round(mean(IC_Bajo_95_MCO)),
        IC_Alto_95_Promedio = round(mean(IC_Alto_95_MCO)),
        .groups = 'drop' # Quitar la agrupación
      )
    
    # Alerta de pedido alto (basada en el promedio MCO)
    productos_altos <- data_output_agregada$producto[data_output_agregada$Prediccion_MCO_Promedio > 400]
    if (length(productos_altos) > 0) {
      shinyalert(
        title = "⚠️ Alerta de pedido alto (Promedio)",
        text = paste("El promedio de pedido sugerido es alto para:", paste(head(unique(productos_altos), 3), collapse = ", "), 
                     "y otros productos. Considere la recomendación Logarítmica."),
        type = "warning"
      )
    }
    
    # Devolver la tabla agregada (solo 31 filas)
    data_output_agregada
    
  }, 
  # Opciones de tabla (DataTable para mostrar pocas filas es opcional, pero mantiene el formato)
  options = list(
    scrollY = "400px", 
    paging = FALSE, # Desactivamos paginación ya que hay pocas filas
    dom = 't' # Mostrar solo la tabla (sin barra de búsqueda, info, etc.)
  )
  )
  # Opciones de tabla para mostrar un gran volumen de datos de forma eficiente
  options = list(
    scrollY = "400px", 
    paging = TRUE, # Permite paginación para no sobrecargar
    pageLength = 20
  )
  
  
  
  output$stock_metrics <- renderPrint({
    req(modelo_stock_cv())
    
    modelo <- modelo_stock_cv()$modelo
    df_res <- modelo$resample
    model_summary <- glance(modelo_stock_cv()$lm_final)
    
    cat("=== Métricas del modelo de Stock (CV 5-fold) MCO (pedidos_necesarios) ===\n")
    cat("MAE (Media Folds):", round(mean(df_res$MAE), 2), "\n")
    cat("RMSE (Media Folds):", round(mean(df_res$RMSE), 2), "\n")
    cat("R² (Media Folds):", round(mean(df_res$Rsquared), 3), "\n\n")
    
    cat("=== Resumen Métricas Modelo Final MCO (Ajustado a Todo el Data) ===\n")
    cat("R-cuadrado:", round(model_summary$r.squared, 3), "\n")
    cat("R-cuadrado Ajustado:", round(model_summary$adj.r.squared, 3), "\n")
    cat("Error Estándar de la Estimación (Sigma):", round(model_summary$sigma, 2), "\n")
    cat("F-statistic:", round(model_summary$statistic, 2), "(p-value:", format.pval(model_summary$p.value, digits = 3), ")\n")
    cat("Log-Likelihood:", round(model_summary$logLik, 2), "\n")
    
    cat("\n=== Métricas de CV por Fold (caret) ===\n")
    print(df_res)
  })
  
  output$stock_summary <- renderPrint({
    req(modelo_stock_cv())
    summary(modelo_stock_cv()$lm_final)
  })
  
  output$stock_robust_summary <- renderPrint({
    req(modelo_stock_cv())
    cat("Modelo LM MCO: Resumen con Errores Estándar Robustos (HC3)\n")
    cat("Los coeficientes (Estimate) son idénticos al modelo MCO original. \n")
    cat("La diferencia está en los errores estándar (Std. Error) y los p-values (Pr(>|t|)), \n")
    cat("los cuales están corregidos por la heterocedasticidad (varianza no constante de los residuales).\n\n")
    print(modelo_stock_cv()$robust_summary)
  })
  
  output$stock_log_metrics <- renderPrint({
    req(modelo_stock_cv())
    
    modelo <- modelo_stock_cv()$modelo_log
    df_res <- modelo$resample
    model_summary <- glance(modelo_stock_cv()$lm_log_final)
    
    cat("=== Métricas del modelo de Stock (CV 5-fold) LOGARÍTMICO (log_pedidos) ===\n")
    cat("El RMSE se aplica a la variable dependiente log(pedidos_necesarios + 1).\n")
    cat("MAE (Media Folds):", round(mean(df_res$MAE), 4), "\n")
    cat("RMSE (Media Folds):", round(mean(df_res$RMSE), 4), "\n")
    cat("R² (Media Folds):", round(mean(df_res$Rsquared), 3), "\n\n")
    
    cat("=== Resumen Métricas Modelo Final LOGARÍTMICO (Ajustado a Todo el Data) ===\n")
    cat("R-cuadrado:", round(model_summary$r.squared, 3), "\n")
    cat("R-cuadrado Ajustado:", round(model_summary$adj.r.squared, 3), "\n")
    cat("Error Estándar de la Estimación (Sigma):", round(model_summary$sigma, 4), "\n")
    cat("F-statistic:", round(model_summary$statistic, 2), "(p-value:", format.pval(model_summary$p.value, digits = 3), ")\n")
    cat("Log-Likelihood:", round(model_summary$logLik, 2), "\n")
    
    cat("\n=== Métricas de CV por Fold (caret) ===\n")
    print(df_res)
  })
  
  output$stock_log_summary <- renderPrint({
    req(modelo_stock_cv())
    summary(modelo_stock_cv()$lm_log_final)
  })
  
  output$stock_log_robust_summary <- renderPrint({
    req(modelo_stock_cv())
    cat("Modelo LM Logarítmico: Resumen con Errores Estándar Robustos (HC3)\n\n")
    print(modelo_stock_cv()$robust_summary_log)
  })
  
  output$stock_vif_dw <- renderPrint({
    req(modelo_stock_cv())
    
    cat("=== Factor de Inflación de la Varianza (VIF) y Tolerancia (1/VIF) ===\n")
    vif_df <- data.frame(VIF = modelo_stock_cv()$vif_res, Tolerancia = 1/modelo_stock_cv()$vif_res)
    print(vif_df)
    
    cat("\n=== Prueba de Durbin-Watson (Autocorrelación de Residuales) ===\n")
    print(modelo_stock_cv()$dw_res)
  })
  
  output$stock_coefs <- renderPrint({
    req(modelo_stock_cv())
    
    cat("=== Coeficientes de Regresión e Intervalos de Confianza (95%) NO ROBUSTOS (MCO) ===\n")
    cat("Advertencia: Estos IC son ineficientes e incorrectos debido a la heterocedasticidad. \n")
    cat("Use el resumen Robusto (HC3) en la pestaña 'Resumen y Métricas'.\n\n")
    
    coefs_summary <- tidy(modelo_stock_cv()$lm_final)
    ics_df <- modelo_stock_cv()$coefs_ic 
    coefs_summary <- left_join(coefs_summary, ics_df, by = "term")
    print(coefs_summary)
  })
  
  output$stock_vcov <- renderPrint({
    req(modelo_stock_cv())
    cat("=== Matriz de Varianza-Covarianza NO ROBUSTA (MCO) ===\n")
    print(modelo_stock_cv()$vcov_mat)
  })
  
  output$stock_anova <- renderPrint({
    req(modelo_stock_cv())
    
    cat("=== Tabla de Análisis de Varianza (ANOVA) NO ROBUSTA ===\n")
    cat("Advertencia: La prueba F global es incorrecta. Use el resumen Robusto para la significancia de coeficientes.\n\n")
    print(modelo_stock_cv()$anova_res)
  })
  
  output$stock_pcor <- renderPrint({
    req(modelo_stock_cv())
    cat("=== Matriz de Correlación Parcial (Entre variables numéricas y dependiente MCO) ===\n")
    print(modelo_stock_cv()$pcor_mat)
  })
  
  output$stock_cor_plot <- renderPlot({
    req(modelo_stock_cv())
    
    numeric_data <- modelo_stock_cv()$dataset %>% 
      dplyr::select_if(is.numeric)
    
    cor_mat <- cor(numeric_data)
    cor_melt <- melt(cor_mat)
    
    ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      geom_text(aes(label = round(value, 2)), color = "white", size = 4) +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), name = "Correlación") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()) +
      ggtitle("Mapa de Calor de la Matriz de Correlación")
  })
  
  output$plot_cv_stock <- renderPlot({
    req(modelo_stock_cv())
    df_preds_folds <- modelo_stock_cv()$modelo$pred
    
    ggplot(df_preds_folds, aes(x = obs, y = pred, color = Resample)) +
      geom_point(alpha = 0.6) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      labs(title = "Predicción vs Real por Fold - Modelo Stock (LM CV - MCO)",
           x = "Valor Real (Observado)", y = "Predicción", color = "Fold") +
      theme_minimal()
  })
  
  output$plot_res_fit <- renderPlot({
    req(modelo_stock_cv())
    diag_data <- modelo_stock_cv()$diag_case
    
    ggplot(diag_data, aes(x = .fitted, y = .std.resid)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_smooth(method = "loess", se = FALSE, color = "blue") +
      labs(title = "Residuales Estandarizados vs. Valores Ajustados (Muestra Heterocedasticidad)",
           x = "Valores Ajustados (Predichos)", y = "Residuales Estandarizados") +
      theme_minimal()
  })
  
  output$plot_qq <- renderPlot({
    req(modelo_stock_cv())
    diag_data <- modelo_stock_cv()$diag_case
    
    ggplot(diag_data, aes(sample = .std.resid)) +
      stat_qq() +
      stat_qq_line() +
      labs(title = "Gráfico Q-Q de Residuales Estandarizados",
           x = "Cuantiles Teóricos", y = "Residuales Estandarizados") +
      theme_minimal()
  })
  
  output$plot_hist_res <- renderPlot({
    req(modelo_stock_cv())
    diag_data <- modelo_stock_cv()$diag_case
    
    ggplot(diag_data, aes(x = .resid)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Histograma de Residuales",
           x = "Residuales", y = "Frecuencia") +
      theme_minimal()
  })
  
  output$plot_cr <- renderPlot({
    req(modelo_stock_cv())
    car::crPlots(modelo_stock_cv()$lm_final, ask = FALSE)
  })
  
  output$stock_influence_table <- renderTable({
    req(modelo_stock_cv())
    modelo_stock_cv()$influence %>%
      rename(
        Ajustado = .fitted,
        Residual = .resid,
        Apalancamiento = .hat,
        Cook_D = .cooksd,
        Resid_Estandarizado = .std.resid
      )
  })
  
  output$stock_dfbetas_example <- renderPrint({
    req(modelo_stock_cv())
    
    cat("=== DfBetas (Cambio en Coeficientes al Eliminar una Observación) ===\n")
    cat("Muestra los DfBetas para las primeras 5 observaciones:\n\n")
    print(head(modelo_stock_cv()$dfbetas_res, 5))
    cat("\nNota: DfBetas > 1 (o 2/sqrt(n)) indica un caso con alta influencia en ese coeficiente.")
  })
  
  output$download_diagnostics_stock <- downloadHandler(
    filename = function() { paste0('predicciones_modelo_FINAL_consolidado_', Sys.Date(), '.csv') },
    content = function(file) {
      req(modelo_stock_cv())
      
      data_full <- modelo_stock_cv()$dataset # Dataset completo
      
      # 1. Preparar las variables para la predicción
      data_full$stock_ratio <- data_full$stock_inicio / pmax(1, data_full$pedidos_realizados)
      data_full$pacientes_ratio <- data_full$pacientes / pmax(1, data_full$stock_inicio)
      
      # 2. Generar predicciones de ambos modelos
      pred_mco <- predict(modelo_stock_cv()$modelo, newdata = data_full)
      pred_log <- predict(modelo_stock_cv()$modelo_log, newdata = data_full)
      
      # 3. Calcular el Intervalo de Confianza (basado en RMSE de CV)
      rmse_mean <- mean(modelo_stock_cv()$modelo$resample$RMSE)
      t_val <- qt(0.975, df = nrow(data_full) - length(coef(modelo_stock_cv()$lm_final)))
      margin_error <- t_val * rmse_mean
      ic_bajo <- pmax(0, pred_mco - margin_error)
      ic_alto <- pred_mco + margin_error
      
      # 4. Crear data frame de predicciones completas (sin redondear aún)
      predicciones_completas <- data_full %>%
        dplyr::select(producto, pedidos_necesarios) %>%
        rename(Real = pedidos_necesarios) %>%
        mutate(
          Prediccion_MCO = pred_mco, 
          Prediccion_Log = expm1(pred_log), 
          IC_Bajo_95_MCO = ic_bajo,
          IC_Alto_95_MCO = ic_alto
        )
      
      # 5. **AGREGACIÓN FINAL**: Agrupar por 'producto' y calcular el promedio
      export_data_agregada <- predicciones_completas %>%
        group_by(producto) %>%
        summarise(
          Real_Promedio = round(mean(Real)),
          Prediccion_MCO_Promedio = round(mean(Prediccion_MCO)),
          Prediccion_Log_Promedio = round(mean(Prediccion_Log)),
          IC_Bajo_95_Promedio = round(mean(IC_Bajo_95_MCO)),
          IC_Alto_95_Promedio = round(mean(IC_Alto_95_MCO)),
          .groups = 'drop' 
        )
      
      # 6. Escribir el archivo CSV
      write.csv(export_data_agregada, file, row.names = FALSE)
    }
  )
  
  output$stock_normality_tests <- renderPrint({
    req(modelo_stock_cv())
    cat("=== Test de Normalidad de los Residuales ===\n\n")
    print(modelo_stock_cv()$shapiro_test)
    cat("\n---\n")
    print(modelo_stock_cv()$jarque_test)
    cat("\n\nInterpretación:\n")
    cat("Si el p-valor < 0.05, los residuos NO siguen una distribución normal.\n")
    cat("Esto puede indicar la presencia de valores atípicos o una mala especificación del modelo.\n")
  })
  
  output$stock_aic_bic <- renderPrint({
    req(modelo_stock_cv())
    cat("=== Comparación de Modelos (AIC / BIC) ===\n")
    print(modelo_stock_cv()$aic_comparison)
    cat("\n")
    print(modelo_stock_cv()$bic_comparison)
    cat("\nInterpretación:\n")
    cat("- Menor AIC o BIC indica un mejor equilibrio entre ajuste y complejidad del modelo.\n")
    cat("- Si el modelo logarítmico tiene AIC/BIC menores, suele ser preferible.\n")
  })
  
  output$stock_f_test <- renderPrint({
    req(modelo_stock_cv())
    cat("=== F-test de Comparación entre Modelos (MCO vs Logarítmico) ===\n")
    print(modelo_stock_cv()$f_test_models)
    cat("\nInterpretación:\n")
    cat("- El F-test compara si el modelo logarítmico mejora significativamente el ajuste.\n")
    cat("- Si el p-valor < 0.05, el modelo logarítmico aporta información adicional relevante.\n")
  })
  
}

# ==================== RUN APP ====================
shinyApp(ui = ui, server = server)