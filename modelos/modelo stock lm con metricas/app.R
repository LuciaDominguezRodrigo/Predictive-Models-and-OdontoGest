# =======================================================
# Shiny App: Modelos base para la aplicación de TFG
# Autor: Lucía Domínguez Rodrigo
# Descripción:
# - Modelo Stock: predice pedidos necesarios de materiales
#   en una clínica dental (Regresión Lineal con CV) - Diagnóstico Completo.
#   *** MODIFICACIÓN: Inclusión de Errores Estándar Robustos (HC3) Y LM LOGARÍTMICO ***
# - Modelo Diagnóstico: predice diagnóstico dental a partir
#   de variables clínicas simuladas (XGBoost) - Diagnóstico Básico.
# =======================================================

# ==================== LIBRERÍAS ====================
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
library(tibble) # Para rownames_to_column (Mejora la gestión de data frames)
library(sandwich) # Para vcovHC (Errores Estándar Robustos)
library(lmtest) # Para coeftest y waldtest (Pruebas robustas)
library(tseries) # Para jarque.bera.test


# ==================== UI ====================
ui <- fluidPage(
  useShinyalert(), 
  titlePanel("Modelos Regresión Lineal y XGBoost -  Modelo de stock"),
  
  tabsetPanel(
    # ==================== MODELO STOCK (LM CV) ====================
    tabPanel("Modelo Stock",
             sidebarLayout(
               sidebarPanel(
                 actionButton("entrenar_stock", "Entrenar Modelo Stock y Diagnósticos"),
                 tags$hr(),
                 h4("Exportar Diagnósticos"),
                 downloadButton("download_diagnostics_stock", "Descargar Diagnósticos por Caso (CSV)")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Pedido Mes Siguiente (MCO vs Log)", tableOutput("pedido_mes_siguiente")),
                   
                   # --- Diagnósticos del Modelo MCO ---
                   tabPanel("Resumen y Métricas (MCO)", 
                            h3("Métricas CV"), verbatimTextOutput("stock_metrics"),
                            tags$hr(),
                            h3("Resumen del Modelo LM (MCO Final)"), verbatimTextOutput("stock_summary"),
                            tags$hr(),
                            h3("Resumen del Modelo LM (Coeficientes Robusto - HC3)"), verbatimTextOutput("stock_robust_summary"),
                            tags$hr(),
                            h3("VIF y Durbin-Watson"), verbatimTextOutput("stock_vif_dw")),
                   
                   # --- Diagnósticos del Modelo Logarítmico (NUEVO) ---
                   tabPanel("Resumen y Métricas (Logarítmico)",
                            h3("Métricas CV (Log)"), verbatimTextOutput("stock_log_metrics"),
                            tags$hr(),
                            h3("Resumen del Modelo LM Logarítmico (Final)"), verbatimTextOutput("stock_log_summary"),
                            tags$hr(),
                            h3("Resumen del Modelo LM Logarítmico (Coeficientes Robusto - HC3)"), verbatimTextOutput("stock_log_robust_summary")),
                   
                   tabPanel("Coeficientes e IC 95% (MCO)", verbatimTextOutput("stock_coefs"), 
                            tags$hr(), h3("Matriz de Varianza-Covarianza"), verbatimTextOutput("stock_vcov")),
                   
                   tabPanel("Análisis de Varianza (ANOVA)", verbatimTextOutput("stock_anova")),
                   
                   # --- Pruebas de normalidad ---
                   tabPanel("Normalidad de Residuales",
                            h3("Pruebas de Normalidad de los Residuales (Modelo MCO)"),
                            verbatimTextOutput("stock_normality_tests")),
                   
                   # --- Comparación entre Modelos ---
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
                   
                   # --- Gráficos ---
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
                   
                   # --- Diagnósticos de Influencia ---
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
    
    # ==================== MODELO DIAGNÓSTICO (XGBOOST) ====================
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
    
    dataset_historico <- data.frame(
      producto = factor(sample(c("Guantes","Mascarillas","Agujas", "cemento"), n, replace = TRUE)),
      stock_inicio = sample(50:500, n, replace = TRUE),
      pacientes = sample(20:80, n, replace = TRUE),
      pedidos_realizados = sample(50:400, n, replace = TRUE)
    )
    
    dataset_historico$pedidos_necesarios <- with(dataset_historico,
                                                 pmax(0, round(pedidos_realizados + 0.35*(100 - stock_inicio) + rnorm(n,0,5)))
    )
    
    dataset_historico$stock_ratio <- dataset_historico$stock_inicio / pmax(1, dataset_historico$pedidos_realizados)
    dataset_historico$pacientes_ratio <- dataset_historico$pacientes / pmax(1, dataset_historico$stock_inicio)
    
    # CORRECCIÓN DE FACTOR: Eliminamos niveles no utilizados del factor
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
    
    # 1. Información de diagnóstico por caso (Mahalanobis fix)
    model_mat <- model.matrix(lm_final)
    model_vars_numeric <- model_mat[, colnames(model_mat) != "(Intercept)"]
    
    diag_case <- augment(lm_final) %>%
      mutate(
        mahalanobis = mahalanobis(model_vars_numeric, 
                                  center = colMeans(model_vars_numeric), 
                                  cov = cov(model_vars_numeric)),
        caso = 1:n()
      ) %>%
      dplyr::select(caso, everything())
    
    # 2. VIF y Durbin-Watson
    vif_res <- car::vif(lm_final)
    dw_res <- car::durbinWatsonTest(lm_final)
    
    # 3. Coeficientes e IC - Uso de backticks correcto
    coefs_ic <- confint(lm_final) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("term") %>% 
      rename(conf.low = `2.5 %`, conf.high = `97.5 %`)
    
    # 4. Matriz Varianza-Covarianza
    vcov_mat <- vcov(lm_final)
    
    # 5. ANOVA
    anova_res <- anova(lm_final)
    
    # 6. Correlaciones parciales
    numeric_vars <- c("stock_inicio", "pacientes", "pedidos_realizados", "stock_ratio", "pacientes_ratio")
    pcor_mat <- tryCatch({
      ppcor::pcor(dataset_historico[, c("pedidos_necesarios", numeric_vars)])$estimate
    }, error = function(e) paste("No se pudo calcular la correlación parcial:", e$message))
    
    # 7. DFBETAS (para ejemplo)
    dfbetas_res <- as.data.frame(dfbetas(lm_final))
    names(dfbetas_res) <- paste0("DfB_", names(dfbetas_res))
    
    # Unir medidas de influencia importantes para tabla resumen (ordenar por Cook's D)
    influence_summary <- diag_case %>% 
      dplyr::select(caso, .fitted, .resid, .hat, .cooksd, .std.resid) %>%
      arrange(desc(.cooksd)) %>%
      head(10)
    
    # 8. Errores estándar robustos (HC3) - LM MCO
    robust_vcov <- vcovHC(lm_final, type = "HC3")
    robust_summary <- coeftest(lm_final, vcov. = robust_vcov)
    
    # 9. Errores estándar robustos (HC3) - LM Logarítmico (NUEVO)
    robust_vcov_log <- vcovHC(lm_log_final, type = "HC3")
    robust_summary_log <- coeftest(lm_log_final, vcov. = robust_vcov_log)
    
    # 10. Normalidad de los residuales (Test Shapiro-Wilk y Jarque-Bera)
    shapiro_test <- shapiro.test(residuals(lm_final))
    jarque_test <- tseries::jarque.bera.test(residuals(lm_final))
    
    # 11. Comparación AIC / BIC / F-test entre modelos (MCO vs Logarítmico)
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
      f_test_models = f_test_models
      
    )
  })
  
  # === Tabla de datos de entrada para la predicción ===
  output$next_month_data_input <- renderTable({
    next_month_data_base <- data.frame(
      producto = factor(c("Guantes","Mascarillas","Agujas", "cemento")),
      stock_inicio = c(50, 120, 80,90),
      pacientes = c(200, 320, 150, 150),
      pedidos_realizados = c(150, 300, 100, 200)
    )
    next_month_data_base
  })
  
  # === Predicción del pedido del mes siguiente (Incluye Logarítmico) ===
  output$pedido_mes_siguiente <- renderTable({
    req(modelo_stock_cv())
    
    # Datos de entrada base
    next_month_data <- data.frame(
      producto = factor(c("Guantes", "Mascarillas", "Agujas", "cemento"),
                        levels = levels(modelo_stock_cv()$dataset$producto)),
      stock_inicio = c(50, 120, 80, 80),
      pacientes = c(200, 320, 150, 200),
      pedidos_realizados = c(150, 300, 100, 200)
    )
    
    next_month_data$stock_ratio <- next_month_data$stock_inicio / pmax(1, next_month_data$pedidos_realizados)
    next_month_data$pacientes_ratio <- next_month_data$pacientes / pmax(1, next_month_data$stock_inicio)
    
    # === PREDICCIÓN MCO ORIGINAL ===
    pred_pedido <- predict(modelo_stock_cv()$modelo, newdata = next_month_data)
    next_month_data$Pedido_Sugerido <- round(pred_pedido)
    
    # === PREDICCIÓN LM LOGARÍTMICO (Reducido) ===
    pred_log <- predict(modelo_stock_cv()$modelo_log, newdata = next_month_data)
    next_month_data$Pedido_Reducido <- round(expm1(pred_log)) # expm1 = exp(x) - 1
    
    # Intervalos de Confianza (basados en RMSE del MCO original)
    rmse_mean <- mean(modelo_stock_cv()$modelo$resample$RMSE)
    t_val <- qt(0.975, df = nrow(modelo_stock_cv()$dataset) - length(coef(modelo_stock_cv()$lm_final)))
    
    margin_error <- t_val * rmse_mean
    
    next_month_data$IC_Bajo_95 <- round(pmax(0, pred_pedido - margin_error))
    next_month_data$IC_Alto_95 <- round(pred_pedido + margin_error)
    
    # Alerta de pedido alto (basada en el MCO original)
    productos_altos <- next_month_data$producto[next_month_data$Pedido_Sugerido > 400]
    if (length(productos_altos) > 0) {
      shinyalert(
        title = "⚠️ Alerta de pedido alto",
        text = paste("Se sugiere un pedido alto de:", paste(productos_altos, collapse = ", "), 
                     "\n(Modelo MCO). Considere la recomendación Logarítmica."),
        type = "warning"
      )
    }
    
    # Devolver tabla con la comparación
    next_month_data[, c("producto", "Pedido_Sugerido", "Pedido_Reducido", "IC_Bajo_95", "IC_Alto_95")]
  })
  
  
  # ==================== PESTAÑA 1: LM MCO ORIGINAL ====================
  
  # === Métricas CV (MCO) ===
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
  
  # === Resumen del Modelo LM (MCO final) ===
  output$stock_summary <- renderPrint({
    req(modelo_stock_cv())
    summary(modelo_stock_cv()$lm_final)
  })
  
  # === Resumen del Modelo LM (Coeficientes Robusto - HC3) MCO ===
  output$stock_robust_summary <- renderPrint({
    req(modelo_stock_cv())
    cat("Modelo LM MCO: Resumen con Errores Estándar Robustos (HC3)\n")
    cat("Los coeficientes (Estimate) son idénticos al modelo MCO original. \n")
    cat("La diferencia está en los errores estándar (Std. Error) y los p-values (Pr(>|t|)), \n")
    cat("los cuales están corregidos por la heterocedasticidad (varianza no constante de los residuales).\n\n")
    print(modelo_stock_cv()$robust_summary)
  })
  
  # ==================== PESTAÑA 2: LM LOGARÍTMICO (NUEVO) ====================
  
  # === Métricas CV (Logarítmico) ===
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
  
  # === Resumen del Modelo LM (Logarítmico final) ===
  output$stock_log_summary <- renderPrint({
    req(modelo_stock_cv())
    summary(modelo_stock_cv()$lm_log_final)
  })
  
  # === Resumen del Modelo LM (Coeficientes Robusto - HC3) Logarítmico ===
  output$stock_log_robust_summary <- renderPrint({
    req(modelo_stock_cv())
    cat("Modelo LM Logarítmico: Resumen con Errores Estándar Robustos (HC3)\n\n")
    print(modelo_stock_cv()$robust_summary_log)
  })
  
  # ==================== PESTAÑAS RESTANTES (MCO) ====================
  
  # === VIF y Durbin-Watson (MCO) ===
  output$stock_vif_dw <- renderPrint({
    req(modelo_stock_cv())
    
    cat("=== Factor de Inflación de la Varianza (VIF) y Tolerancia (1/VIF) ===\n")
    vif_df <- data.frame(VIF = modelo_stock_cv()$vif_res, Tolerancia = 1/modelo_stock_cv()$vif_res)
    print(vif_df)
    
    cat("\n=== Prueba de Durbin-Watson (Autocorrelación de Residuales) ===\n")
    print(modelo_stock_cv()$dw_res)
  })
  
  # === Coeficientes e IC 95% (MCO)
  output$stock_coefs <- renderPrint({
    req(modelo_stock_cv())
    
    cat("=== Coeficientes de Regresión e Intervalos de Confianza (95%) NO ROBUSTOS (MCO) ===\n")
    cat("Advertencia: Estos IC son ineficientes e incorrectos debido a la heterocedasticidad. \n")
    cat("Use el resumen Robusto (HC3) en la pestaña 'Resumen y Métricas'.\n\n")
    
    # 1. Obtenemos el resumen tidy (con columna 'term')
    coefs_summary <- tidy(modelo_stock_cv()$lm_final)
    
    # 2. Obtenemos los ICs (ya preparados en modelo_stock_cv)
    ics_df <- modelo_stock_cv()$coefs_ic 
    
    # 3. Unimos por la columna 'term'
    coefs_summary <- left_join(coefs_summary, ics_df, by = "term")
    
    print(coefs_summary)
  })
  
  # === Matriz de Varianza-Covarianza (MCO) ===
  output$stock_vcov <- renderPrint({
    req(modelo_stock_cv())
    cat("=== Matriz de Varianza-Covarianza NO ROBUSTA (MCO) ===\n")
    print(modelo_stock_cv()$vcov_mat)
  })
  
  # === ANOVA (MCO) ===
  output$stock_anova <- renderPrint({
    req(modelo_stock_cv())
    
    cat("=== Tabla de Análisis de Varianza (ANOVA) NO ROBUSTA ===\n")
    cat("Advertencia: La prueba F global es incorrecta. Use el resumen Robusto para la significancia de coeficientes.\n\n")
    print(modelo_stock_cv()$anova_res)
  })
  
  # === Correlaciones Parciales (MCO) ===
  output$stock_pcor <- renderPrint({
    req(modelo_stock_cv())
    cat("=== Matriz de Correlación Parcial (Entre variables numéricas y dependiente MCO) ===\n")
    print(modelo_stock_cv()$pcor_mat)
  })
  
  # === Gráfico Matriz de Correlación ===
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
  
  # === Gráfico Predicción vs Real por Fold (MCO) ===
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
  
  # === Gráfico Residuales vs Ajustados (MCO) ===
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
  
  # === Gráfico Q-Q (Normalidad) (MCO) ===
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
  
  # === Histograma de Residuales (MCO) ===
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
  
  # === Gráficos Parciales (car::crPlots) (MCO) ===
  output$plot_cr <- renderPlot({
    req(modelo_stock_cv())
    # Genera los gráficos de Componentes y Residuales para el modelo LM
    car::crPlots(modelo_stock_cv()$lm_final, ask = FALSE)
  })
  
  # === Tabla de Influencia (Top 10 Cook's D) (MCO) ===
  output$stock_influence_table <- renderTable({
    req(modelo_stock_cv())
    # .hat = Apalancamiento, .cooksd = Distancia de Cook
    modelo_stock_cv()$influence %>%
      rename(
        Ajustado = .fitted,
        Residual = .resid,
        Apalancamiento = .hat,
        Cook_D = .cooksd,
        Resid_Estandarizado = .std.resid
      )
  })
  
  # === Ejemplo de DfBetas (MCO) ===
  output$stock_dfbetas_example <- renderPrint({
    req(modelo_stock_cv())
    
    cat("=== DfBetas (Cambio en Coeficientes al Eliminar una Observación) ===\n")
    cat("Muestra los DfBetas para las primeras 5 observaciones:\n\n")
    print(head(modelo_stock_cv()$dfbetas_res, 5))
    cat("\nNota: DfBetas > 1 (o 2/sqrt(n)) indica un caso con alta influencia en ese coeficiente.")
  })
  
  # === Descargar Diagnósticos por Caso (MCO) ===
  output$download_diagnostics_stock <- downloadHandler(
    filename = function() { paste0('diagnostico_stock_', Sys.Date(), '.csv') },
    content = function(file) {
      req(modelo_stock_cv())
      write.csv(modelo_stock_cv()$diag_case, file, row.names = FALSE)
    }
  )
  
  # === Normalidad de los residuales (MCO) ===
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
  
  # === Comparación AIC/BIC (MCO vs Logarítmico) ===
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
  
  # === F-test de comparación entre modelos ===
  output$stock_f_test <- renderPrint({
    req(modelo_stock_cv())
    cat("=== F-test de Comparación entre Modelos (MCO vs Logarítmico) ===\n")
    print(modelo_stock_cv()$f_test_models)
    cat("\nInterpretación:\n")
    cat("- El F-test compara si el modelo logarítmico mejora significativamente el ajuste.\n")
    cat("- Si el p-valor < 0.05, el modelo logarítmico aporta información adicional relevante.\n")
  })
  
  
  # ==================== MODELO DIAGNÓSTICO XGBOOST ====================
  
  # El código del Modelo Diagnóstico XGBoost permanece inalterado.
  modelo_diag <- eventReactive(input$entrenar_diag, {
    set.seed(123)
    n <- 3000
    
    # ==================== Generar dataset ====================
    pacientes <- data.frame(
      edad = sample(10:80, n, replace = TRUE),
      sexo = factor(sample(c("M", "F"), n, replace = TRUE)),
      dolor = runif(n, 0, 10),
      inflamacion_gums = factor(sample(c("No", "Si", "Severa"), n, replace = TRUE, prob = c(0.5, 0.4, 0.1))),
      historial_caries = factor(sample(c("No", "Si"), n, replace = TRUE, prob = c(0.4, 0.6))),
      numero_dientes = sample(20:32, n, replace = TRUE),
      tipo_sintoma = factor(sample(c("dolor sordo", "dolor agudo", "sensibilidad", "fractura"), n, replace = TRUE)),
      nivel_hacinamiento = runif(n, 0, 5),
      presencia_quiste = factor(sample(c("No","Si"), n, replace = TRUE, prob = c(0.95,0.05))),
      historial_tratamiento = factor(sample(c("Ninguno","Empastes","Endodoncia_previa"), n, replace = TRUE, prob = c(0.6,0.3,0.1)))
    )
    
    diagnostico <- rep(NA, n)
    diagnostico[pacientes$dolor < 3 & pacientes$inflamacion_gums == "No" & pacientes$historial_caries == "No"] <- "Limpieza"
    diagnostico[pacientes$dolor >= 7 & pacientes$historial_caries == "Si" & pacientes$tipo_sintoma == "dolor agudo"] <- "Endodoncia"
    diagnostico[pacientes$edad < 25 & pacientes$nivel_hacinamiento >= 3] <- "Ortodoncia"
    diagnostico[pacientes$presencia_quiste == "Si" | pacientes$tipo_sintoma == "fractura" | pacientes$dolor > 8 | pacientes$inflamacion_gums == "Severa"] <- "Cirugia"
    diagnostico[is.na(diagnostico)] <- sample(c("Limpieza","Endodoncia","Ortodoncia","Cirugia"),
                                              sum(is.na(diagnostico)), replace = TRUE)
    pacientes$diagnostico <- factor(diagnostico, levels = c("Limpieza","Endodoncia","Ortodoncia","Cirugia"))
    
    # Features adicionales
    pacientes$edad_dolor <- pacientes$edad * pacientes$dolor
    pacientes$hacinamiento_sintoma <- pacientes$nivel_hacinamiento * as.numeric(pacientes$tipo_sintoma)
    pacientes$inflamacion_grave <- ifelse(pacientes$inflamacion_gums=="Severa",1,0)
    pacientes$quiste <- ifelse(pacientes$presencia_quiste=="Si",1,0)
    
    # Balancear clases
    set.seed(123)
    pacientes_bal <- upSample(x = pacientes[, setdiff(names(pacientes), "diagnostico")],
                              y = pacientes$diagnostico)
    pacientes_bal$diagnostico <- pacientes_bal$Class
    pacientes_bal$Class <- NULL
    
    # Train/Test
    trainIndex <- createDataPartition(pacientes_bal$diagnostico, p = 0.8, list = FALSE)
    trainData <- pacientes_bal[trainIndex, ]
    testData <- pacientes_bal[-trainIndex, ]
    
    # Matriz sparse para XGBoost
    train_mat <- sparse.model.matrix(diagnostico ~ .-1, data = trainData)
    test_mat <- sparse.model.matrix(diagnostico ~ .-1, data = testData)
    label_train <- as.numeric(trainData$diagnostico) - 1
    label_test <- as.numeric(testData$diagnostico) - 1
    
    dtrain <- xgb.DMatrix(data = train_mat, label = label_train)
    dtest <- xgb.DMatrix(data = test_mat, label = label_test)
    
    params <- list(
      objective = "multi:softprob",
      num_class = length(levels(pacientes$diagnostico)),
      eval_metric = "mlogloss",
      eta = 0.05,
      max_depth = 6
    )
    
    modelo <- xgb.train(params = params, data = dtrain, nrounds = 200, watchlist = list(train=dtrain), verbose=0)
    
    list(modelo = modelo, dtest = dtest, test_labels = label_test, testData = testData, levels = levels(pacientes$diagnostico))
  })
  
  # === Gráfico de importancia XGBoost ===
  output$varImp_diag <- renderPlot({
    req(modelo_diag())
    importance <- xgb.importance(model = modelo_diag()$modelo)
    xgb.plot.importance(importance_matrix = importance, top_n = 10, rel_to_first = TRUE, main = "Importancia de Variables (Gain)")
  })
  
  # === Matriz de confusión XGBoost (Texto) ===
  output$cm_diag_text <- renderPrint({
    req(modelo_diag())
    
    pred_prob <- predict(modelo_diag()$modelo, newdata = modelo_diag()$dtest)
    pred_matrix <- matrix(pred_prob, ncol = length(modelo_diag()$levels), byrow = TRUE)
    pred_labels <- max.col(pred_matrix) - 1 # 0-indexed
    
    cm <- caret::confusionMatrix(factor(pred_labels, levels=0:3, labels=modelo_diag()$levels),
                                 modelo_diag()$testData$diagnostico)
    print(cm)
  })
  
  # === Matriz de confusión XGBoost (Gráfico) ===
  output$cm_diag_plot <- renderPlot({
    req(modelo_diag())
    
    pred_prob <- predict(modelo_diag()$modelo, newdata = modelo_diag()$dtest)
    pred_matrix <- matrix(pred_prob, ncol = length(modelo_diag()$levels), byrow = TRUE)
    pred_labels <- max.col(pred_matrix) - 1
    
    cm <- caret::confusionMatrix(factor(pred_labels, levels=0:3, labels=modelo_diag()$levels),
                                 modelo_diag()$testData$diagnostico)
    
    cm_melt <- melt(cm$table)
    
    ggplot(cm_melt, aes(Reference, Prediction, fill=value)) +
      geom_tile() +
      geom_text(aes(label=value), color="white", size=5) +
      scale_fill_gradient(low="lightblue", high="darkred") +
      labs(title = "Matriz de Confusión - Diagnóstico (XGBoost)", 
           x = "Real", y = "Predicción", fill = "Conteo") +
      theme_minimal()
  })
  
  # === Descargar Predicciones (XGBoost) ===
  output$download_diag_predictions <- downloadHandler(
    filename = function() { paste0('predicciones_diag_test_', Sys.Date(), '.csv') },
    content = function(file) {
      req(modelo_diag())
      
      pred_prob <- predict(modelo_diag()$modelo, newdata = modelo_diag()$dtest)
      pred_matrix <- matrix(pred_prob, ncol = length(modelo_diag()$levels), byrow = TRUE)
      pred_labels <- max.col(pred_matrix) - 1
      pred_class <- factor(pred_labels, levels=0:3, labels=modelo_diag()$levels)
      
      export_data <- modelo_diag()$testData
      export_data$Prediccion_XGBoost <- pred_class
      
      # Añadir probabilidades de clase
      colnames(pred_matrix) <- paste0("Prob_", modelo_diag()$levels)
      export_data <- cbind(export_data, pred_matrix)
      
      write.csv(export_data, file, row.names = FALSE)
    }
  )
}


# ==================== RUN APP ====================
shinyApp(ui = ui, server = server)