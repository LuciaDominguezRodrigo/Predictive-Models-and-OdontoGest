library(shiny)
library(caret)
library(xgboost)
library(pdp)
library(ggplot2)
library(DT)
library(doParallel)
library(pROC)
library(dplyr)
library(fastshap)
library(plotly)
library(data.table)

# -----------------------
# 1. Generar dataset complejo (Simulación Clínica)
# -----------------------
generar_dataset_complejo <- function(n) {
  set.seed(123)
  # Variables Base
  Edad <- rnorm(n, 50, 15)
  Edad <- pmax(18, pmin(95, Edad))
  
  IMC <- rnorm(n, 27 + 0.05 * Edad, 4)
  Glucosa <- rnorm(n, 90 + 0.4 * IMC, 15)
  HbA1c <- 4.5 + 0.03 * Glucosa + rnorm(n, 0, 0.5)
  Presion <- rnorm(n, 110 + 0.3 * Edad, 10)
  Colesterol <- rnorm(n, 180 + 0.2 * IMC, 20)
  FC <- rnorm(n, 70, 10)
  
  # Variables ruidosas o secundarias
  Resistencia <- 0.5 * IMC + 0.03 * Glucosa + rnorm(n, 0, 2)
  Colesterol_eff <- Colesterol + 0.5 * Edad + rnorm(n, 0, 10)
  Inflam <- rnorm(n, 0.02 * Glucosa + 0.03 * IMC, 2)
  
  # Fórmula del Score Latente
  score <- 0.02*Edad + 0.1*IMC + 0.15*Glucosa + 0.2*HbA1c +
    0.05*Edad*Glucosa +            
    0.1*(IMC^2) -                  
    0.07*(Glucosa*IMC) +
    0.1*Presion + 0.1*Colesterol + 0.1*Inflam + 0.2*Resistencia +
    rnorm(n, 0, 8)                
  
  # Creación de etiquetas
  Diagnostico <- cut(
    score,
    breaks = quantile(score, probs = seq(0, 1, length.out = 5)),
    labels = c("norma", "leve", "moderado", "severo"),
    include.lowest = TRUE
  )
  
  data.frame(
    Edad, IMC, Glucosa, HbA1c, Presion, Colesterol, FC,
    Resistencia, Colesterol_eff, Inflam, Diagnostico
  )
}

# -----------------------
# 2. Ingeniería de Características
# -----------------------
aplicar_feature_engineering <- function(df) {
  df %>%
    mutate(
      Interaccion_Edad_Glu = Edad * Glucosa,
      Edad_Senior = as.numeric(Edad > 50),
      IMC_Squared = IMC^2,
      Ratio_Glu_HbA1c = ifelse(HbA1c > 0, Glucosa / HbA1c, 0)
    )
}

# -----------------------
# Función segura para entrenar
# -----------------------
train_safe <- function(expr) {
  tryCatch({
    result <- expr
    list(result = result, error = NULL)
  }, error = function(e) {
    list(result = NULL, error = e)
  })
}

# -----------------------
# UI
# -----------------------
ui <- fluidPage(
  titlePanel("Análisis XGBoost Avanzado (TFG) - Optimización y SHAP"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Configuración del Experimento"),
      numericInput("n_obs", "Número de pacientes (n):", 3000, min = 500, max = 20000),
      actionButton("generar", "1. Generar Datos & Feature Eng.", class = "btn-primary"),
      hr(),
      h4("Entrenamiento del Modelo"),
      helpText("Aplica búsqueda aleatoria de hiperparámetros y pesos de clase para corregir desbalance leve/moderado."),
      actionButton("entrenar", "2. Entrenar XGBoost Optimizado", class = "btn-success"),
      hr(),
      h4("Interpretabilidad (XAI)"),
      uiOutput("select_shap_var_ui"),
      hr(),
      h4("Estado del Sistema:"),
      verbatimTextOutput("estado", placeholder = TRUE),
      tags$style(type="text/css", "#estado { font-size: 11px; color: #333; }")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Datos Procesados", 
                 h5("Primeras 100 filas con nuevas variables generadas:"),
                 DTOutput("tabla_datos")),
        
        tabPanel("Métricas Completas TFG", 
                 verbatimTextOutput("resultados"),
                 helpText("Incluye análisis de Overfitting, Estabilidad (CV) y F1-Score.")),
        
        tabPanel("Calibración y Fiabilidad", 
                 h5("Curva de Calibración (Reliability Diagram)"),
                 plotOutput("plot_calibracion"),
                 h5("Métrica de Fiabilidad"),
                 verbatimTextOutput("metricas_calibracion"),
                 helpText("Muestra si las probabilidades predichas (e.g., 80% riesgo) coinciden con la realidad clínica.")),
        # -------------------------------------
        
        tabPanel("Importancia Global", 
                 plotOutput("plot_importancia"),
                 helpText("Comparativa de ganancia de información (Gain).")),
        
        tabPanel("Errores de Predicción", 
                 DTOutput("tabla_errores")),
        
        tabPanel("PDP Interactivo", 
                 plotlyOutput("plot_pdp"),
                 helpText("Efecto marginal de una variable manteniendo el resto constante.")),
        
        tabPanel("SHAP - Importancia Local", 
                 plotlyOutput("plot_shap_importance"),
                 helpText("Impacto medio absoluto en la probabilidad de la clase 'severo'.")),
        
        tabPanel("SHAP - Dependencia", 
                 plotlyOutput("plot_shap_beeswarm"),
                 helpText("Relación detallada entre valor de variable e impacto SHAP."))
      )
    )
  )
)

# -----------------------
# SERVER
# -----------------------
server <- function(input, output, session) {
  
  estado <- reactiveVal("Sistema listo. Genere los datos primero.")
  
  # Generación y Procesamiento de Datos
  datos <- eventReactive(input$generar, {
    estado("Generando datos sintéticos...")
    raw_data <- generar_dataset_complejo(input$n_obs)
    
    estado("Aplicando Ingeniería de Características (Interaction, Binning, Ratios)...")
    proc_data <- aplicar_feature_engineering(raw_data)
    
    estado(paste("Dataset listo con", nrow(proc_data), "filas y", ncol(proc_data), "variables."))
    proc_data
  })
  
  output$tabla_datos <- renderDT({
    req(datos())
    datatable(head(datos(), 100), options = list(scrollX = TRUE))
  })
  
  # Helper: predict_prob_safe
  predict_prob_safe <- function(model, newdata) {
    p <- tryCatch({
      predict(model, newdata = newdata, type = "prob")
    }, error = function(e) { NULL })
    if (is.null(p)) return(NULL)
    p_df <- as.data.frame(p)
    if (is.null(colnames(p_df))) colnames(p_df) <- paste0("V", seq_len(ncol(p_df)))
    return(p_df)
  }
  
  # Entrenamiento Avanzado
  mod_xgb <- eventReactive(input$entrenar, {
    req(datos())
    df <- datos()
    estado("Iniciando pipeline de entrenamiento...")
    
    # 1. Partición
    trainIndex <- createDataPartition(df$Diagnostico, p = 0.8, list = FALSE)
    trainData <- df[trainIndex, ]
    testData <- df[-trainIndex, ]
    
    niveles_obj <- c("norma", "leve", "moderado", "severo")
    trainData$Diagnostico <- factor(trainData$Diagnostico, levels = niveles_obj)
    testData$Diagnostico  <- factor(testData$Diagnostico,  levels = niveles_obj)
    
    # 2. Estrategia de Pesos
    weights_vector <- ifelse(trainData$Diagnostico %in% c("leve", "moderado"), 2.0, 1.0)
    
    estado("Configurando Cross-Validation y Random Search...")
    
    ctrl <- trainControl(
      method = "cv", 
      number = 5, 
      classProbs = TRUE, 
      allowParallel = TRUE,
      summaryFunction = multiClassSummary, 
      search = "random", 
      verboseIter = FALSE
    )
    
    estado("Entrenando XGBoost con optimización de hiperparámetros (esto puede tardar)...")
    
    cl <- makeCluster(max(1, detectCores() - 1))
    registerDoParallel(cl)
    
    modelo <- train_safe(
      train(
        Diagnostico ~ ., 
        data = trainData, 
        method = "xgbTree", 
        trControl = ctrl,
        weights = weights_vector, 
        tuneLength = 10, 
        metric = "Kappa" 
      )
    )
    stopCluster(cl)
    
    if (!is.null(modelo$error)) {
      estado(paste("Error crítico:", modelo$error$message))
      return(NULL)
    }
    
    estado("Entrenamiento finalizado. Evaluando en Test set...")
    pred_probs <- predict_prob_safe(modelo$result, newdata = testData)
    
    # 4. Cálculo de SHAP
    estado("Calculando explicaciones SHAP...")
    Xtrain <- trainData %>% select(-Diagnostico)
    
    pred_wrapper_severo <- function(object, newdata) {
      p <- predict_prob_safe(object, newdata = newdata)
      if (is.null(p) || !"severo" %in% colnames(p)) return(rep(NA, nrow(newdata)))
      return(as.numeric(p[["severo"]]))
    }
    
    set.seed(123)
    nsamp <- min(300, nrow(Xtrain))
    X_explain <- Xtrain %>% sample_n(nsamp)
    
    shap_res <- tryCatch({
      fastshap::explain(
        object = modelo$result,
        X = as.data.frame(X_explain),
        pred_wrapper = pred_wrapper_severo,
        nsim = 30,
        adjust = TRUE
      )
    }, error = function(e) { NULL })
    
    if (!is.null(shap_res) && is.null(dim(shap_res))) {
      shap_res <- NULL 
    } else if (!is.null(shap_res)) {
      colnames(shap_res) <- colnames(X_explain)
    }
    
    estado("Proceso completado con éxito.")
    
    list(modelo = modelo, trainData = trainData, testData = testData,
         pred_probs = pred_probs, shap_sample = shap_res, X_explain = X_explain)
  })
  
  output$select_shap_var_ui <- renderUI({
    req(mod_xgb())
    if (is.null(mod_xgb()$X_explain)) return(NULL)
    vars <- colnames(mod_xgb()$X_explain)
    selectInput("shap_var", "Variable para análisis detallado:", choices = vars, selected = vars[1])
  })
  
  # ---  Resultados y Métricas Completas (Robustez, Overfitting, F1) ---
  output$resultados <- renderPrint({
    req(mod_xgb())
    modobj <- mod_xgb()
    if(is.null(modobj)) return("Hubo un error en el modelado.")
    
    mod <- modobj$modelo$result
    testData <- modobj$testData
    trainData <- modobj$trainData
    
    # 1. ANÁLISIS DE TIEMPO
    cat("=== 1. EFICIENCIA COMPUTACIONAL ===\n")
    if(!is.null(mod$times$everything)) {
      cat("Tiempo total de entrenamiento:", round(mod$times$everything[3], 2), "segundos.\n")
    } else {
      cat("Datos de tiempo no disponibles en el objeto caret.\n")
    }
    
    cat("\n--- MEJORES HIPERPARÁMETROS (Random Search) ---\n")
    print(mod$bestTune)
    
    # 2. ANÁLISIS DE OVERFITTING
    acc_train <- max(mod$results$Accuracy) # Mejor accuracy en CV
    pred_test <- predict(mod, newdata = testData)
    cm <- confusionMatrix(pred_test, testData$Diagnostico)
    acc_test <- cm$overall["Accuracy"]
    
    cat("\n=== 2. ANÁLISIS DE ROBUSTEZ (OVERFITTING) ===\n")
    cat(sprintf("Accuracy en Entrenamiento (CV): %.4f\n", acc_train))
    cat(sprintf("Accuracy en Test (Validación):  %.4f\n", acc_test))
    diff_acc <- acc_train - acc_test
    if(diff_acc > 0.05) {
      cat("(!) ALERTA: Posible Overfitting (Diferencia > 5%).\n")
    } else {
      cat("(OK) El modelo generaliza bien (Diferencia baja).\n")
    }
    
    # 3. ESTABILIDAD
    cat("\n=== 3. ESTABILIDAD DEL MODELO (CV SD) ===\n")
    if(!is.null(mod$resample)) {
      sd_kappa <- sd(mod$resample$Kappa)
      cat("Desviación Estándar Kappa (5-Folds):", round(sd_kappa, 4), "\n")
      if(sd_kappa < 0.05) cat("(OK) Modelo estable.\n") else cat("(!) Cierta inestabilidad.\n")
    }
    
    cat("\n--- MATRIZ DE CONFUSIÓN (TEST SET) ---\n")
    print(cm$table)
    
    cat("\n=== 4. MÉTRICAS DETALLADAS POR CLASE (Incluye F1-Score) ===\n")
    print(cm$byClass[, c("Sensitivity", "Specificity", "F1", "Balanced Accuracy")])
    
    # AUC
    if (!is.null(modobj$pred_probs)) {
      roc_multi <- tryCatch(multiclass.roc(testData$Diagnostico, modobj$pred_probs), error=function(e) NULL)
      if(!is.null(roc_multi)) {
        cat("\n--- AUC MULTICLASE: ", round(auc(roc_multi), 4), " ---\n")
      }
    }
  })
  
  # ---Gráfico de Calibración ---
  output$plot_calibracion <- renderPlot({
    req(mod_xgb())
    modobj <- mod_xgb()
    testData <- modobj$testData
    
    # Probabilidades para severo
    prob_severo <- modobj$pred_probs[,"severo"]
    real_severo <- ifelse(testData$Diagnostico == "severo", 1, 0)
    
    df_cal <- data.frame(prob = prob_severo, real = real_severo)
    df_cal$bin <- cut(df_cal$prob, breaks = seq(0, 1, 0.1), include.lowest = TRUE)
    
    cal_data <- df_cal %>%
      group_by(bin) %>%
      summarise(
        mean_prob = mean(prob),
        obs_rate = mean(real),
        n = n(),
        .groups = 'drop'
      )
    
    ggplot(cal_data, aes(x = mean_prob, y = obs_rate)) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
      geom_line(color = "#2980b9", size = 1) +
      geom_point(aes(size = n), color = "#2980b9") +
      labs(title = "Curva de Calibración (Clase: Severo)",
           subtitle = "Diagonal gris = Calibración Perfecta",
           x = "Probabilidad Predicha Promedio",
           y = "Fracción Real de Positivos",
           size = "Nº Pacientes") +
      xlim(0, 1) + ylim(0, 1) +
      theme_minimal()
  })
  
  # --- NUEVO: Métricas de Calibración (Brier Score) ---
  output$metricas_calibracion <- renderPrint({
    req(mod_xgb())
    prob_severo <- mod_xgb()$pred_probs[,"severo"]
    real_severo <- ifelse(mod_xgb()$testData$Diagnostico == "severo", 1, 0)
    
    brier <- mean((prob_severo - real_severo)^2)
    
    cat("--- BRIER SCORE (Clase Severo) ---\n")
    cat(round(brier, 4), "\n")
    cat("Interpretación: Cuanto más cerca de 0, mejor calibrado está el modelo.\n")
    if(brier < 0.10) cat("Evaluación: Excelente calibración clínica.\n")
    else if(brier < 0.20) cat("Evaluación: Calibración aceptable.\n")
    else cat("Evaluación: El modelo necesita recalibración.\n")
  })
  
  # Gráfico de Importancia (Gain) - SIN CAMBIOS
  output$plot_importancia <- renderPlot({
    req(mod_xgb())
    mod <- mod_xgb()$modelo$result
    imp <- varImp(mod)$importance
    imp$Variable <- rownames(imp)
    imp <- imp %>% arrange(desc(Overall)) %>% head(15)
    
    ggplot(imp, aes(x = reorder(Variable, Overall), y = Overall)) +
      geom_col(fill = "#2c3e50") +
      coord_flip() +
      labs(title = "Importancia de Variables (Gain)", 
           subtitle = "Incluye nuevas variables de ingeniería",
           x = NULL, y = "Importancia Relativa") +
      theme_minimal() +
      theme(text = element_text(size=12))
  })
  
  # Errores
  output$tabla_errores <- renderDT({
    req(mod_xgb())
    modobj <- mod_xgb()
    testData <- modobj$testData
    mod <- modobj$modelo$result
    
    testData$Prediccion <- predict(mod, newdata = testData)
    testData$Prob_Severo <- round(predict(mod, newdata = testData, type="prob")[,"severo"], 3)
    
    errores <- testData[testData$Diagnostico != testData$Prediccion, ]
    errores <- errores %>% select(Diagnostico, Prediccion, Prob_Severo, Edad, Glucosa, everything())
    datatable(errores, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # PDP 
  output$plot_pdp <- renderPlotly({
    req(mod_xgb())
    modobj <- mod_xgb()
    mod <- modobj$modelo$result
    trainData <- modobj$trainData
    
    imp <- varImp(mod)$importance
    top_var <- rownames(imp)[order(imp$Overall, decreasing = T)][1]
    
    pd <- tryCatch({
      partial(mod, pred.var = top_var, train = trainData, which.class = "severo", prob = TRUE)
    }, error = function(e) NULL)
    
    if (is.null(pd)) return(NULL)
    
    p <- ggplot(pd, aes(x = .data[[top_var]], y = yhat)) +
      geom_line(color = "#e74c3c", size = 1) +
      geom_point(size = 1.5) +
      labs(title = paste("PDP: Efecto de", top_var, "en prob. Severo"),
           y = "Probabilidad (Logit/Prob)", x = top_var) +
      theme_light()
    
    ggplotly(p)
  })
  
  # SHAP Importancia 
  output$plot_shap_importance <- renderPlotly({
    req(mod_xgb())
    shap_vals <- mod_xgb()$shap_sample
    if(is.null(shap_vals)) return(NULL)
    
    mean_shap <- colMeans(abs(shap_vals), na.rm = TRUE)
    df_imp <- data.frame(Variable = names(mean_shap), MeanSHAP = mean_shap) %>%
      arrange(desc(MeanSHAP)) %>%
      head(15)
    
    p <- ggplot(df_imp, aes(x = reorder(Variable, MeanSHAP), y = MeanSHAP)) +
      geom_col(fill = "#8e44ad") +
      coord_flip() +
      labs(title = "Impacto Medio SHAP (Clase Severo)", x = "", y = "Media |SHAP|") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # SHAP Beeswarm 
  output$plot_shap_beeswarm <- renderPlotly({
    req(mod_xgb())
    modobj <- mod_xgb()
    shap_vals <- as.data.frame(modobj$shap_sample)
    X <- modobj$X_explain
    var <- input$shap_var
    
    if(is.null(shap_vals) || is.null(var) || !(var %in% names(shap_vals))) return(NULL)
    
    df_plot <- data.frame(val = X[[var]], shap = shap_vals[[var]])
    
    p <- ggplot(df_plot, aes(x = val, y = shap, 
                             text = paste("Valor:", round(val, 2), "<br>SHAP:", round(shap, 3)))) +
      geom_point(aes(color = shap), alpha = 0.7) +
      scale_color_gradient2(low = "blue", mid = "gray", high = "red") +
      geom_smooth(method = "loess", color = "black", linetype = "dashed", se = FALSE, size=0.5) +
      labs(title = paste("Dependencia SHAP:", var), x = var, y = "Impacto SHAP") +
      theme_light()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$estado <- renderText(estado())
}

shinyApp(ui, server)