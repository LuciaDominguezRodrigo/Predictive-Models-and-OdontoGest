# =======================================================
# Shiny App: Modelos base para la aplicación de TFG
# Autor: Lucía Domínguez Rodrigo
# Descripción: 
#   - Modelo Stock: predice pedidos necesarios de materiales 
#     en una clínica dental (Regresión Lineal con CV).
#   - Modelo Diagnóstico: predice diagnóstico dental a partir 
#     de variables clínicas simuladas (XGBoost).
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

# ==================== UI ====================
ui <- fluidPage(
  useShinyalert(),
  titlePanel("Modelos Regresión Lineal y XGBoost - Clínica Dental"),
  
  tabsetPanel(
    # ==================== MODELO STOCK ====================
    tabPanel("Modelo Stock",
             sidebarLayout(
               sidebarPanel(
                 actionButton("entrenar_stock", "Entrenar modelo Stock")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Pedido mes siguiente", tableOutput("pedido_mes_siguiente")),
                   tabPanel("Métricas CV", verbatimTextOutput("stock_metrics")),
                   tabPanel("Predicción vs Real por Fold", plotOutput("plot_cv_stock", height = "400px", width = "100%"))
                 )
               )
             )
    ),
    
    # ==================== MODELO DIAGNÓSTICO ====================
    tabPanel("Modelo Diagnóstico",
             sidebarLayout(
               sidebarPanel(
                 actionButton("entrenar_diag", "Entrenar modelo Diagnóstico")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Error OOB / Importancia", plotOutput("varImp_diag", height = "400px", width = "100%")),
                   tabPanel("Matriz de confusión", verbatimTextOutput("cm_diag_text"), plotOutput("cm_diag_plot", height = "400px", width = "100%"))
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
      producto = factor(sample(c("Guantes","Mascarillas","Agujas"), n, replace = TRUE)),
      stock_inicio = sample(50:500, n, replace = TRUE),
      pacientes = sample(20:80, n, replace = TRUE),
      pedidos_realizados = sample(50:400, n, replace = TRUE)
    )
    
    dataset_historico$pedidos_necesarios <- with(dataset_historico,
                                                 pmax(0, round(pedidos_realizados + 0.35*(100 - stock_inicio) + rnorm(n,0,5)))
    )
    
    dataset_historico$stock_ratio <- dataset_historico$stock_inicio / pmax(1, dataset_historico$pedidos_realizados)
    dataset_historico$pacientes_ratio <- dataset_historico$pacientes / pmax(1, dataset_historico$stock_inicio)
    
    # === Train LM con CV 5-fold ===
    ctrl <- trainControl(method = "cv", number = 5, savePredictions = "final")
    
    modelo <- train(
      pedidos_necesarios ~ stock_inicio + pacientes + pedidos_realizados + producto + stock_ratio + pacientes_ratio,
      data = dataset_historico,
      method = "lm",
      trControl = ctrl,
      metric = "RMSE"
    )
    
    list(modelo = modelo, dataset = dataset_historico)
  })
  
  # === Métricas CV ===
  output$stock_metrics <- renderPrint({
    req(modelo_stock_cv())
    
    modelo <- modelo_stock_cv()$modelo
    df_res <- modelo$resample
    
    cat("=== Métricas del modelo de Stock (CV) ===\n")
    cat("MAE:", round(mean(df_res$MAE),2), "\n")
    cat("RMSE:", round(mean(df_res$RMSE),2), "\n")
    cat("R²:", round(mean(df_res$Rsquared),3), "\n\n")
    
    cat("=== Métricas de CV (caret) ===\n")
    print(df_res)
  })
  
  # === Gráfico Predicción vs Real por Fold ===
  output$plot_cv_stock <- renderPlot({
    req(modelo_stock_cv())
    df_preds_folds <- modelo_stock_cv()$modelo$pred
    
    ggplot(df_preds_folds, aes(x = obs, y = pred, color = Resample)) +
      geom_point(alpha = 0.6) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
      labs(title = "Predicción vs Real por Fold - Modelo Stock (LM CV)",
           x = "Valor Real", y = "Predicción", color = "Fold") +
      theme_minimal()
  })
  
  # === Predicción del pedido del mes siguiente ===
  output$pedido_mes_siguiente <- renderTable({
    req(modelo_stock_cv())
    
    next_month_data <- data.frame(
      producto = factor(c("Guantes","Mascarillas","Agujas")),
      stock_inicio = c(50, 120, 80),
      pacientes = c(200, 320, 150),
      pedidos_realizados = c(150, 300, 100)
    )
    
    next_month_data$stock_ratio <- next_month_data$stock_inicio / pmax(1, next_month_data$pedidos_realizados)
    next_month_data$pacientes_ratio <- next_month_data$pacientes / pmax(1, next_month_data$stock_inicio)
    
    pred_pedido <- predict(modelo_stock_cv()$modelo, newdata = next_month_data)
    next_month_data$Pedido_Sugerido <- round(pred_pedido)
    
    productos_altos <- next_month_data$producto[next_month_data$Pedido_Sugerido > 400]
    if(length(productos_altos) > 0){
      shinyalert(
        title = "Alerta de pedido alto",
        text = paste("Se sugiere pedir mucho de:", paste(productos_altos, collapse = ", ")),
        type = "warning"
      )
    }
    
    next_month_data[, c("producto","Pedido_Sugerido")]
  })
  
  # ==================== MODELO DIAGNÓSTICO XGBOOST ====================
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
    
    list(modelo = modelo, dtest = dtest, test_labels = label_test, testData = testData)
  })
  
  # === Gráfico de importancia XGBoost ===
  output$varImp_diag <- renderPlot({
    req(modelo_diag())
    importance <- xgb.importance(model = modelo_diag()$modelo)
    xgb.plot.importance(importance_matrix = importance, top_n = 10, rel_to_first = TRUE)
  })
  
  # === Matriz de confusión XGBoost ===
  output$cm_diag_text <- renderPrint({
    req(modelo_diag())
    
    pred_prob <- predict(modelo_diag()$modelo, newdata = modelo_diag()$dtest)
    pred_matrix <- matrix(pred_prob, ncol = length(levels(modelo_diag()$testData$diagnostico)), byrow = TRUE)
    pred_labels <- max.col(pred_matrix) - 1
    
    cm <- caret::confusionMatrix(factor(pred_labels, levels=0:3, labels=levels(modelo_diag()$testData$diagnostico)),
                                 modelo_diag()$testData$diagnostico)
    print(cm)
  })
  
  output$cm_diag_plot <- renderPlot({
    req(modelo_diag())
    
    pred_prob <- predict(modelo_diag()$modelo, newdata = modelo_diag()$dtest)
    pred_matrix <- matrix(pred_prob, ncol = length(levels(modelo_diag()$testData$diagnostico)), byrow = TRUE)
    pred_labels <- max.col(pred_matrix) - 1
    
    cm <- caret::confusionMatrix(factor(pred_labels, levels=0:3, labels=levels(modelo_diag()$testData$diagnostico)),
                                 modelo_diag()$testData$diagnostico)
    
    cm_melt <- melt(cm$table)
    
    ggplot(cm_melt, aes(Reference, Prediction, fill=value)) +
      geom_tile() +
      geom_text(aes(label=value), color="white") +
      scale_fill_gradient(low="blue", high="red") +
      ggtitle("Matriz de confusión - Diagnóstico (XGBoost)")
  })
}

# ==================== RUN APP ====================
shinyApp(ui = ui, server = server)
