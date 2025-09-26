# =======================================================
# Shiny App: Demostración de Sobreajuste
# Autor: Lucía Domínguez Rodrigo
# Descripción: Genera un dataset simulado, entrena varios 
# modelos de regresión (LM, RF, XGBoost) y compara 
# rendimiento en train vs test para ilustrar el sobreajuste.
# =======================================================


# ==================== LIBRERÍAS ====================
library(shiny)
library(caret)
library(ggplot2)
library(dplyr)
library(randomForest)
library(xgboost)
library(reshape2)

# ==================== UI ====================
ui <- fluidPage(
  titlePanel("Demostración de Sobreajuste"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("generar", "Generar dataset y entrenar modelos")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Métricas Train vs Test", tableOutput("metrics_table")),
        tabPanel("Predicción vs Real - Train", plotOutput("plot_train")),
        tabPanel("Predicción vs Real - Test", plotOutput("plot_test"))
      )
    )
  )
)

# ==================== SERVER ====================
server <- function(input, output, session) {
  
  datos_modelos <- eventReactive(input$generar, {
    set.seed(123)
    
    # === Dataset simulado ===
    n <- 200
    x <- runif(n, 0, 10)
    y <- 3*x + 7 + rnorm(n, 0, 3)  # Relación lineal + ruido
    dataset <- data.frame(x = x, y = y)
    
    # Partición train/test
    trainIndex <- createDataPartition(dataset$y, p = 0.7, list = FALSE)
    trainData <- dataset[trainIndex, ]
    testData <- dataset[-trainIndex, ]
    
    # === Modelos ===
    
    # Regresión Lineal
    modelo_lm <- lm(y ~ x, data = trainData)
    pred_train_lm <- predict(modelo_lm, trainData)
    pred_test_lm <- predict(modelo_lm, testData)
    
    # Random Forest
    modelo_rf <- randomForest(y ~ x, data = trainData, ntree = 200)
    pred_train_rf <- predict(modelo_rf, trainData)
    pred_test_rf <- predict(modelo_rf, testData)
    
    # XGBoost
    # Convertir a matrix numérica
    train_mat <- as.matrix(trainData["x"])
    test_mat <- as.matrix(testData["x"])
    dtrain <- xgb.DMatrix(data = train_mat, label = trainData$y)
    dtest <- xgb.DMatrix(data = test_mat, label = testData$y)
    
    params <- list(objective = "reg:squarederror", eta = 0.1, max_depth = 10)
    modelo_xgb <- xgb.train(params = params, data = dtrain, nrounds = 500, verbose = 0)
    pred_train_xgb <- predict(modelo_xgb, dtrain)
    pred_test_xgb <- predict(modelo_xgb, dtest)
    
    # Métricas
    mae <- function(real, pred) mean(abs(real - pred))
    rmse <- function(real, pred) sqrt(mean((real - pred)^2))
    
    resultados <- data.frame(
      Modelo = c("Regresión Lineal", "Random Forest", "XGBoost"),
      MAE_Train = c(mae(trainData$y, pred_train_lm),
                    mae(trainData$y, pred_train_rf),
                    mae(trainData$y, pred_train_xgb)),
      RMSE_Train = c(rmse(trainData$y, pred_train_lm),
                     rmse(trainData$y, pred_train_rf),
                     rmse(trainData$y, pred_train_xgb)),
      MAE_Test = c(mae(testData$y, pred_test_lm),
                   mae(testData$y, pred_test_rf),
                   mae(testData$y, pred_test_xgb)),
      RMSE_Test = c(rmse(testData$y, pred_test_lm),
                    rmse(testData$y, pred_test_rf),
                    rmse(testData$y, pred_test_xgb))
    )
    
    list(
      resultados = resultados,
      pred_train = data.frame(x = trainData$x, y = trainData$y,
                              lm = pred_train_lm, rf = pred_train_rf, xgb = pred_train_xgb),
      pred_test = data.frame(x = testData$x, y = testData$y,
                             lm = pred_test_lm, rf = pred_test_rf, xgb = pred_test_xgb)
    )
  })
  
  # === Tabla de métricas ===
  output$metrics_table <- renderTable({
    req(datos_modelos())
    datos_modelos()$resultados
  })
  
  # === Gráfico Train ===
  output$plot_train <- renderPlot({
    req(datos_modelos())
    df <- datos_modelos()$pred_train
    df_melt <- melt(df, id.vars = c("x", "y"), variable.name = "Modelo", value.name = "Pred")
    
    # ordenar por x
    df_melt <- df_melt %>% arrange(Modelo, x)
    
    ggplot(df_melt, aes(x = x, y = Pred, color = Modelo)) +
      geom_line(size = 1) +
      geom_point(aes(y = y), color = "black", alpha = 0.5) +
      labs(title = "Predicción vs Real - Train", y = "valores predichos", x = "valores reales") +
      theme_minimal()
  })
  
  # === Gráfico Test ===
  output$plot_test <- renderPlot({
    req(datos_modelos())
    df <- datos_modelos()$pred_test
    df_melt <- melt(df, id.vars = c("x", "y"), variable.name = "Modelo", value.name = "Pred")
    
    # ordenar por x
    df_melt <- df_melt %>% arrange(Modelo, x)
    
    
    ggplot(df_melt, aes(x = x, y = Pred, color = Modelo)) +
      geom_line(size = 1) +
      geom_point(aes(y = y), color = "black", alpha = 0.5) +
      labs(title = "Predicción vs Real - Test", y = "valores predichos", x = "valores reales") +
      theme_minimal()
  })
}

# ==================== RUN APP ====================
shinyApp(ui = ui, server = server)
