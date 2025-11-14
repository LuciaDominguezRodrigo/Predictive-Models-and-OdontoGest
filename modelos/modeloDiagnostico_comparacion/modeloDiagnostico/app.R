library(shiny)
library(caret)
library(xgboost)
library(randomForest)
library(e1071)
library(ggplot2)
library(reshape2)
library(dplyr)
library(DT)
library(nnet)
library(MASS)
library(doParallel)

# -----------------------
# Función para generar dataset más complejo
# -----------------------
generar_dataset_complejo <- function(n) {
  set.seed(123)
  Edad <- rnorm(n, 50, 15)
  IMC <- rnorm(n, 27 + 0.05 * Edad, 4)
  Glucosa <- rnorm(n, 90 + 0.4 * IMC, 15)
  HbA1c <- 4.5 + 0.03 * Glucosa + rnorm(n, 0, 0.5)
  Presion <- rnorm(n, 110 + 0.3 * Edad, 10)
  Colesterol <- rnorm(n, 180 + 0.2 * IMC, 20)
  FC <- rnorm(n, 70, 10)
  
  Resistencia <- 0.5 * IMC + 0.03 * Glucosa + rnorm(n, 0, 2)
  Colesterol_eff <- Colesterol + 0.5 * Edad + rnorm(n, 0, 10)
  Inflam <- rnorm(n, 0.02 * Glucosa + 0.03 * IMC, 2)
  
  # Relaciones no lineales e interacciones complejas
  score <- 0.02*Edad + 0.1*IMC + 0.15*Glucosa + 0.2*HbA1c +
    0.05*Edad*Glucosa + 0.1*(IMC^2) - 0.07*(Glucosa*IMC) +
    0.1*Presion + 0.1*Colesterol + 0.1*Inflam + 0.2*Resistencia +
    rnorm(n, 0, 10)  # ruido adicional
  
  Diagnostico <- cut(
    score,
    breaks = quantile(score, probs = seq(0, 1, length.out = 5)),
    labels = c("Normal", "Leve", "Moderado", "Severo"),
    include.lowest = TRUE
  )
  
  data.frame(
    Edad, IMC, Glucosa, HbA1c, Presion, Colesterol, FC,
    Resistencia, Colesterol_eff, Inflam, Diagnostico
  )
}

# -----------------------
# Función segura de entrenamiento
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
  titlePanel("Modelo de Diagnóstico - Dataset Complejo"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n_obs", "Número de pacientes:", 5000, min = 500, max = 20000),
      actionButton("generar", "Generar Datos"),
      hr(),
      actionButton("entrenar", "Entrenar Modelos"),
      hr(),
      h4("Estado:"),
      verbatimTextOutput("estado", placeholder = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Datos", DTOutput("tabla_datos")),
        tabPanel("Resultados", verbatimTextOutput("resultados")),
        tabPanel("Importancia", plotOutput("plot_importancia"))
      )
    )
  )
)

# -----------------------
# SERVER
# -----------------------
server <- function(input, output) {
  estado <- reactiveVal("Esperando...")
  
  # Generación de datos
  datos <- eventReactive(input$generar, {
    estado("Generando dataset complejo...")
    d <- generar_dataset_complejo(input$n_obs)
    estado(paste("Dataset generado con", nrow(d), "filas."))
    d
  })
  
  output$tabla_datos <- renderDT({
    req(datos())
    datatable(head(datos(), 100))
  })
  
  # Entrenamiento
  modelos <- eventReactive(input$entrenar, {
    req(datos())
    df <- datos()
    estado("Creando particiones de entrenamiento...")
    
    trainIndex <- createDataPartition(df$Diagnostico, p = 0.8, list = FALSE)
    trainData <- df[trainIndex, ]
    testData <- df[-trainIndex, ]
    
    estado("Iniciando clúster paralelo...")
    cl <- makeCluster(max(1, detectCores() - 1))
    registerDoParallel(cl)
    on.exit(stopCluster(cl))
    
    ctrl <- trainControl(method = "cv", number = 3)
    
    estado("Entrenando XGBoost...")
    mod_xgb <- train_safe(train(Diagnostico ~ ., data = trainData, method = "xgbTree", trControl = ctrl))
    
    estado("Entrenando Random Forest...")
    mod_rf <- train_safe(train(Diagnostico ~ ., data = trainData, method = "rf", trControl = ctrl))
    
    estado("Entrenando SVM...")
    mod_svm <- train_safe(train(Diagnostico ~ ., data = trainData, method = "svmRadial", trControl = ctrl))
    
    estado("Entrenando Regresión Logística Multinomial...")
    mod_log <- train_safe(train(Diagnostico ~ ., data = trainData, method = "multinom", trControl = ctrl))
    
    estado("Entrenamiento completado.")
    
    list(xgb = mod_xgb, rf = mod_rf, svm = mod_svm, log = mod_log, test = testData)
  })
  
  # Resultados
  output$resultados <- renderPrint({
    req(modelos())
    mods <- modelos()
    test <- mods$test
    
    for (m in c("xgb", "rf", "svm", "log")) {
      cat("\n----- MODELO:", toupper(m), "-----\n")
      if (!is.null(mods[[m]]$error)) {
        cat("❌ Error en el entrenamiento:\n")
        print(mods[[m]]$error)
        next
      }
      pred <- predict(mods[[m]]$result, newdata = test)
      print(confusionMatrix(pred, test$Diagnostico))
    }
  })
  
  # Importancia XGBoost
  output$plot_importancia <- renderPlot({
    req(modelos())
    mod <- modelos()$xgb
    if (!is.null(mod$error)) {
      plot.new(); title("XGBoost falló. No hay importancia disponible.")
      return()
    }
    imp <- varImp(mod$result)$importance
    imp$Variable <- rownames(imp)
    if (!"Overall" %in% colnames(imp)) {
      imp$Overall <- rowMeans(imp[, 1:nlevels(datos()$Diagnostico), drop = FALSE])
    }
    ggplot(imp, aes(x = reorder(Variable, Overall), y = Overall)) +
      geom_col() + coord_flip() +
      labs(title = "Importancia de Variables en XGBoost", x = "Variable", y = "Importancia") +
      theme_minimal()
  })
  
  output$estado <- renderText(estado())
}

shinyApp(ui, server)
