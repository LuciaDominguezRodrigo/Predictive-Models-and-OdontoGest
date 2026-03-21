# ==============================================================================
# INTEGRACIÓN: model_diagnosis.R (Basado fielmente en app.R)
# ==============================================================================
library(caret)
library(xgboost)
library(dplyr)

# 1. Carga de datos desde la BBDD
# Adaptamos las columnas para que coincidan con la lógica de entrenamiento
cargar_datos_diagnostico <- function(pool) {
  df <- dbGetQuery(pool, "SELECT edad, indice_placa, sangrado_sondaje, profundidad_bolsa_max, es_fumador, diagnostico_final FROM historico_diagnosticos")
  
  # Aseguramos que el target sea factor para clasificación (Igual que en app.R)
  df$diagnostico_final <- as.factor(df$diagnostico_final)
  return(df)
}

# 2. Entrenamiento (Copiando los hiperparámetros de tu app.R)
entrenar_modelo_diagnostico <- function(df) {
  
  # Mismo control de entrenamiento (Cross-Validation)
  control <- trainControl(
    method = "cv", 
    number = min(5, nrow(df)),
    classProbs = TRUE, # Necesario para ver las barras de probabilidad
    summaryFunction = multiClassSummary
  )
  
  # Mismos hiperparámetros de XGBoost que usaste en el prototipo
  grid_xgb <- expand.grid(
    nrounds = 100,         # Aumentado para igualar app.R
    max_depth = 6,         # Mayor profundidad para captar relaciones complejas
    eta = 0.1,             # Learning rate más fino
    gamma = 0, 
    colsample_bytree = 0.8, 
    min_child_weight = 1, 
    subsample = 0.8
  )
  
  modelo <- train(
    diagnostico_final ~ .,
    data = df,
    method = "xgbTree",
    trControl = control,
    tuneGrid = grid_xgb,
    na.action = na.pass
  )
  
  return(modelo)
}