library(xgboost)
library(dplyr)
library(DBI)

# =========================
# 1. CARGA DE DATOS
# =========================
cargar_datos_diagnostico <- function(pool) {
  
  df <- dbGetQuery(pool, "
    SELECT 
      edad, 
      indice_placa, 
      sangrado_sondaje, 
      profundidad_bolsa_max, 
      es_fumador, 
      diagnostico_final 
    FROM historico_diagnosticos
  ")
  
  # Limpiar nombres de clases (MUY IMPORTANTE)
  df$diagnostico_final <- as.factor(make.names(df$diagnostico_final))
  
  # Eliminar NA
  df <- na.omit(df)
  
  return(df)
}

# =========================
# 2. ENTRENAMIENTO
# =========================
entrenar_modelo_diagnostico <- function(df) {
  
  target_col <- "diagnostico_final"
  
  # Target
  y_factor <- as.factor(df[[target_col]])
  y <- as.numeric(y_factor) - 1  # requerido por xgboost
  
  # Features
  X <- df %>%
    select(-all_of(target_col)) %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  
  # DMatrix
  dtrain <- xgb.DMatrix(data = X, label = y)
  
  # Número de clases
  num_class <- length(levels(y_factor))
  
  # Parámetros
  params <- list(
    objective = "multi:softprob",
    num_class = num_class,
    eval_metric = "mlogloss",
    max_depth = 6,
    eta = 0.1,
    gamma = 0,
    colsample_bytree = 0.8,
    min_child_weight = 1,
    subsample = 0.8
  )
  
  # Entrenar modelo
  modelo <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )
  
  return(list(
    modelo = modelo,
    niveles = levels(y_factor)
  ))
}

# =========================
# 3. PREDICCIÓN
# =========================
predecir_diagnostico <- function(modelo_obj, nuevos_datos) {
  
  modelo <- modelo_obj$modelo
  niveles <- modelo_obj$niveles
  
  X_new <- nuevos_datos %>%
    mutate(across(everything(), as.numeric)) %>%
    as.matrix()
  
  dtest <- xgb.DMatrix(data = X_new)
  
  pred <- predict(modelo, dtest)
  
  num_class <- length(niveles)
  
  pred_matrix <- matrix(pred, ncol = num_class, byrow = TRUE)
  
  clases <- max.col(pred_matrix) - 1
  
  resultado <- factor(niveles[clases + 1], levels = niveles)
  
  return(resultado)
}

# =========================
# 4. PIPELINE COMPLETO
# =========================
entrenar_y_guardar_modelo <- function(pool, ruta = "modelo_xgb.rds") {
  
  df <- cargar_datos_diagnostico(pool)
  
  if (nrow(df) < 10) {
    stop("No hay suficientes datos para entrenar el modelo")
  }
  
  modelo <- entrenar_modelo_diagnostico(df)
  
  saveRDS(modelo, ruta)
  
  return(TRUE)
}

cargar_modelo <- function(ruta = "modelo_xgb.rds") {
  return(readRDS(ruta))
}