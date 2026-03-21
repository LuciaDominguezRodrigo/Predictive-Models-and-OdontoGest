library(caret)
library(dplyr)

# 1. Carga de datos con estadísticas para la UI
cargar_datos_entrenamiento <- function(pool) {
  query <- "SELECT producto, fecha, stock_inicio, pacientes_atendidos AS pacientes, 
                   pedidos_realizados, cantidad_usada AS pedidos_necesarios 
            FROM historico_stock"
  
  df <- dbGetQuery(pool, query)
  
  # Estadísticas para el mensaje de éxito
  stats <- list(
    n_registros = nrow(df),
    meses = length(unique(format(as.Date(df$fecha), "%Y-%m"))),
    fecha_min = min(df$fecha),
    fecha_max = max(df$fecha)
  )
  
  # Ingeniería de variables
  df <- df %>%
    mutate(
      producto = as.factor(producto),
      stock_ratio = stock_inicio / pmax(1, pedidos_realizados),
      pacientes_ratio = pacientes / pmax(1, stock_inicio),
      log_pedidos = log1p(pedidos_necesarios)
    )
  
  return(list(data = df, stats = stats))
}

# 2. Entrenamiento (Tu lógica de app.R)
entrenar_modelo_ia <- function(df) {
  ctrl <- trainControl(method = "cv", number = 5)
  
  # Entrenamos el logarítmico que es más estable
  modelo_log <- train(
    log_pedidos ~ stock_inicio + pacientes + pedidos_realizados + producto + stock_ratio + pacientes_ratio,
    data = df,
    method = "lm",
    trControl = ctrl
  )
  
  return(modelo_log)
}

# 3. Predicción (Corregida para evitar el error de nombres/columnas)
predecir_stock <- function(modelo, nuevos_datos) {
  # Añadimos los ratios a los datos que entran del slider
  nuevos_datos <- nuevos_datos %>%
    mutate(
      stock_ratio = stock_inicio / pmax(1, pedidos_realizados),
      pacientes_ratio = pacientes / pmax(1, stock_inicio)
    )
  
  # Predicción
  pred_log <- expm1(predict(modelo, nuevos_datos))
  
  # DEVOLVEMOS 4 COLUMNAS: esto soluciona tu error de 'names' attribute
  return(data.frame(
    producto = nuevos_datos$producto,
    stock_actual = nuevos_datos$stock_inicio,
    pacientes = nuevos_datos$pacientes,
    sugerencia_ia = round(pred_log)
  ))
}