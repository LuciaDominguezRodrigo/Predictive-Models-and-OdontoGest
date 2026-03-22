# ==============================================================================
# TESTS - Diagnosis Server
# ==============================================================================

suppressPackageStartupMessages({
  library(testthat)
  library(shiny)
  library(mockery)
  library(DBI)
})

# 1. Cargar el módulo
source("../modules/diagnosis/diagnosis_server.R")

# ------------------------------------------------------------------------------
# SETUP DE OBJETOS GLOBALES PARA EL TEST
# ------------------------------------------------------------------------------

fake_conn <- structure(list(), class = "DBIConnection")
user_session_doc <- reactiveVal(list(id = 5, nombre = "Dr. García", tipo_usuario = "doctor"))

mock_datos <- data.frame(
  edad = rep(30, 15),
  indice_placa = rep(20, 15),
  sangrado_sondaje = rep(10, 15),
  profundidad_bolsa_max = rep(2, 15),
  es_fumador = rep(0, 15),
  diagnostico_final = factor(rep(c("Salud Normal", "Caries", "Periodontitis"), 5))
)

# ------------------------------------------------------------------------------
# TEST 1: Entrenamiento Automático
# ------------------------------------------------------------------------------

test_that("El modelo se entrena automáticamente al iniciar", {
  
  assign("cargar_datos_diagnostico", function(p) mock_datos, envir = .GlobalEnv)
  assign("entrenar_modelo_diagnostico", function(d) list(method = "xgbTree", finalModel = "fake"), envir = .GlobalEnv)
  
  testServer(diagnosticoServer, args = list(
    pool = fake_conn,
    current_user = user_session_doc
  ), {
    # Al iniciar, v_modelo_entrenado debe tener el valor de nuestro stub
    expect_false(is.null(v_modelo_entrenado()))
    expect_equal(v_modelo_entrenado()$method, "xgbTree")
  })
})

# ------------------------------------------------------------------------------
# TEST 2: Flujo de predicción y guardado
# ------------------------------------------------------------------------------

test_that("El botón analizar genera resultados y guarda en BBDD", {
  
  m_exec <- mock(1)
  
  assign("cargar_datos_diagnostico", function(p) mock_datos, envir = .GlobalEnv)
  assign("entrenar_modelo_diagnostico", function(d) list(method = "xgbTree"), envir = .GlobalEnv)
  assign("dbExecute", m_exec, envir = .GlobalEnv)
  
  # Stub para la predicción de caret/xgboost
  assign("predict", function(...) {
    data.frame("Salud Normal" = 0.9, "Caries" = 0.05, "Periodontitis" = 0.05, check.names = FALSE)
  }, envir = .GlobalEnv)
  
  testServer(diagnosticoServer, args = list(
    pool = fake_conn,
    current_user = user_session_doc
  ), {
    # Simulamos entrada de datos
    session$setInputs(
      edad = 40, placa = 20, sangrado = 10, 
      bolsa = 2, fumador = FALSE
    )
    
    # Pulsamos botón
    session$setInputs(btn_analizar = 1)
    
    # Comprobamos resultado reactivo
    res <- v_ultimo_resultado()
    expect_s3_class(res, "data.frame")
    expect_equal(max(res$Probabilidad), 0.9)
    
    # Comprobamos que se llamó a la base de datos para guardar el diagnóstico
    expect_called(m_exec, 1)
  })
  
  # Limpieza: Restaurar predict original de stats para no romper otros tests
  assign("predict", stats::predict, envir = .GlobalEnv)
  assign("dbExecute", DBI::dbExecute, envir = .GlobalEnv)
})

# ------------------------------------------------------------------------------
# TEST 3: Validación de datos insuficientes
# ------------------------------------------------------------------------------

test_that("No entrena si la BBDD tiene menos de 10 registros", {
  
  # Devolvemos solo 3 filas
  assign("cargar_datos_diagnostico", function(p) mock_datos[1:3, ], envir = .GlobalEnv)
  
  testServer(diagnosticoServer, args = list(
    pool = fake_conn,
    current_user = user_session_doc
  ), {
    # El modelo debe permanecer NULL
    expect_null(v_modelo_entrenado())
  })
})