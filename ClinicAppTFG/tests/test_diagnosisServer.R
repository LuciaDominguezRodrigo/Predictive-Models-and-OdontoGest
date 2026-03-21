# ==============================================================================
# TESTS - Diagnosis Server (Usando stub de mockery)
# ==============================================================================

suppressPackageStartupMessages({
  library(testthat)
  library(shiny)
  library(mockery)
  library(DBI)
  library(plotly)
})

# Cargar el módulo
source("../modules/diagnosis/diagnosis_server.R")

# ------------------------------------------------------------------------------
# DATOS DE PRUEBA Y CONFIGURACIÓN
# ------------------------------------------------------------------------------
fake_conn <- structure(list(), class = "DBIConnection")

user_session_doc <- reactiveVal(list(
  id = 3,
  nombre = "Dr. House",
  tipo_usuario = "doctor"
))

mock_datos_entrenamiento <- data.frame(
  edad = rep(30, 10),
  indice_placa = rep(50, 10),
  sangrado_sondaje = rep(20, 10),
  profundidad_bolsa_max = rep(3, 10),
  es_fumador = rep(0, 10),
  diagnostico_final = rep("Normal", 10),
  stringsAsFactors = TRUE
)

# ------------------------------------------------------------------------------
# SUITE DE TESTS
# ------------------------------------------------------------------------------

describe("Módulo Diagnóstico IA", {
  
  # TEST 1: Verificar la auto-sincronización inicial
  test_that("El modelo se intenta sincronizar automáticamente al arrancar", {
    
    # Creamos los mocks
    m_cargar <- mock(mock_datos_entrenamiento)
    m_entrenar <- mock("modelo_xgboost_fake")
    
    # Usamos stub para interceptar las llamadas dentro de diagnosticoServer
    # IMPORTANTE: Reemplazamos las funciones que el servidor llama internamente
    stub(diagnosticoServer, "cargar_datos_diagnostico", m_cargar)
    stub(diagnosticoServer, "entrenar_modelo_diagnostico", m_entrenar)
    
    testServer(diagnosticoServer, args = list(
      pool = fake_conn,
      current_user = user_session_doc
    ), {
      session$flushReact() 
      # Al iniciar, el server llama a ejecutar_sincronizacion_ia() automáticamente
      expect_equal(v_modelo_entrenado(), "modelo_xgboost_fake")
      expect_called(m_cargar, 1)
      expect_called(m_entrenar, 1)
    })
  })
  
  # TEST 2: Ejecución de Diagnóstico y guardado en BBDD
  test_that("El botón Analizar genera predicción y guarda en BBDD", {
    
    modelo_fake <- list(tipo = "xgboost")
    probs_fake <- data.frame(
      "Salud Normal" = 0.1, 
      "Caries" = 0.2, 
      "Periodontitis" = 0.7, 
      check.names = FALSE
    )
    
    m_predict <- mock(probs_fake)
    m_execute <- mock(1)
    
    # Aplicamos stubs
    stub(diagnosticoServer, "predict", m_predict)
    stub(diagnosticoServer, "dbExecute", m_execute)
    # Evitamos que la auto-sincronización falle si no hay datos mockeando el cargador
    stub(diagnosticoServer, "cargar_datos_diagnostico", mock(mock_datos_entrenamiento))
    stub(diagnosticoServer, "entrenar_modelo_diagnostico", mock(modelo_fake))
    
    testServer(diagnosticoServer, args = list(
      pool = fake_conn,
      current_user = user_session_doc
    ), {
      # Preparamos el estado
      v_modelo_entrenado(modelo_fake)
      
      # Simulamos entrada de datos de un caso grave
      session$setInputs(
        edad = 55, placa = 80, sangrado = 40, 
        bolsa = 6, fumador = TRUE
      )
      
      # Disparamos el análisis
      session$setInputs(btn_analizar = 1)
      
      res <- v_ultimo_resultado()
      expect_equal(res$Categoria[which.max(res$Probabilidad)], "Periodontitis")
      expect_called(m_execute, 1) # Verificamos que se guardó en MySQL
    })
  })
  
  # TEST 3: Seguridad ante datos insuficientes
  test_that("La IA no se entrena si hay menos de 10 registros", {
    
    # Mock que devuelve un solo registro
    m_cargar_pocos <- mock(data.frame(edad = 1))
    m_entrenar <- mock() # No debería llamarse
    
    stub(diagnosticoServer, "cargar_datos_diagnostico", m_cargar_pocos)
    stub(diagnosticoServer, "entrenar_modelo_diagnostico", m_entrenar)
    
    testServer(diagnosticoServer, args = list(
      pool = fake_conn,
      current_user = user_session_doc
    ), {
      expect_null(v_modelo_entrenado())
      expect_called(m_entrenar, 0)
    })
  })
})