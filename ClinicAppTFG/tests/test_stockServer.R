# ==============================================================================
# TESTS - Stock Server (COMPATIBLE CON TU IMPLEMENTACIÓN ACTUAL)
# ==============================================================================

suppressPackageStartupMessages({
  library(testthat)
  library(shiny)
  library(mockery)
  library(DBI)
})

# Cargar módulo
source("../modules/stock_management/stock_server.R")

# ------------------------------------------------------------------------------
# MOCKS
# ------------------------------------------------------------------------------

fake_conn <- structure(list(), class = "DBIConnection")

user_session_admin <- reactiveVal(list(
  id = 1,
  nombre = "Admin",
  tipo_usuario = "admin"
))

mock_info_bbdd <- list(
  data = data.frame(
    producto = c("Guantes", "Mascarillas"),
    stock_inicio = c(50, 50),
    pacientes = c(10, 10),
    pedidos_realizados = c(20, 20),
    pedidos_necesarios = c(25, 25),
    stringsAsFactors = FALSE
  ),
  stats = list(
    n_registros = 100,
    fecha_min = "2023-01-01",
    fecha_max = "2024-01-01",
    meses = 12
  )
)

mock_prediccion <- data.frame(
  producto = c("Guantes", "Mascarillas"),
  stock_actual = c(50, 50),
  pacientes = c(100, 100),
  sugerencia_ia = c(120, 110),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# TESTS
# ------------------------------------------------------------------------------

describe("Stock Server", {
  
  # --------------------------------------------------------------------------
  test_that("Predicción IA genera datos", {
    
    # Sobrescribir funciones externas
    cargar_datos_entrenamiento <<- function(...) mock_info_bbdd
    entrenar_modelo_ia <<- function(...) list()
    predecir_stock <<- function(...) mock_prediccion
    
    testServer(stockServer, args = list(
      pool = fake_conn,
      current_user = user_session_admin
    ), {
      
      session$setInputs(btn_predecir = 1, num_pacientes = 100)
      
      # Verificamos que la tabla se ha generado
      expect_true(!is.null(v_datos_tabla()))
      expect_equal(nrow(v_datos_tabla()), 2)
    })
  })
  
  # --------------------------------------------------------------------------
  test_that("Edición manual modifica sugerencia", {
    
    testServer(stockServer, args = list(
      pool = fake_conn,
      current_user = user_session_admin
    ), {
      
      v_datos_tabla(mock_prediccion)
      
      session$setInputs(tabla_editable_ia_cell_edit = list(
        row = 1,
        col = 4,
        value = "500"
      ))
      
      expect_equal(v_datos_tabla()$sugerencia_ia[1], 500)
    })
  })
  
  # --------------------------------------------------------------------------
  test_that("Añadir producto funciona", {
    
    testServer(stockServer, args = list(
      pool = fake_conn,
      current_user = user_session_admin
    ), {
      
      v_datos_tabla(mock_prediccion)
      
      session$setInputs(
        nuevo_prod_nombre = "Bisturí",
        nuevo_prod_cant = 10
      )
      
      session$setInputs(btn_add_fila = 1)
      
      df <- v_datos_tabla()
      
      expect_equal(nrow(df), 3)
      expect_equal(df$producto[3], "Bisturí")
      expect_equal(df$sugerencia_ia[3], 10)
    })
  })
  
  # --------------------------------------------------------------------------
  test_that("No añade producto si nombre vacío", {
    
    testServer(stockServer, args = list(
      pool = fake_conn,
      current_user = user_session_admin
    ), {
      
      v_datos_tabla(mock_prediccion)
      
      session$setInputs(
        nuevo_prod_nombre = "",
        nuevo_prod_cant = 10
      )
      
      session$setInputs(btn_add_fila = 1)
      
      expect_equal(nrow(v_datos_tabla()), 2)
    })
  })
  
  # --------------------------------------------------------------------------
  test_that("Confirmar pedido ejecuta INSERT", {
    
    m_execute <- mock(1)
    
    assign("dbExecute", m_execute, envir = .GlobalEnv)
    
    testServer(stockServer, args = list(
      pool = fake_conn,
      current_user = user_session_admin
    ), {
      
      v_datos_tabla(mock_prediccion)
      
      session$setInputs(confirmar_pedido_final = 1)
      
      expect_called(m_execute, 1)
      
      args <- mock_args(m_execute)[[1]]
      
      expect_match(args[[2]], "INSERT INTO pedidos_laboratorio")
      expect_match(args[[3]][[2]], "PEDIDO IA VALIDADO")
    })
  })
  
})