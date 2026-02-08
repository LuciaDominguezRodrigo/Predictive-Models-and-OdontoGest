# ==============================================================================
# PROYECTO: ClinicAppTFG
# MÓDULO: Testing Unitario - Login
# DESCRIPCIÓN: Pruebas automatizadas para el módulo de login sin depender de DB real
# ==============================================================================

# 1. CARGA DE LIBRERÍAS --------------------------------------------------------
suppressPackageStartupMessages({
  library(testthat)
  library(shiny)
  library(mockery)
  library(DBI)
  library(bcrypt)
})

# 2. CARGA DEL MÓDULO ----------------------------------------------------------
source("../modules/login/login_server.R")

# 3. STUBS GLOBALES ------------------------------------------------------------
# dbGetQuery simula la consulta a la base de datos
dbGetQuery <- function(pool, query, params) data.frame()

# checkpw simula verificación de contraseña
checkpw <- function(pw, hash) FALSE

# runjs simula la ejecución de JavaScript (limpieza de inputs)
runjs <- function(code) NULL
stub(loginServer, "runjs", runjs)

# 4. SUITE DE PRUEBAS UNITARIAS ------------------------------------------------
describe("Módulo Login", {
  
  # --- Caso 1: Login exitoso ---
  test_that("Login exitoso con usuario y contraseña correctos", {
    user_data <- data.frame(
      id = 1,
      usuario = "usuario1",
      password_hash = bcrypt::hashpw("pass123")
    )
    
    # Mocks
    m_query <- mock(user_data)
    stub(loginServer, "dbGetQuery", m_query)
    stub(loginServer, "checkpw", function(pw, hash) bcrypt::checkpw(pw, hash))
    
    # Reactivos
    user_logged <- reactiveVal(FALSE)
    current_user <- reactiveVal(NULL)
    
    testServer(loginServer, args = list(
      id = "login1",
      pool = "pool_simulado",
      user_logged = user_logged,
      current_user = current_user,
      show_view = reactiveVal(FALSE)
    ), {
      session$setInputs(
        usuario = "usuario1",
        contraseña = "pass123",
        btn_login = 1
      )
      session$flushReact()
      
      # Verificaciones
      expect_true(user_logged())
      expect_equal(current_user()$usuario, "usuario1")
      expect_called(m_query, 1)
    })
  })
  
  # --- Caso 2: Contraseña incorrecta ---
  test_that("Login falla con contraseña incorrecta", {
    user_data <- data.frame(
      id = 1,
      usuario = "usuario1",
      password_hash = bcrypt::hashpw("pass123")
    )
    
    m_query <- mock(user_data)
    stub(loginServer, "dbGetQuery", m_query)
    stub(loginServer, "checkpw", function(pw, hash) FALSE)
    
    user_logged <- reactiveVal(FALSE)
    current_user <- reactiveVal(NULL)
    
    testServer(loginServer, args = list(
      id = "login2",
      pool = "pool_simulado",
      user_logged = user_logged,
      current_user = current_user,
      show_view = reactiveVal(FALSE)
    ), {
      session$setInputs(
        usuario = "usuario1",
        contraseña = "wrongpass",
        btn_login = 1
      )
      session$flushReact()
      
      expect_false(user_logged())
      expect_null(current_user())
      expect_equal(output$login_msg, "❌ Usuario o contraseña incorrectos")
    })
  })
  
  # --- Caso 3: Usuario no existe ---
  test_that("Login falla si usuario no existe", {
    m_query <- mock(data.frame()) # No hay filas
    stub(loginServer, "dbGetQuery", m_query)
    stub(loginServer, "checkpw", function(pw, hash) FALSE)
    
    user_logged <- reactiveVal(FALSE)
    current_user <- reactiveVal(NULL)
    
    testServer(loginServer, args = list(
      id = "login3",
      pool = "pool_simulado",
      user_logged = user_logged,
      current_user = current_user,
      show_view = reactiveVal(FALSE)
    ), {
      session$setInputs(
        usuario = "noexiste",
        contraseña = "cualquier",
        btn_login = 1
      )
      session$flushReact()
      
      expect_false(user_logged())
      expect_null(current_user())
      expect_equal(output$login_msg, "❌ Usuario o contraseña incorrectos")
    })
  })
  
  # --- Caso 4: Forgot password ---
  test_that("Al hacer click en 'forgot_password' se limpia input y muestra vista", {
    show_view <- reactiveVal(FALSE)
    current_user <- reactiveVal(NULL)
    
    testServer(loginServer, args = list(
      id = "login4",
      pool = "pool_simulado",
      user_logged = reactiveVal(FALSE),
      current_user = current_user,
      show_view = show_view
    ), {
      session$setInputs(forgot_password = 1)
      session$flushReact()
      
      expect_true(show_view())
    })
  })
})
