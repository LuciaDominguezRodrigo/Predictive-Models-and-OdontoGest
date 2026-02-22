# ==============================================================================
# PROYECTO: ClinicAppTFG
# MÓDULO: Testing Unitario - User Management (CORREGIDO con mockery)
# ==============================================================================

# 1. CARGA DE LIBRERÍAS --------------------------------------------------------
suppressPackageStartupMessages({
  library(testthat)
  library(shiny)
  library(mockery)
  library(DBI)
})

# 2. CARGA DEL MÓDULO ----------------------------------------------------------
source("../modules/user_management/user_management_server.R")

# 3. SUITE DE PRUEBAS ----------------------------------------------------------
describe("Módulo User Management con soporte de Teléfono", {
  
  fake_conn <- structure(list(), class = "DBIConnection")
  user_session_mock <- reactiveVal(list(tipo_usuario = "admin"))
  
  # --- Caso 1: Crear paciente exitosamente ---
  test_that("Crear paciente con teléfono funciona correctamente", {
    m_execute <- mock(1, 1)
    
    # Reemplazamos funciones con stub()
    stub(userManagementServer, "dbExecute", m_execute)
    stub(userManagementServer, "dbGetQuery", function(...) data.frame())
    stub(userManagementServer, "poolWithTransaction", function(p, f) f("conn"))
    stub(userManagementServer, "showNotification", function(...) NULL)
    stub(userManagementServer, "updateTextInput", function(...) NULL)
    stub(userManagementServer, "bcrypt::hashpw", function(pass, ...) "hash_simulado_123")
    
    testServer(userManagementServer, args = list(pool = fake_conn, user_session = user_session_mock), {
      session$setInputs(
        nombre        = "Juan Perez",
        usuario       = "juan_paciente",
        password      = "clave Segura",
        email         = "juan@ejemplo.com",
        telefono      = "600112233",
        tipo_usuario  = "paciente",
        btn_save_user = 1
      )
      session$flushReact()
      
      expect_called(m_execute, 2)
      args_user <- mock_args(m_execute)[[1]]
      expect_true(any(grep("600112233", args_user)))
    })
  })
  
  # --- Caso 2: Intentar crear usuario admin ---
  test_that("No se permite la creación de administradores", {
    m_show <- mock()
    
    stub(userManagementServer, "showNotification", m_show)
    stub(userManagementServer, "dbGetQuery", function(...) data.frame(val = 0))
    
    testServer(userManagementServer, args = list(pool = fake_conn, user_session = user_session_mock), {
      session$setInputs(
        nombre       = "Admin Falso",
        usuario      = "malicious_admin",
        password     = "123",
        email        = "admin@fake.com",
        telefono     = "000000000",
        tipo_usuario = "admin",
        btn_save_user = 1
      )
      session$flushReact()
      
      expect_called(m_show, 1)
      args <- mock_args(m_show)[[1]]
      expect_match(args[[1]], "No se pueden crear administradores")
    })
  })
  
  # --- Caso 3: Campos vacíos lanzan error ---
  test_that("Campos vacíos lanzan error", {
    m_show <- mock()
    
    stub(userManagementServer, "showNotification", m_show)
    stub(userManagementServer, "dbGetQuery", function(...) data.frame(val = 0))
    
    testServer(userManagementServer, args = list(pool = fake_conn, user_session = user_session_mock), {
      session$setInputs(
        nombre = "",
        usuario = "test",
        password = "123",
        email = "test@test.com",
        telefono = "123",
        tipo_usuario = "paciente",
        btn_save_user = 1
      )
      session$flushReact()
      
      expect_called(m_show, 1)
      args <- mock_args(m_show)[[1]]
      expect_match(args[[1]], "Por favor, rellene todos los campos")
    })
  })
  
  # --- Caso 4: Lógica de duplicados ---
  test_that("Lógica de duplicados detectada correctamente", {
    m_show <- mock()
    
    stub(userManagementServer, "showNotification", m_show)
    stub(userManagementServer, "dbGetQuery", function(conn, statement, ...) {
      if (grepl("SUM", statement)) return(data.frame(val = 0))
      return(data.frame(id = 99))
    })
    
    testServer(userManagementServer, args = list(pool = fake_conn, user_session = user_session_mock), {
      session$setInputs(
        nombre        = "Juan",
        usuario       = "duplicado",
        password      = "123456",
        email         = "test@ejemplo.com", 
        telefono      = "111222333",
        tipo_usuario  = "paciente",
        btn_save_user = 1
      )
      session$flushReact()
      
      args <- mock_args(m_show)[[1]]
      expect_match(args[[1]], "ya está registrado") 
    })
  })
  
  # --- Caso 6: Validación de formato de Email ---
  test_that("Email con formato inválido lanza advertencia", {
    m_show <- mock()
    
    stub(userManagementServer, "showNotification", m_show)
    stub(userManagementServer, "dbGetQuery", function(...) data.frame(val = 0))
    
    testServer(userManagementServer, args = list(pool = fake_conn, user_session = user_session_mock), {
      session$setInputs(
        nombre        = "Usuario Test",
        usuario       = "test_user",
        password      = "123456",
        email         = "correo_mal_formado.com", 
        telefono      = "123456789",
        tipo_usuario  = "paciente",
        btn_save_user = 1
      )
      session$flushReact()
      
      expect_called(m_show, 1)
      args_enviados <- mock_args(m_show)[[1]]
      expect_match(args_enviados[[1]], "El formato del correo electrónico no es válido")
    })
  })
  
  # --- Caso 7: Protección de Rol ---
  test_that("Un paciente no puede crear otros usuarios", {
    m_show <- mock()
    user_session_paciente <- reactiveVal(list(tipo_usuario = "paciente"))
    
    stub(userManagementServer, "showNotification", m_show)
    stub(userManagementServer, "dbGetQuery", function(...) data.frame(val = 0))
    
    testServer(userManagementServer, args = list(pool = fake_conn, user_session = user_session_paciente), {
      session$setInputs(
        nombre        = "Intento Malicioso",
        usuario       = "hacker",
        password      = "1234",
        email         = "hacker@test.com",
        telefono      = "000",
        tipo_usuario  = "paciente",
        btn_save_user = 1
      )
      session$flushReact()
      
      expect_called(m_show, 1)
      args <- mock_args(m_show)[[1]]
      expect_match(args[[1]], "No tienes permisos")
    })
  })
  
})