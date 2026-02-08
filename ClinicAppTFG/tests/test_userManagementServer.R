# ==============================================================================
# PROYECTO: ClinicAppTFG
# MÓDULO: Testing Unitario - User Management
# DESCRIPCIÓN: Pruebas automatizadas para validar la lógica de negocio sin
#              dependencias de base de datos reales.
# ==============================================================================

# 1. CARGA DE LIBRERÍAS --------------------------------------------------------
suppressPackageStartupMessages({
  library(testthat)
  library(shiny)
  library(mockery)
  library(DBI)
  library(bcrypt)
})

suppressWarnings(
  suppressPackageStartupMessages({
    library(shiny)
    library(mockery)
    library(DBI)
  })
)


# 2. CARGA DEL MÓDULO ----------------------------------------------------------
source("../modules/user_management/user_management_server.R")

# 3. STUBS GLOBALES ------------------------------------------------------------
dbGetQuery <- function(...) NULL
dbExecute <- function(...) NULL
poolWithTransaction <- function(pool, code) code("conn")
showNotification <- function(msg, type) NULL
updateTextInput <- function(session, inputId, value) NULL
hashpw <- function(pw) paste0("HASH_", pw)  # Simplificación del hash

# 4. SUITE DE PRUEBAS UNITARIAS ------------------------------------------------
describe("Módulo User Management", {
  
  # --- Caso 1: Crear paciente exitosamente ---
  test_that("Crear paciente funciona correctamente", {
    pool_mock <- "Pool_Simulado"
    
    # Mock de dbExecute
    m_execute <- mock(1)
    
    # Stub de dbExecute
    stub(userManagementServer, "dbExecute", m_execute)
    
    # Stub de poolWithTransaction para ejecutar la función callback inmediatamente
    stub(userManagementServer, "poolWithTransaction", function(pool, code) code("conn_simulado"))
    
    # Stub de dbGetQuery para simular que no existe usuario previo
    stub(userManagementServer, "dbGetQuery", function(pool, query, params) data.frame())
    
    # Ejecutar el testServer
    testServer(userManagementServer, args = list(pool = pool_mock), {
      session$setInputs(
        nombre        = "Juan",
        usuario       = "paciente1",
        password      = "pass123",
        email         = "juan@test.com",
        tipo_usuario  = "paciente",
        btn_save_user = 1
      )
      
      session$flushReact()
      
      # Verificaciones
      expect_called(m_execute, 2)  # Se llama dos veces: usuarios + pacientes
    })
  })
  
  
  
  
  
  # --- Caso 2: Intentar crear usuario admin ---
  test_that("No se puede crear usuario admin", {
    m_show <- mock()
    
    stub(userManagementServer, "showNotification", m_show)
    
    testServer(userManagementServer, args = list(id = "test", pool = "pool"), {
      session$setInputs(
        nombre = "Admin",
        usuario = "admin1",
        password = "1234",
        email = "admin@test.com",
        tipo_usuario = "admin",
        btn_save_user = 1
      )
      session$flushReact()
      
      # Verificaciones
      expect_called(m_show, 1)
    })
  })
  
  # --- Caso 3: Usuario existente ---
  test_that("Usuario existente no se puede crear", {
    m_query <- mock(data.frame(id = 1)) # Simula que usuario ya existe
    m_show <- mock()
    
    stub(userManagementServer, "dbGetQuery", m_query)
    stub(userManagementServer, "showNotification", m_show)
    
    testServer(userManagementServer, args = list(id = "test", pool = "pool"), {
      session$setInputs(
        nombre = "Juan",
        usuario = "paciente1",
        password = "pass123",
        email = "juan@test.com",
        tipo_usuario = "paciente",
        btn_save_user = 1
      )
      session$flushReact()
      
      expect_called(m_query, 1)
      expect_called(m_show, 1)
    })
  })
  
  # --- Caso 4: Error en transacción ---
  test_that("Error en transacción se maneja correctamente", {
    m_pool <- mock(stop("DB error"))
    m_show <- mock()
    
    stub(userManagementServer, "poolWithTransaction", m_pool)
    stub(userManagementServer, "showNotification", m_show)
    stub(userManagementServer, "dbGetQuery", mock(data.frame()))
    stub(userManagementServer, "hashpw", hashpw)
    
    testServer(userManagementServer, args = list(id = "test", pool = "pool"), {
      session$setInputs(
        nombre = "Juan",
        usuario = "paciente2",
        password = "pass123",
        email = "juan2@test.com",
        tipo_usuario = "paciente",
        btn_save_user = 1
      )
      session$flushReact()
      
      expect_called(m_show, 1)
    })
  })
})
