# ==============================================================================
# PROYECTO: ClinicAppTFG
# MÓDULO: Testing Unitario - Lab Server
# ==============================================================================

# 1. CARGA DE LIBRERÍAS --------------------------------------------------------
suppressPackageStartupMessages({
  library(testthat)
  library(shiny)
  library(mockery)
  library(DBI)
})

# 2. CARGA DEL MÓDULO ----------------------------------------------------------
source("../modules/lab/lab_server.R") 

# 3. OBJETOS MOCK --------------------------------------------------------------
fake_pool <- structure(list(), class = "Pool") # O DBIConnection

# Mocks de sesión para diferentes roles
user_doctor <- reactiveVal(list(id = 10, tipo_usuario = "doctor"))
user_lab    <- reactiveVal(list(id = 20, tipo_usuario = "laboratorio"))

# 4. SUITE DE PRUEBAS ----------------------------------------------------------

describe("Módulo Lab Server", {
  
  # ---------------------------------------------------------------------------
  # CASO 1: Creación de pedido por un Doctor
  # ---------------------------------------------------------------------------
  test_that("Un doctor puede enviar un pedido correctamente", {
    
    m_execute <- mock(1)
    # Mock para obtener la lista de laboratorios en el modal
    m_get_query <- mock(data.frame(id = 1, nombre = "Lab Dental X"))
    
    stub(labServer, "dbGetQuery", m_get_query)
    stub(labServer, "dbExecute", m_execute)
    stub(labServer, "showNotification", function(...) NULL)
    stub(labServer, "showModal", function(...) NULL)
    stub(labServer, "removeModal", function(...) NULL)
    
    testServer(labServer, args = list(pool = fake_pool, current_user = user_doctor), {
      
      # Simulamos clic en nuevo pedido para abrir modal
      session$setInputs(btn_nuevo_pedido = 1)
      
      # Simulamos rellenar el modal y dar a guardar
      session$setInputs(
        lab_id = 1,
        paciente = "Paciente de Prueba",
        desc = "Corona molar inferior",
        save_p = 1
      )
      
      session$flushReact()
      
      # Verificamos que se ejecutó el INSERT
      expect_called(m_execute, 1)
      args <- mock_args(m_execute)[[1]]
      # El tercer parámetro en el INSERT es el nombre del paciente
      expect_equal(args$params[[3]], "Paciente de Prueba")
    })
  })
  
  # ---------------------------------------------------------------------------
  # CASO 2: Flujo del Laboratorio (Aceptar y Enviar)
  # ---------------------------------------------------------------------------
  test_that("El laboratorio puede cambiar el estado a proceso y enviar", {
    
    m_execute <- mock(1, 1)
    # Simulamos que al consultar el estado real para dibujar botones, devuelve 'en_proceso'
    stub(labServer, "dbGetQuery", function(...) data.frame(estado = "en_proceso"))
    stub(labServer, "dbExecute", m_execute)
    stub(labServer, "showModal", function(...) NULL)
    stub(labServer, "removeModal", function(...) NULL)
    
    testServer(labServer, args = list(pool = fake_pool, current_user = user_lab), {
      
      # 1. Simular clic en "Aceptar y Empezar" (vía el input generado por JS/onclick)
      session$setInputs(lab_proceso = 500) # ID del pedido 500
      session$flushReact()
      
      # 2. Simular envío (rellenando modal de transporte)
      session$setInputs(lab_envio = 500)
      session$setInputs(
        courier = "MRW",
        track = "XYZ123",
        confirm_envio = 1
      )
      session$flushReact()
      
      expect_called(m_execute, 2)
      
      # Verificamos la última llamada (el UPDATE de envío)
      args_envio <- mock_args(m_execute)[[2]]
      expect_match(args_envio[[2]], "UPDATE pedidos_laboratorio SET estado = 'enviado'")
      expect_true("MRW" %in% args_envio$params)
    })
  })
  
  # ---------------------------------------------------------------------------
  # CASO 3: Acción del Doctor (Aceptar Trabajo terminado)
  # ---------------------------------------------------------------------------
  test_that("El doctor puede finalizar un trabajo (Aceptar)", {
    
    m_execute <- mock(1)
    stub(labServer, "dbExecute", m_execute)
    stub(labServer, "dbGetQuery", function(...) data.frame(estado = "enviado"))
    
    testServer(labServer, args = list(pool = fake_pool, current_user = user_doctor), {
      
      # El doctor recibe el paquete y pulsa el botón OK (ID del pedido 123)
      session$setInputs(doc_aceptar = 123)
      session$flushReact()
      
      expect_called(m_execute, 1)
      args <- mock_args(m_execute)[[1]]
      expect_match(args[[2]], "estado = 'aceptado'")
      expect_equal(args$params[[1]], 123)
    })
  })
  
  # ---------------------------------------------------------------------------
  # CASO 4: Caso de Error / Validación
  # ---------------------------------------------------------------------------
  test_that("No se guarda el pedido si faltan campos obligatorios", {
    
    m_execute <- mock(1)
    stub(labServer, "dbExecute", m_execute)
    
    testServer(labServer, args = list(pool = fake_pool, current_user = user_doctor), {
      
      # Intentamos guardar con campos vacíos (req() debería detenerlo)
      session$setInputs(
        paciente = "", 
        desc = "",
        save_p = 1
      )
      
      session$flushReact()
      
      # No debería haberse llamado a dbExecute por el req()
      expect_called(m_execute, 0)
    })
  })
})