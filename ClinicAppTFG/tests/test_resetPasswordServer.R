# ==============================================================================
# PROYECTO: ClinicAppTFG
# MÓDULO: Testing Unitario - Recuperación de Contraseña I
# DESCRIPCIÓN: Pruebas automatizadas para validar la lógica de negocio sin
#              dependencias de base de datos o servidores de correo reales.
# ==============================================================================

# 1. CARGA DE LIBRERÍAS --------------------------------------------------------
# Usamos suppressPackageStartupMessages para limpiar la salida de la consola
suppressPackageStartupMessages({
  library(testthat)
  library(shiny)
  library(mockery)
})

# 2. CONFIGURACIÓN DEL ENTORNO DE PRUEBA ---------------------------------------
# Cargamos la lógica del servidor a testear
source("../modules/reset_password/reset_password_server.R")

# Definimos stubs globales: Funciones "cascarón" que interceptaremos con mocks.
# Esto evita errores de "función no encontrada" sin cargar drivers de DB.
dbGetQuery <- function(...) NULL
dbExecute  <- function(...) NULL

# 3. SUITE DE PRUEBAS UNITARIAS ------------------------------------------------

describe("Módulo de Restablecimiento de Contraseña", {
  
  # --- Caso 1: Flujo Exitoso Completo ---
  test_that("Caso de Éxito: Usuario encontrado y correo enviado", {
    pool_mock <- "Pool_Simulado" 
    show_view_mock <- mock()
    
    # Mock: Datos simulados de respuesta de la base de datos
    res_db <- data.frame(
      id = 1, 
      email = "alumno@tfg.com", 
      nombre = "Usuario TFG", 
      stringsAsFactors = FALSE
    )
    
    m_query   <- mock(res_db)
    m_execute <- mock(1)    # Simula 1 fila actualizada correctamente
    m_correo  <- mock(TRUE) # Simula envío de correo exitoso
    
    # Inyección de dependencias (Stubs)
    stub(resetPasswordServer, "dbGetQuery", m_query)
    stub(resetPasswordServer, "dbExecute", m_execute)
    stub(resetPasswordServer, "enviar_correo_restablecimiento", m_correo)
    
    testServer(resetPasswordServer, args = list(pool = pool_mock, show_view = show_view_mock), {
      # Acción: El usuario solicita recuperación
      session$setInputs(usuario_reset = "alumno@tfg.com")
      session$setInputs(btn_reset = 1)
      
      # Verificaciones (Assertions)
      expect_called(m_query, 1)
      expect_called(m_execute, 1)
      expect_called(m_correo, 1)
      expect_match(output$reset_msg, "Se ha enviado un correo")
    })
  })
  
  # --- Caso 2: Seguridad (Usuario inexistente) ---
  test_that("Seguridad: El sistema no revela si el usuario existe o no", {
    pool_mock <- "Pool_Simulado"
    m_query_empty <- mock(data.frame()) # Simula que no hay coincidencias
    
    stub(resetPasswordServer, "dbGetQuery", m_query_empty)
    
    testServer(resetPasswordServer, args = list(pool = pool_mock, show_view = mock()), {
      session$setInputs(usuario_reset = "no_existo@test.com")
      session$setInputs(btn_reset = 1)
      
      # Aunque el usuario no existe, el mensaje debe ser positivo/genérico por seguridad
      expect_match(output$reset_msg, "Si el usuario existe, recibirás un correo")
      expect_called(m_query_empty, 1)
    })
  })
  
  # --- Caso 3: Error en Base de Datos ---
  test_that("Resiliencia: Manejo de error cuando falla el UPDATE en DB", {
    pool_mock <- "Pool_Simulado"
    res_db <- data.frame(id = 1, email = "a@b.com", nombre = "User", stringsAsFactors = FALSE)
    
    m_query   <- mock(res_db)
    m_execute_fail <- mock(0) # Simula que la DB no pudo guardar el token (0 filas)
    m_correo  <- mock(TRUE)
    
    stub(resetPasswordServer, "dbGetQuery", m_query)
    stub(resetPasswordServer, "dbExecute", m_execute_fail)
    stub(resetPasswordServer, "enviar_correo_restablecimiento", m_correo)
    
    testServer(resetPasswordServer, args = list(pool = pool_mock, show_view = mock()), {
      session$setInputs(usuario_reset = "a@b.com")
      session$setInputs(btn_reset = 1)
      
      # Verificación: Si no se guardó el token, NO se debe enviar el correo
      expect_called(m_correo, 0)
      expect_match(output$reset_msg, "Error interno")
    })
  })
  
  # --- Caso 4: Navegación ---
  test_that("Navegación: El botón de volver regresa al Login", {
    show_view_mock <- mock()
    
    testServer(resetPasswordServer, args = list(pool = "pool", show_view = show_view_mock), {
      session$setInputs(back_login = 1)
      
      # Verifica que se llamó a la función de cambio de vista con FALSE
      expect_called(show_view_mock, 1)
      expect_args(show_view_mock, 1, FALSE)
    })
  })
})