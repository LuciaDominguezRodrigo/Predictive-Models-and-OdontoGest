library(testthat)
library(shiny)
library(mockery)
library(pool)

# Cargamos el código del módulo (asumiendo que está en un archivo llamado module_contact.R)
source("../modules/contact_management/contact_management_server.R")

test_that("contactManagementServer procesa correctamente el archivado de un mensaje", {
  
  # 1. MOCKS: Simulamos el pool y las funciones de base de datos
  # Creamos un pool falso para que no intente conectar a una DB real
  mock_pool <- "fake_pool_object" 
  
  # Simulamos dbGetQuery para que devuelva un mensaje de prueba
  stub(contactManagementServer, "dbGetQuery", data.frame(
    id = 1,
    nombre = "Juan Perez",
    email = "juan@example.com",
    mensaje = "Hola, necesito ayuda",
    leido = FALSE,
    fecha = Sys.time(),
    stringsAsFactors = FALSE
  ))
  
  # Simulamos dbExecute para capturar cuando se intenta actualizar la DB
  m_execute <- mock(1) # Devuelve 1 fila afectada
  stub(contactManagementServer, "dbExecute", m_execute)
  
  # Simulamos la función smtp para que no envíe correos reales
  m_smtp <- mock(TRUE)
  stub(contactManagementServer, "server", function(...) m_smtp)
  
  # 2. TEST SERVER
  testServer(contactManagementServer, args = list(id = "test", pool = mock_pool), {
    
    # Verificamos que al inicio se intentó leer la base de datos
    # (mensajes_pendientes se ejecuta al renderizar la UI)
    expect_equal(nrow(mensajes_pendientes()), 1)
    
    # 3. ACCIÓN: Simulamos que el usuario pulsa "Archivar" en el mensaje ID 1
    # Recuerda que usamos el input delegado 'target_archive'
    session$setInputs(target_archive = 1)
    
    # 4. VERIFICACIÓN:
    # Comprobamos que dbExecute fue llamado con los argumentos correctos
    args <- expect_called(m_execute, 1) # Se llamó una vez
    call_args <- mock_args(m_execute)[[1]]
    
    expect_match(call_args[[2]], "UPDATE contacto SET leido = TRUE")
    expect_equal(call_args[[3]][[1]], 1) # El ID pasado fue el 1
    
    # Verificamos que el valor de refresh aumentó
    expect_gt(refresh(), 0)
  })
})

test_that("contactManagementServer prepara los datos para responder", {
  mock_pool <- "fake_pool_object"
  
  stub(contactManagementServer, "dbGetQuery", data.frame(
    id = 42,
    nombre = "Ana Lopez",
    email = "ana@example.com",
    mensaje = "Consulta",
    leido = FALSE,
    stringsAsFactors = FALSE
  ))
  
  test_that("contactManagementServer gestiona errores de envío SMTP", {
    mock_pool <- "fake_pool_object"
    
    # Stub para que dbGetQuery devuelva un mensaje
    stub(contactManagementServer, "dbGetQuery", data.frame(
      id = 99, nombre = "Error Test", email = "test@error.com", 
      mensaje = "Falla por favor", leido = FALSE, stringsAsFactors = FALSE
    ))
    
    # FORZAMOS EL ERROR: El servidor SMTP lanzará una excepción
    m_smtp_fail <- function(...) stop("Error de conexión SMTP: Tiempo de espera agotado")
    stub(contactManagementServer, "server", function(...) m_smtp_fail)
    
    # Mock de la notificación de Shiny para ver si se muestra el error
    m_notify <- mock()
    stub(contactManagementServer, "showNotification", m_notify)
    
    testServer(contactManagementServer, args = list(id = "test", pool = mock_pool), {
      # 1. Preparamos el mensaje a enviar
      rv_active_msg$id <- 99
      rv_active_msg$email <- "test@error.com"
      session$setInputs(email_body = "Esta respuesta va a fallar")
      
      # 2. Ejecutamos el envío
      session$setInputs(confirm_send = 1)
      
      # 3. VERIFICACIÓN:
      # ¿Se llamó a showNotification con el mensaje de error?
      args <- mock_args(m_notify)[[1]]
      expect_match(args[[1]], "Error de conexión SMTP")
      
      # ¿Se evitó el guardado en base de datos? (refresh no debería subir si falló antes)
      # Dependiendo de dónde pongas el dbExecute en tu código, 
      # aquí verificarías que el contador no subió.
    })
  })
  
  testServer(contactManagementServer, args = list(id = "test", pool = mock_pool), {
    # Simulamos click en responder al mensaje 42
    session$setInputs(target_reply = 42)
    
    # Verificamos que los reactiveValues se llenaron con la info correcta para el modal
    expect_equal(rv_active_msg$id, 42)
    expect_equal(rv_active_msg$nombre, "Ana Lopez")
    expect_equal(rv_active_msg$email, "ana@example.com")
  })
})