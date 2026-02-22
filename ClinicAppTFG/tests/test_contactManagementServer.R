# test_contactManagementServer.R
library(testthat)
library(shiny)
library(mockery)
library(emayili)

# Cargar el módulo
source("../modules/contact_management/contact_management_server.R")

# ---- 1. TEST: Archivar mensaje ----
test_that("Archiva correctamente un mensaje (unitario)", {
  mock_pool <- "fake_pool"
  
  # Mock mensajes pendientes
  stub(contactManagementServer, "dbGetQuery",
       function(...) {
         data.frame(
           id = 1,
           nombre = "Juan Perez",
           email = "juan@example.com",
           mensaje = "Hola, necesito ayuda",
           leido = FALSE,
           fecha = Sys.time()
         )
       })
  
  # Mock dbExecute
  m_exec <- mock(1)
  stub(contactManagementServer, "dbExecute", m_exec)
  
  # Mock SMTP
  stub(contactManagementServer, "server", function(...) TRUE)
  
  testServer(contactManagementServer,
             args = list(id="test", pool=mock_pool),
             {
               
               # Simula el clic en Archivar
               session$setInputs(target_archive = 1)
               
               # Verifica llamada a dbExecute
               expect_called(m_exec, 1)
               args <- mock_args(m_exec)[[1]]
               sql <- args[[2]]
               expect_match(sql, "UPDATE contacto")
               expect_match(sql, "SET leido = TRUE")
               expect_match(sql, "Archivado manualmente")
               expect_equal(args[[3]][[1]], 1)
             })
})

# ---- 2. TEST: Abrir modal de respuesta ----
test_that("Carga correctamente los datos del mensaje y abre modal", {
  mock_pool <- "fake_pool"
  
  stub(contactManagementServer, "dbGetQuery",
       function(...) {
         data.frame(
           id = 42,
           nombre = "Ana Lopez",
           email = "ana@example.com",
           mensaje = "Consulta",
           leido = FALSE
         )
       })
  
  show_mock <- mock(NULL)
  stub(contactManagementServer, "showModal", show_mock)
  
  testServer(contactManagementServer,
             args = list(id="test", pool=mock_pool),
             {
               
               session$setInputs(target_reply = 42)
               
               # Verifica reactiveValues cargados
               expect_equal(rv_active_msg$id, 42)
               expect_equal(rv_active_msg$nombre, "Ana Lopez")
               expect_equal(rv_active_msg$email, "ana@example.com")
               
               # Verifica que se mostró el modal
               expect_called(show_mock, 1)
             })
})

# ---- 3. TEST: Enviar email y archivar ----
test_that("Envía email y archiva correctamente", {
  mock_pool <- "fake_pool"
  
  stub(contactManagementServer, "dbGetQuery",
       function(...) {
         data.frame(
           id = 50,
           nombre = "Carlos Ruiz",
           email = "carlos@example.com",
           mensaje = "Necesito información",
           leido = FALSE
         )
       })
  
  smtp_mock <- mock(TRUE)
  stub(contactManagementServer, "server", function(...) smtp_mock)
  
  m_exec <- mock(1)
  stub(contactManagementServer, "dbExecute", m_exec)
  
  # Mock modal
  stub(contactManagementServer, "showModal", function(...) NULL)
  stub(contactManagementServer, "removeModal", function(...) NULL)
  
  testServer(contactManagementServer,
             args = list(id="test", pool=mock_pool),
             {
               # Abrir modal
               session$setInputs(target_reply = 50)
               
               # Usuario escribe mensaje
               session$setInputs(email_body = "Gracias por contactar")
               
               # Usuario pulsa enviar
               session$setInputs(confirm_send = 1)
               
               # Verifica SMTP
               expect_called(smtp_mock, 1)
               
               # Verifica SQL y parámetros
               expect_called(m_exec, 1)
               args <- mock_args(m_exec)[[1]]
               sql <- args[[2]]
               params <- args[[3]]
               expect_match(sql, "UPDATE contacto")
               expect_match(sql, "SET leido = TRUE")
               expect_match(sql, "respuesta =")
               expect_equal(params[[1]], "Gracias por contactar")
               expect_equal(params[[2]], 50)
             })
})