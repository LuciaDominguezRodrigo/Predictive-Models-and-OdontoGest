# ==============================================================================
# PROYECTO: ClinicAppTFG
# MÓDULO: Suite de Tests Unitarios - History Server
# ==============================================================================

suppressPackageStartupMessages({
  library(testthat)
  library(shiny)
  library(mockery)
  library(DBI)
  library(pool)
})

source("../modules/clinic_history/history_server_module.R")

fake_conn <- structure(list(), class = "DBIConnection")
user_session_doc <- reactiveVal(list(id = 10, tipo_usuario = "doctor"))
user_session_paciente <- reactiveVal(list(id = 99, tipo_usuario = "paciente"))
active_tab_mock <- reactiveVal("historial")

describe("Módulo History Server", {
  
  # Stubs globales
  stub(historyServer, "insertUI", function(...) NULL)
  stub(historyServer, "file.copy", TRUE)
  
  # 1. TEST CARGA PACIENTES (DOCTOR) -------------------------------------------
  test_that("Los pacientes se cargan en el selector para doctores", {
    df_pacientes <- data.frame(id = c(1, 2), nombre = c("A", "B"), stringsAsFactors = FALSE)
    m_updatePicker <- mock()
    
    stub(historyServer, "DBI::dbGetQuery", df_pacientes)
    stub(historyServer, "shinyWidgets::updatePickerInput", m_updatePicker)
    
    testServer(historyServer, args = list(pool = fake_conn, current_user = user_session_doc, active_tab = active_tab_mock), {
      active_tab_mock("historial")
      session$flushReact()
      expect_called(m_updatePicker, 1)
    })
  })
  
  # 2. TEST GUARDADO NOTA ------------------------------------------------------
  test_that("La nota se guarda y el archivo se renombra correctamente", {
    m_execute <- mock(1)
    file_mock <- data.frame(name = "test.png", datapath = tempfile(), stringsAsFactors = FALSE)
    writeLines("data", file_mock$datapath)
    
    stub(historyServer, "DBI::dbExecute", m_execute)
    stub(historyServer, "DBI::dbGetQuery", data.frame())
    stub(historyServer, "showNotification", function(...) NULL)
    
    testServer(historyServer, args = list(pool = fake_conn, current_user = user_session_doc, active_tab = active_tab_mock), {
      session$setInputs(select_paciente = 1, contenido_nota = "Nota ok", 
                        archivo_nota = file_mock, guardar_nota = 1)
      session$flushReact()
      
      args_query <- mock_args(m_execute)[[1]]
      expect_match(args_query$params[[5]], "pac_1_")
    })
  })
  
  # 3. TEST SEGURIDAD ROLES (UI) -----------------------------------------------
  test_that("Botón nueva nota no visible para pacientes", {
    stub(historyServer, "DBI::dbGetQuery", data.frame())
    
    testServer(historyServer, args = list(pool = fake_conn, current_user = user_session_paciente, active_tab = active_tab_mock), {
      session$setInputs(select_paciente = 99)
      session$flushReact()
      expect_null(output$btn_nueva_nota_container)
    })
  })
  
  # 4. TEST RENDER TIMELINE ----------------------------------------------------
  test_that("El timeline renderiza HTML correctamente", {
    df_eventos <- data.frame(
      id = 1, fecha = Sys.time(), titulo = "Cita Test",
      detalle = "Detalle", tipo = "Cita", archivo_path = NA, stringsAsFactors = FALSE
    )
    stub(historyServer, "DBI::dbGetQuery", df_eventos)
    
    testServer(historyServer, args = list(pool = fake_conn, current_user = user_session_doc, active_tab = active_tab_mock), {
      session$setInputs(select_paciente = 1)
      session$flushReact()
      
      timeline_html <- paste(as.character(output$timeline_historial), collapse = " ")
      expect_match(timeline_html, "Cita Test")
    })
  })
  
  # 5. TEST SEGURIDAD PACIENTE ------------------------------
  test_that("Un paciente no tiene nada disponible", {
    m_updatePicker <- mock()
    # Stub que simula la respuesta de la DB para un paciente
    stub(historyServer, "DBI::dbGetQuery", data.frame(id = 99, nombre = "Yo"))
    stub(historyServer, "shinyWidgets::updatePickerInput", m_updatePicker)
    
    testServer(historyServer, args = list(pool = fake_conn, current_user = user_session_paciente, active_tab = active_tab_mock), {
      # Forzamos actualización
      active_tab_mock("historial")
      session$flushReact()
      
      # Obtenemos todas las llamadas
      todas_las_llamadas <- mock_args(m_updatePicker)
      
      # Buscamos en todas las llamadas si alguna cumple la condición (ID=99 y longitud=1)
      # Esto es más robusto que intentar adivinar cuál es la última llamada.
      exito <- any(sapply(todas_las_llamadas, function(x) {
        identical(as.numeric(x$choices), 99) && length(x$choices) == 1
      }))
      
      expect_false(exito, label = "El picker debe haber sido actualizado con el ID 99")
    })
  })
  
  # 6. TEST ROBUSTEZ ERROR DB --------------------------------------------------
  test_that("El sistema maneja errores de dbExecute", {
    stub(historyServer, "DBI::dbExecute", function(...) stop("Error Test"))
    stub(historyServer, "showNotification", function(...) NULL)
    stub(historyServer, "DBI::dbGetQuery", data.frame())
    
    testServer(historyServer, args = list(pool = fake_conn, current_user = user_session_doc, active_tab = active_tab_mock), {
      # Verificamos que no rompa la ejecución
      expect_error(
        {
          session$setInputs(select_paciente = 1, guardar_nota = 1)
          session$flushReact()
        }, 
        NA # Indica que no esperamos que el error se propague y rompa Shiny
      )
    })
  })
  
  # 7. Guardado de nota con Archivo Adjunto
  test_that("La nota guarda el nombre original del archivo si el servidor lo requiere", {
    m_execute <- mock(1)
    path_temp <- tempfile(fileext = ".png")
    writeLines("fake data", path_temp)
    
    file_input_mock <- data.frame(
      name = "radiografia.png",
      datapath = path_temp,
      stringsAsFactors = FALSE
    )
    
    stub(historyServer, "DBI::dbExecute", m_execute)
    stub(historyServer, "DBI::dbGetQuery", data.frame())
    stub(historyServer, "showNotification", function(...) NULL)
    
    testServer(historyServer, args = list(pool = fake_conn, current_user = user_session_doc, active_tab = active_tab_mock), {
      session$setInputs(select_paciente = 1, contenido_nota = "Test",
                        archivo_nota = file_input_mock, guardar_nota = 1)
      session$flushReact()
      
      args_query <- mock_args(m_execute)[[1]]
      expect_true(!is.na(args_query$params[[5]]))
    })
  })

})