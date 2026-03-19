# ==============================================================================
# PROYECTO: ClinicAppTFG
# MÓDULO: Testing Unitario - Appointment Management (FIXED PARAMS)
# ==============================================================================

# 1. CARGA DE LIBRERÍAS --------------------------------------------------------
suppressPackageStartupMessages({
  library(testthat)
  library(shiny)
  library(mockery)
  library(DBI)
  library(pool)
  library(toastui)      
  library(shinyWidgets) 
  library(shinyjs)     
})

# 2. CARGA DEL MÓDULO ----------------------------------------------------------
source("../modules/appointments/appointment_server.R")

# 3. OBJETOS MOCK --------------------------------------------------------------
fake_conn <- structure(list(), class = "DBIConnection")
fake_pool <- structure(list(), class = c("Pool", "R6"))
user_session_staff <- reactiveVal(list(id = 1, tipo_usuario = "recepcion", nombre = "Admin Test"))
user_session_paciente <- reactiveVal(list(id = 10, tipo_usuario = "paciente", nombre = "Juan Paciente"))

# 4. SUITE DE PRUEBAS ----------------------------------------------------------

describe("Módulo Appointment Server", {
  
  # CASO 1: Validación de Solape
  test_that("Detecta correctamente un solape de cita", {
    m_query <- function(conn, sql, ...) {
      if (grepl("COUNT", toupper(sql))) return(data.frame(count = 1))
      if (grepl("usuarios", sql)) return(data.frame(id=1, nombre="Dr. Test"))
      return(data.frame()) 
    }
    stub(appointmentServer, "DBI::dbGetQuery", m_query)
    
    testServer(appointmentServer, args = list(pool = fake_conn, current_user = user_session_staff), {
      session$setInputs(fecha_cita = as.Date("2026-05-20"), hora_inicio = "10:00", 
                        duracion = 30, gabinete = 1, doctor = 2)
      session$flushReact()
      expect_match(output$feedback_solape$html, "Conflicto: Horario ocupado")
    })
  })
  
  # CASO 2: Guardar Cita Nueva
  test_that("Personal de recepción puede guardar una cita nueva", {
    m_execute <- mock(1) 
    m_notify  <- mock()
    
    # --- FIX AQUÍ ---
    stub(appointmentServer, "pool::poolCheckout", fake_conn)
    stub(appointmentServer, "pool::poolReturn", function(...) NULL)
    # ----------------
    
    stub(appointmentServer, "DBI::dbExecute", m_execute)
    stub(appointmentServer, "showNotification", m_notify)
    stub(appointmentServer, "removeModal", function() NULL)
    stub(appointmentServer, "DBI::dbGetQuery", function(conn, sql, ...) {
      
      if (grepl("COUNT", toupper(sql))) {
        return(data.frame(count = 0))
      }
      
      if (grepl("LAST_INSERT_ID", sql)) {
        return(data.frame(id = 1))
      }
      
      if (grepl("SELECT estado", sql)) {
        return(data.frame(estado = "programada"))
      }
      
      if (grepl("JOIN usuarios", sql)) {
        return(data.frame(email = "test@test.com", nombre = "Paciente Test"))
      }
      
      return(data.frame())
    })
    
    testServer(appointmentServer, args = list(pool = fake_conn, current_user = user_session_staff), {
      session$setInputs(fecha_cita = as.Date("2026-05-20"), hora_inicio = "12:00", 
                        duracion = 30, paciente = 5, doctor = 2, gabinete = 1, 
                        servicio = "Limpieza", guardar = 1)
      session$flushReact()
      expect_called(m_execute, 1)
    })
  })
  
  # CASO 3: Restricción de Paciente
  test_that("Un paciente no tiene permiso para guardar citas", {
    m_execute <- mock(1)
    stub(appointmentServer, "DBI::dbExecute", m_execute)
    stub(appointmentServer, "DBI::dbGetQuery", function(...) data.frame())
    
    testServer(appointmentServer, args = list(pool = fake_conn, current_user = user_session_paciente), {
      session$setInputs(guardar = 1)
      session$flushReact()
      expect_called(m_execute, 0)
    })
  })
  
  # CASO 4: Anular Cita (CORREGIDO)
  test_that("Al confirmar anulación se ejecuta el UPDATE 'cancelada'", {
    m_execute <- mock(1)
    
    # 🔧 MOCK POOL (CLAVE)
    stub(appointmentServer, "pool::poolCheckout", fake_conn)
    stub(appointmentServer, "pool::poolReturn", function(...) NULL)
    
    # 🔧 MOCKS RESTO
    stub(appointmentServer, "DBI::dbExecute", m_execute)
    stub(appointmentServer, "DBI::dbGetQuery", function(...) data.frame())
    stub(appointmentServer, "removeModal", function() NULL)
    
    testServer(appointmentServer, args = list(pool = fake_conn, current_user = user_session_staff), {
      rv$cita_id <- 123
      session$setInputs(btn_confirmar_si = 1)
      session$flushReact()
      
      expect_called(m_execute, 1)
      
      args_sql <- mock_args(m_execute)[[1]]
      expect_match(args_sql[[2]], "UPDATE citas SET estado='cancelada'")
      
      params_enviados <- unlist(args_sql[[3]])
      expect_true(any(params_enviados == 123))
    })
  })
  # CASO 5: Finalizar Cita (CORREGIDO)
  test_that("Se puede finalizar una cita con observaciones", {
    m_execute <- mock(1)
    stub(appointmentServer, "DBI::dbExecute", m_execute)
    stub(appointmentServer, "DBI::dbGetQuery", function(...) data.frame())
    stub(appointmentServer, "removeModal", function() NULL)
    
    testServer(appointmentServer, args = list(pool = fake_conn, current_user = user_session_staff), {
      rv$cita_id <- 50
      obs_text <- "Test de observacion medica"
      session$setInputs(observaciones = obs_text, btn_finalizar = 1)
      session$flushReact()
      
      expect_called(m_execute, 1)
      args_sql <- mock_args(m_execute)[[1]]
      expect_match(args_sql[[2]], "completada")
      
      # Verificamos que el texto de observación está en los parámetros
      params_enviados <- unlist(args_sql[[3]])
      expect_true(any(grepl("observacion", params_enviados)))
    })
  })
  
  test_that("El filtro de gabinete genera la consulta SQL correcta", {
    m_query <- mock(data.frame())
    stub(appointmentServer, "DBI::dbGetQuery", m_query)
    
    testServer(appointmentServer, args = list(pool = fake_conn, current_user = user_session_staff), {
      # Cambiamos el filtro a Gabinete 2
      session$setInputs(filtro_gabinete = "2")
      session$flushReact()
      
      # Verificamos si en las llamadas a dbGetQuery se incluyó el filtro
      # Buscamos en todas las llamadas realizadas al mock
      all_calls <- mock_args(m_query)
      found_filter <- any(sapply(all_calls, function(x) grepl("AND c.gabinete = ?", x[[2]])))
      
      expect_true(found_filter, info = "La consulta SQL no incluye el filtro de gabinete")
    })
  })
  
  # 2. TEST SEGURIDAD (CORREGIDO)
  test_that("SEGURIDAD: Un paciente solo puede consultar SUS propias citas", {
    m_query <- mock(data.frame())
    stub(appointmentServer, "DBI::dbGetQuery", m_query)
    
    testServer(appointmentServer, args = list(pool = fake_conn, current_user = user_session_paciente), {
      session$flushReact()
      
      all_calls <- mock_args(m_query)
      
      found_correct_query <- any(sapply(all_calls, function(x) {
        is_appointment_sql <- grepl("FROM citas", x[[2]])
        
        has_patient_filter <- grepl("c.paciente_id = ?", x[[2]])
        
        has_correct_id <- FALSE
        if (length(x) >= 3) { 
          params <- unlist(x[[3]])
          if (!is.null(params) && any(params == 10)) {
            has_correct_id <- TRUE
          }
        }
        
        return(is_appointment_sql && has_patient_filter && has_correct_id)
      }))
      
      expect_true(found_correct_query, info = "El paciente pudo haber accedido a citas ajenas o el query no se ejecutó")
    })
  })
  # 3. TEST LIMPIEZA JS (CORREGIDO)
  test_that("Se ejecuta la limpieza de JS al guardar", {
    m_clean <- mock(NULL)
    
    stub(appointmentServer, "pool::poolCheckout", fake_conn)
    stub(appointmentServer, "pool::poolReturn", function(...) NULL)
    
    stub(appointmentServer, "shinyjs::runjs", m_clean)
    stub(appointmentServer, "removeModal", function() NULL)
    stub(appointmentServer, "showNotification", function(...) NULL)
    
    stub(appointmentServer, "DBI::dbExecute", function(...) 1)
    
    stub(appointmentServer, "DBI::dbGetQuery", function(conn, sql, ...) {
      if (grepl("COUNT", toupper(sql))) return(data.frame(count = 0))
      
      if (grepl("LAST_INSERT_ID", sql)) {
        return(data.frame(id = 1))
      }
      
      if (grepl("SELECT estado", sql)) {
        return(data.frame(estado = "programada"))
      }
      
      if (grepl("JOIN usuarios", sql)) {
        return(data.frame(email = "test@test.com", nombre = "Paciente Test"))
      }
      
      return(data.frame())
    })
    
    testServer(appointmentServer, args = list(
      pool = fake_conn,
      current_user = user_session_staff
    ), {
      
      session$setInputs(
        fecha_cita = Sys.Date(), 
        hora_inicio = "10:00", 
        duracion = 30,
        paciente = 1, 
        doctor = 1, 
        gabinete = 1, 
        servicio = "Test",
        guardar = 1
      )
      
      session$flushReact()
      
      expect_called(m_clean, 1)
    })
  })
})