# ==============================================================================
# TESTING UNITARIO - CERTIFICATE SERVER
# ==============================================================================

suppressPackageStartupMessages({
  library(testthat)
  library(shiny)
  library(mockery)
  library(DBI)
  library(shinyjs)
})

source("../modules/medical_certificate/medical_certificate_server.R") 

# Helper
get_ui_text <- function(x) {
  paste(as.character(x), collapse = " ")
}

# Mock global webshot
stub(certificateServer, "webshot2::webshot", function(url, file) { 
  writeLines("%PDF-1.4 mock content", file) 
})

describe("Módulo Justificantes", {
  
  # --- Caso 1 ---
  test_that("Mensaje si no hay citas", {
    current_user <- reactiveVal(list(id = 1, nombre = "Paciente Test"))
    
    m_db <- mock(data.frame())
    stub(certificateServer, "DBI::dbGetQuery", m_db)
    
    testServer(certificateServer, args = list(
      id = "cert1", pool = "pool", current_user = current_user
    ), {
      ui_out <- get_ui_text(output$ui_selector)
      
      expect_match(ui_out, "No tenemos el justificante todavía")
      expect_match(ui_out, "alert-light")
    })
  })
  
  # --- Caso 2 ---
  test_that("Selector con citas", {
    current_user <- reactiveVal(list(id = 1, nombre = "Paciente Test"))
    
    fake_citas <- data.frame(
      id = 101,
      fecha_inicio = "2026-03-19 10:00:00",
      tipo_servicio = "Limpieza Dental",
      doctor = "Dra. García",
      stringsAsFactors = FALSE
    )
    
    m_db <- mock(fake_citas, cycle = TRUE)
    stub(certificateServer, "DBI::dbGetQuery", m_db)
    
    testServer(certificateServer, args = list(
      id = "cert2", pool = "pool", current_user = current_user
    ), {
      ui_out <- get_ui_text(output$ui_selector)
      
      expect_match(ui_out, "Limpieza Dental")
      expect_match(ui_out, "btn-download")
    })
  })
  
  # --- Caso 3 ---
  test_that("El ID del usuario se pasa correctamente", {
    current_user <- reactiveVal(list(id = 55))
    
    m_db <- mock(data.frame(), cycle = TRUE)
    stub(certificateServer, "DBI::dbGetQuery", m_db)
    
    testServer(certificateServer, args = list(
      id = "cert3", pool = "pool", current_user = current_user
    ), {
      citas_completadas()
      
      args <- mock_args(m_db)[[1]]
      expect_equal(args$params[[1]], 55)
    })
  })
  
  # --- Caso 4 ---
  test_that("El selector aparece solo si hay citas", {
    current_user <- reactiveVal(list(id = 1))
    
    fake_citas <- data.frame(
      id = 1,
      fecha_inicio = "2026-03-19",
      tipo_servicio = "Consulta",
      doctor = "Doc"
    )
    
    m_db <- mock(fake_citas)
    stub(certificateServer, "DBI::dbGetQuery", m_db)
    
    testServer(certificateServer, args = list(
      id = "cert4", pool = "pool", current_user = current_user
    ), {
      ui_out <- get_ui_text(output$ui_selector)
      
      expect_match(ui_out, "select")
    })
  })
  
  # --- Caso 5 ---
  test_that("No hay selector si no hay citas", {
    current_user <- reactiveVal(list(id = 1))
    
    m_db <- mock(data.frame())
    stub(certificateServer, "DBI::dbGetQuery", m_db)
    
    testServer(certificateServer, args = list(
      id = "cert5", pool = "pool", current_user = current_user
    ), {
      ui_out <- get_ui_text(output$ui_selector)
      
      expect_false(grepl("select", ui_out))
    })
  })
  
})