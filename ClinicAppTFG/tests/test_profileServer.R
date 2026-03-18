# ==============================================================================
# PROYECTO: ClinicAppTFG
# MÓDULO: Testing Unitario - Perfil de Usuario
# DESCRIPCIÓN: Pruebas para edición de perfil, carga de imágenes y blobs
# ==============================================================================

# 1. CARGA DE LIBRERÍAS --------------------------------------------------------
suppressPackageStartupMessages({
  library(testthat)
  library(shiny)
  library(mockery)
  library(DBI)
  library(httr)
  library(base64enc)
})

# 2. CARGA DEL MÓDULO ----------------------------------------------------------
source("../modules/profile/profile_server.R") 

# 3. STUBS GLOBALES ------------------------------------------------------------
dbExecute <- function(pool, query, params) 1
GET <- function(url) list(status_code = 200, content = raw(0))
content <- function(res, as) raw(0)

# 4. SUITE DE PRUEBAS UNITARIAS ------------------------------------------------
describe("Módulo Perfil (Profile)", {
  
  # --- Caso 1: Actualización de Nombre ---
  test_that("El nombre se actualiza en DB y en el reactivo current_user", {
    # Datos iniciales
    user_init <- list(id = 1, nombre = "Original", email = "test@test.com", telefono = "123", foto_blob = NULL)
    current_user <- reactiveVal(user_init)
    
    # Mock de la base de datos
    m_db <- mock(1)
    stub(profileServer, "dbExecute", m_db)
    
    testServer(profileServer, args = list(
      id = "profile1",
      pool = "pool_simulado",
      current_user = current_user
    ), {
      # Simulamos la apertura del modal y el cambio de input
      session$setInputs(input_new_nom = "Nuevo Nombre")
      session$setInputs(do_save_nom = 1) # Disparamos el botón de guardar
      session$flushReact()
      
      # Verificaciones
      expect_equal(current_user()$nombre, "Nuevo Nombre")
      expect_called(m_db, 1)
      # Verificamos que se llamó con el parámetro correcto
      args <- mock_args(m_db)[[1]]
      expect_match(args[[2]], "UPDATE usuarios SET nombre")
    })
  })
  
  # --- Caso 2: Carga de Foto desde URL ---
  test_that("La foto se descarga desde URL y se guarda el blob", {
    user_init <- list(id = 1, nombre = "User", foto_blob = NULL)
    current_user <- reactiveVal(user_init)
    
    fake_blob <- as.raw(c(0x89, 0x50, 0x4E, 0x47)) # Firma de un PNG
    m_get <- mock(list(status_code = 200))
    m_content <- mock(fake_blob)
    m_db <- mock(1)
    
    stub(profileServer, "GET", m_get)
    stub(profileServer, "status_code", function(x) 200)
    stub(profileServer, "content", m_content)
    stub(profileServer, "dbExecute", m_db)
    
    testServer(profileServer, args = list(
      id = "profile2",
      pool = "pool_simulado",
      current_user = current_user
    ), {
      session$setInputs(new_photo_url = "http://imagen.com/foto.png")
      session$setInputs(do_save_photo_url = 1)
      session$flushReact()
      
      expect_equal(current_user()$foto_blob, list(fake_blob))
      expect_called(m_get, 1)
      expect_called(m_db, 1)
    })
  })
  
  # --- Caso 3: Fallo en URL de Foto ---
  test_that("Si la URL falla, se lanza notificación de error y no cambia nada", {
    current_user <- reactiveVal(list(id = 1, foto_blob = NULL))
    
    stub(profileServer, "GET", mock(list(status_code = 404)))
    stub(profileServer, "status_code", function(x) 404)
    # Stub de showNotification para que no explote
    stub(profileServer, "showNotification", mock(NULL)) 
    
    testServer(profileServer, args = list(
      id = "profile3",
      pool = "pool_simulado",
      current_user = current_user
    ), {
      session$setInputs(new_photo_url = "http://error.com/nada.png")
      session$setInputs(do_save_photo_url = 1)
      session$flushReact()
      
      # El blob debe seguir siendo NULL
      expect_null(current_user()$foto_blob)
    })
  })
  
  # --- Caso 4: Renderizado de Imagen por Defecto ---
  test_that("Si no hay blob, el contenedor de imagen usa la ruta por defecto", {
    # Usuario sin foto
    current_user <- reactiveVal(list(id = 1, nombre = "Test", foto_blob = NULL))
    
    testServer(profileServer, args = list(
      id = "profile4",
      pool = "pool_simulado",
      current_user = current_user
    ), {
      # El output de un renderUI en testServer suele ser una lista.
      # Accedemos al componente HTML (el primer elemento)
      img_html_raw <- output$profile_img_container
      
      # Lo convertimos a un string plano de una sola dimensión
      img_str <- as.character(img_html_raw[[1]])
      
      # Verificamos que contenga la ruta correcta
      expect_match(img_str, "img/default_user.png", fixed = TRUE)
      # También podemos verificar que sea un tag img
      expect_match(img_str, "<img", fixed = TRUE)
    })
  })
})