# ==============================================================================
# PROYECTO: ClinicAppTFG
# MÓDULO: Testing Unitario - User Management (CORREGIDO)
# ==============================================================================

# 1. CARGA DE LIBRERÍAS --------------------------------------------------------
suppressPackageStartupMessages({
  library(testthat)
  library(shiny)
  library(mockery)
  library(DBI)
  # library(bcrypt) # No es estrictamente necesaria si la mockeamos
})

# 2. CARGA DEL MÓDULO ----------------------------------------------------------
# Asegúrate de que la ruta sea correcta según tu estructura de carpetas
source("../modules/user_management/user_management_server.R")

# 3. SUITE DE PRUEBAS ----------------------------------------------------------
describe("Módulo User Management con soporte de Teléfono", {
  
  # Setup: Objetos comunes para los tests
  fake_conn <- structure(list(), class = "DBIConnection")
  user_session_mock <- reactiveVal(list(tipo_usuario = "admin"))
  
  # --- Caso 1: Crear paciente exitosamente ---
  test_that("Crear paciente con teléfono funciona correctamente", {
    m_execute <- mock(1, 1) # Esperamos 2 inserciones (usuario + paciente)
    
    with_mock(
      dbExecute = m_execute,
      dbGetQuery = function(...) data.frame(), # Simula usuario no existente
      poolWithTransaction = function(p, f) f("conn"),
      showNotification = function(...) NULL,
      updateTextInput = function(...) NULL,
      # MOCK DE BCRYPT: Evita errores si no está la lib o retardo por hashing
      `bcrypt::hashpw` = function(pass, ...) "hash_simulado_123",
      {
        testServer(userManagementServer, args = list(pool = fake_conn, user_session = user_session_mock), {
          # Enviamos todos los inputs, incluyendo el nuevo campo 'telefono'
          session$setInputs(
            nombre        = "Juan Perez",
            usuario       = "juan_paciente",
            password      = "clave Segura",
            email         = "juan@ejemplo.com",
            telefono      = "600112233", # <--- CAMBIO CLAVE
            tipo_usuario  = "paciente",
            btn_save_user = 1
          )
          session$flushReact()
          
          # Verificamos que se llamó a dbExecute 2 veces (User y Paciente)
          expect_called(m_execute, 2)
          
          # Verificación opcional: ¿El primer insert llevaba el teléfono?
          args_user <- mock_args(m_execute)[[1]]
          expect_true(any(grep("600112233", args_user)))
        })
      }
    )
  })
  
  # --- Caso 2: Intentar crear usuario admin ---
  test_that("No se permite la creación de administradores", {
    m_show <- mock()
    
    with_mock(
      showNotification = m_show,
      dbGetQuery = function(...) data.frame(val = 0),
      {
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
          
          # Debe mostrar notificación de error
          expect_called(m_show, 1)
          # El mensaje debería ser el de restricción de admin
          args <- mock_args(m_show)[[1]]
          expect_match(args[[1]], "No se pueden crear administradores")
        })
      }
    )
  })
  
  
  # --- Caso 3: Campos vacíos lanzan error ---
  test_that("Campos vacíos lanzan error", {
    m_show <- mock()
    
    with_mock(
      showNotification = m_show,
      dbGetQuery = function(...) data.frame(val = 0), 
      {
        testServer(userManagementServer, args = list(pool = fake_conn, user_session = user_session_mock), {
          session$setInputs(
            nombre = "", # Campo vacío obligatorio
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
      }
    )
  })
  
  # --- Caso 4: Lógica de duplicados ---
  test_that("Lógica de duplicados detectada correctamente", {
    m_show <- mock()
    
    with_mock(
      showNotification = m_show,
      # Mockeamos dbGetQuery para manejar ambas llamadas (poll y duplicados)
      dbGetQuery = function(conn, statement, ...) {
        if (grepl("SUM", statement)) return(data.frame(val = 0))
        
        # Simulamos que encuentra un usuario duplicado devolviendo una fila
        return(data.frame(id = 99))  
      },
      {
        testServer(userManagementServer, args = list(pool = fake_conn, user_session = user_session_mock), {
          session$setInputs(
            nombre        = "Juan",
            usuario       = "duplicado",
            password      = "123456",
            # Usamos un email ultra-simple para evitar fallos de regex en el test
            email         = "test@ejemplo.com", 
            telefono      = "111222333",
            tipo_usuario  = "paciente",
            btn_save_user = 1
          )
          session$flushReact()
          
          # Obtenemos los argumentos de la notificación lanzada
          args <- mock_args(m_show)[[1]]
          
          # Verificamos que el mensaje NO sea el de email inválido
          # y que SÍ sea el de duplicado
          expect_match(args[[1]], "ya está registrado") 
        })
      }
    )
  })

  # --- Caso 6: Validación de formato de Email (Específico) ---
  test_that("Email con formato inválido lanza advertencia", {
    m_show <- mock()
    
    with_mock(
      showNotification = m_show,
      # Mockeamos dbGetQuery para el poll reactivo inicial
      dbGetQuery = function(...) data.frame(val = 0),
      {
        testServer(userManagementServer, args = list(pool = fake_conn, user_session = user_session_mock), {
          session$setInputs(
            nombre        = "Usuario Test",
            usuario       = "test_user",
            password      = "123456",
            # ENTRADA INVÁLIDA: Sin @ o sin dominio
            email         = "correo_mal_formado.com", 
            telefono      = "123456789",
            tipo_usuario  = "paciente",
            btn_save_user = 1
          )
          session$flushReact()
          
          # 1. Verificamos que se llamó a la notificación
          expect_called(m_show, 1)
          
          # 2. Verificamos que el mensaje sea específicamente el de formato
          args_enviados <- mock_args(m_show)[[1]]
          expect_match(args_enviados[[1]], "El formato del correo electrónico no es válido")
          
          # 3. OPCIONAL: Verificar que NUNCA se intentó consultar duplicados en la DB
          # (Ya que el return() del if anterior debería haber cortado la ejecución)
        })
      }
    )
  })
  
  # --- Caso 7: Protección de Rol (Solo admin/recepcionista crean usuarios) ---
  test_that("Un paciente no puede crear otros usuarios", {
    m_show <- mock()
    # Cambiamos el mock de sesión para que sea un PACIENTE
    user_session_paciente <- reactiveVal(list(tipo_usuario = "paciente"))
    
    with_mock(
      showNotification = m_show,
      dbGetQuery = function(...) data.frame(val = 0),
      {
        testServer(userManagementServer, 
                   args = list(pool = fake_conn, user_session = user_session_paciente), {
                     
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
                     
                     # Verificamos que se mostró un error de permisos
                     expect_called(m_show, 1)
                     args <- mock_args(m_show)[[1]]
                     expect_match(args[[1]], "No tienes permisos")
                     
                     # Opcional: Verificar que no se llamó a dbExecute (no se insertó nada)
                   })
      }
    )
  })



})