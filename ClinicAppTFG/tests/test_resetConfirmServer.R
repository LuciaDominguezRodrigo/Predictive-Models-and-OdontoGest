library(testthat)
library(shiny)
library(RSQLite)
library(DBI)

source("../modules/reset_password/reset_confirm_server.R")

test_that("Modulo Reset Password funciona", {
  # 1. Setup DB
  pool_test <- dbConnect(SQLite(), ":memory:")
  on.exit(dbDisconnect(pool_test))
  dbExecute(pool_test, "CREATE TABLE usuarios (id INTEGER, usuario TEXT, password_hash TEXT, reset_token TEXT, token_expiry TEXT)")
  dbExecute(pool_test, "INSERT INTO usuarios VALUES (1, 'user1', 'old', 'token123', '2099-01-01')")
  
  # 2. Test Server sin tocar ClientData
  testServer(resetConfirmServer, args = list(
    id = "test",
    pool = pool_test,
    show_view = function(x) NULL,
    token_manual = "token123" # <--- Inyectamos el token aquí
  ), {
    
    # Forzamos a que corra la validación inicial
    session$flushReact()
    
    # Accedemos a los valores exportados
    res_modulo <- session$getReturned()
    
    # Verificamos que encontró al usuario
    expect_s3_class(res_modulo$user(), "data.frame")
    
    # Test de contraseñas diferentes
    session$setInputs(new_pass1 = "pass1", new_pass2 = "pass2")
    session$setInputs(btn_set_pass = 1)
    session$flushReact()
    
    expect_equal(res_modulo$msg(), "❌ No coinciden")
    
    # Test de éxito
    session$setInputs(new_pass1 = "1234", new_pass2 = "1234")
    session$setInputs(btn_set_pass = 2)
    session$flushReact()
    
    expect_equal(res_modulo$msg(), "✅ Éxito")
  })
})