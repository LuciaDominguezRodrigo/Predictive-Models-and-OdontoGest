# ==============================================================================
# PROYECTO: ClinicAppTFG
# MÓDULO: Testing Unitario - Login
# DESCRIPCIÓN: Pruebas automatizadas para validar la lógica de negocio sin
#              dependencias de base de datos o servidores de correo reales.
# ==============================================================================

library(testthat)
library(bcrypt)
library(mockery)

# -----------------------------
# Función de login usando db_func (mock)
# -----------------------------
login_user <- function(usuario, password, db_func) {
  user <- db_func(usuario)
  if(nrow(user) == 0) return(FALSE)
  checkpw(password, user$contraseña[1])
}

# -----------------------------
# Mocks de usuarios (cycle=TRUE permite múltiples llamadas)
# -----------------------------
mock_admin <- mock(
  data.frame(
    usuario = "admin",
    contraseña = hashpw("1234"),
    tipo_usuario = "admin",
    stringsAsFactors = FALSE
  ),
  cycle = TRUE
)

mock_recepcion <- mock(
  data.frame(
    usuario = "recepcion1",
    contraseña = hashpw("abcd"),
    tipo_usuario = "recepcion",
    stringsAsFactors = FALSE
  ),
  cycle = TRUE
)

mock_paciente <- mock(
  data.frame(
    usuario = "paciente1",
    contraseña = hashpw("pass"),
    tipo_usuario = "paciente",
    stringsAsFactors = FALSE
  ),
  cycle = TRUE
)

mock_noexiste <- mock(
  data.frame(),
  cycle = TRUE
)

# -----------------------------
# Tests básicos
# -----------------------------
test_that("Login correcto funciona", {
  expect_true(login_user("admin", "1234", db_func = mock_admin))
  expect_true(login_user("recepcion1", "abcd", db_func = mock_recepcion))
  expect_true(login_user("paciente1", "pass", db_func = mock_paciente))
})

test_that("Login con contraseña incorrecta falla", {
  expect_false(login_user("admin", "wrongpass", db_func = mock_admin))
  expect_false(login_user("recepcion1", "12345", db_func = mock_recepcion))
  expect_false(login_user("paciente1", "wrong", db_func = mock_paciente))
})

test_that("Login con usuario inexistente falla", {
  expect_false(login_user("noexiste", "1234", db_func = mock_noexiste))
})

# -----------------------------
# Edge cases
# -----------------------------
test_that("Campos vacíos no permiten login", {
  expect_false(login_user("", "", db_func = mock_noexiste))
})

test_that("Usuario con caracteres especiales no rompe la función", {
  special_mock <- mock(
    data.frame(
      usuario = "user!@#",
      contraseña = hashpw("special123"),
      tipo_usuario = "paciente",
      stringsAsFactors = FALSE
    ),
    cycle = TRUE
  )
  expect_true(login_user("user!@#", "special123", db_func = special_mock))
  expect_false(login_user("user!@#", "wrongpass", db_func = special_mock))
})
