# ==============================================================================
# PROYECTO: ClinicAppTFG
# MÓDULO: db_init.R
# DESCRIPCIÓN: Este script recrea la base de datos y las tablas desde cero si RESET_DB = TRUE
# ==============================================================================


library(DBI)
library(pool)
library(bcrypt)

# -----------------------------
#  Configuración
# -----------------------------
RESET_DB <- TRUE   # TRUE = recrea DB y tablas desde cero
DB_NAME  <- "clinic"


# -----------------------------
#  Recrear base de datos
# -----------------------------
if (RESET_DB) {
  tryCatch({
    dbExecute(pool, paste0("DROP DATABASE IF EXISTS ", DB_NAME, ";"))
    dbExecute(pool, paste0("CREATE DATABASE ", DB_NAME, " DEFAULT CHARACTER SET utf8mb4;"))
    dbExecute(pool, paste0("USE ", DB_NAME, ";"))
    message("Base de datos '", DB_NAME, "' recreada desde cero.")
  }, error = function(e) {
    stop("No se pudo recrear la base de datos: ", e$message)
  })
}

# -----------------------------
#  Crear tablas
# -----------------------------
dbExecute(pool, "
CREATE TABLE IF NOT EXISTS usuarios (
  id INT AUTO_INCREMENT PRIMARY KEY,
  usuario VARCHAR(50) UNIQUE NOT NULL,
  nombre VARCHAR(100) NOT NULL,
  password_hash VARCHAR(255) NOT NULL,
  email VARCHAR(100),
  telefono VARCHAR(20),
  tipo_usuario ENUM('admin','recepcion','doctor','paciente') DEFAULT 'paciente',
  reset_token VARCHAR(255) NULL,
  token_expiry DATETIME NULL
) DEFAULT CHARSET = utf8mb4;
")

dbExecute(pool, "
CREATE TABLE IF NOT EXISTS pacientes (
  id INT AUTO_INCREMENT PRIMARY KEY,
  nombre VARCHAR(100) NOT NULL,
  email VARCHAR(100),
  telefono VARCHAR(20),
  fecha_nacimiento DATE
) DEFAULT CHARSET = utf8mb4;
")

dbExecute(pool, "
CREATE TABLE IF NOT EXISTS citas (
  id INT AUTO_INCREMENT PRIMARY KEY,
  paciente_id INT NOT NULL,
  doctor_id INT NOT NULL,
  fecha DATETIME NOT NULL,
  tratamiento VARCHAR(255),
  FOREIGN KEY (paciente_id) REFERENCES pacientes(id),
  FOREIGN KEY (doctor_id) REFERENCES usuarios(id)
) DEFAULT CHARSET = utf8mb4;
")

dbExecute(pool, "
CREATE TABLE IF NOT EXISTS tratamientos (
  id INT AUTO_INCREMENT PRIMARY KEY,
  nombre VARCHAR(100) NOT NULL,
  descripcion TEXT
) DEFAULT CHARSET = utf8mb4;
")

# -----------------------------
#  Funciones para insertar datos iniciales
# -----------------------------
insert_user <- function(usuario, nombre, pass_plain, email="", telefono="", tipo="paciente"){
  pass_hash <- hashpw(pass_plain)
  dbExecute(pool,
            "INSERT INTO usuarios (usuario,nombre,password_hash,email,telefono,tipo_usuario) VALUES (?,?,?,?,?,?)",
            params=list(usuario, nombre, pass_hash, email, telefono, tipo)
  )
  cat("Usuario creado: ", usuario, "\n")
}

insert_paciente <- function(nombre, email="", telefono="", fecha_nacimiento=NA){
  dbExecute(pool, "INSERT INTO pacientes (nombre,email,telefono,fecha_nacimiento) VALUES (?,?,?,?)",
            params=list(nombre,email,telefono,fecha_nacimiento))
  cat("Paciente creado: ", nombre, "\n")
}

insert_tratamiento <- function(nombre, descripcion=""){
  dbExecute(pool, "INSERT INTO tratamientos (nombre,descripcion) VALUES (?,?)",
            params=list(nombre, descripcion))
  cat("Tratamiento creado: ", nombre, "\n")
}

# -----------------------------
#  Insertar datos iniciales (solo si RESET_DB = TRUE)
# -----------------------------
if (RESET_DB) {
  insert_user("admin", "Administrador", "1234", "lucia.dominguez.rodrigo@gmail.com", "123456789", "admin")
  insert_user("recepcion1", "Recepcionista", "abcd", "recepcion@clinica.com", "987654321", "recepcion")
  insert_user("doctor1", "Dr. Pérez", "medico1", "drperez@clinica.com", "555123456", "doctor")
  
  insert_paciente("Juan Pérez", "juan@correo.com", "600111222", "1985-04-12")
  insert_paciente("María López", "maria@correo.com", "600333444", "1990-09-05")
  
  insert_tratamiento("Limpieza dental", "Limpieza y pulido de dientes")
  insert_tratamiento("Empaste", "Tratamiento de cavidad dental")
  
  message("Datos iniciales insertados.")
}
