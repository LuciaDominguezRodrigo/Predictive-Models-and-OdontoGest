# Librerías
library(DBI)
library(pool)
library(bcrypt)

# Suponemos que el pool ya está creado en global.R
# pool <- dbPool(...)

# -----------------------------
# 1️⃣ Crear tablas si no existen
# -----------------------------

# Tabla usuarios
dbExecute(pool, "
CREATE TABLE IF NOT EXISTS usuarios (
  id INT AUTO_INCREMENT PRIMARY KEY,
  usuario VARCHAR(50) UNIQUE NOT NULL,
  nombre VARCHAR(100) NOT NULL,
  contraseña VARCHAR(255) NOT NULL,
  email VARCHAR(100),
  telefono VARCHAR(20),
  tipo_usuario ENUM('admin','recepcion','doctor','paciente') DEFAULT 'paciente'
);
")

# Tabla pacientes
dbExecute(pool, "
CREATE TABLE IF NOT EXISTS pacientes (
  id INT AUTO_INCREMENT PRIMARY KEY,
  nombre VARCHAR(100) NOT NULL,
  email VARCHAR(100),
  telefono VARCHAR(20),
  fecha_nacimiento DATE
);
")

# Tabla citas
dbExecute(pool, "
CREATE TABLE IF NOT EXISTS citas (
  id INT AUTO_INCREMENT PRIMARY KEY,
  paciente_id INT NOT NULL,
  doctor_id INT NOT NULL,
  fecha DATETIME NOT NULL,
  tratamiento VARCHAR(255),
  FOREIGN KEY (paciente_id) REFERENCES pacientes(id),
  FOREIGN KEY (doctor_id) REFERENCES usuarios(id)
);
")

# Tabla tratamientos (opcional)
dbExecute(pool, "
CREATE TABLE IF NOT EXISTS tratamientos (
  id INT AUTO_INCREMENT PRIMARY KEY,
  nombre VARCHAR(100) NOT NULL,
  descripcion TEXT
);
")

# -----------------------------
# 2️⃣ Funciones para insertar datos solo si no existen
# -----------------------------

insert_user <- function(usuario, nombre, pass_plain, email="", telefono="", tipo="paciente"){
  exists <- dbGetQuery(pool, "SELECT 1 FROM usuarios WHERE usuario=?", params=list(usuario))
  if(nrow(exists)==0){
    pass_hash <- hashpw(pass_plain)
    dbExecute(pool, "INSERT INTO usuarios (usuario,nombre,contraseña,email,telefono,tipo_usuario) VALUES (?,?,?,?,?,?)",
              params=list(usuario, nombre, pass_hash, email, telefono, tipo))
    cat("Usuario", usuario, "creado!\n")
  }
}

insert_paciente <- function(nombre, email="", telefono="", fecha_nacimiento=NA){
  exists <- dbGetQuery(pool, "SELECT 1 FROM pacientes WHERE nombre=?", params=list(nombre))
  if(nrow(exists)==0){
    dbExecute(pool, "INSERT INTO pacientes (nombre,email,telefono,fecha_nacimiento) VALUES (?,?,?,?)",
              params=list(nombre,email,telefono,fecha_nacimiento))
    cat("Paciente", nombre, "creado!\n")
  }
}

insert_tratamiento <- function(nombre, descripcion=""){
  exists <- dbGetQuery(pool, "SELECT 1 FROM tratamientos WHERE nombre=?", params=list(nombre))
  if(nrow(exists)==0){
    dbExecute(pool, "INSERT INTO tratamientos (nombre,descripcion) VALUES (?,?)",
              params=list(nombre, descripcion))
    cat("Tratamiento", nombre, "creado!\n")
  }
}

# -----------------------------
# 3️⃣ Insertar datos iniciales de ejemplo
# -----------------------------

# Usuarios de ejemplo
insert_user("admin", "Administrador", "1234", "admin@clinica.com", "123456789", "admin")
insert_user("recepcion1", "Recepcionista", "abcd", "recepcion@clinica.com", "987654321", "recepcion")
insert_user("doctor1", "Dr. Pérez", "medico1", "drperez@clinica.com", "555123456", "doctor")

# Pacientes de ejemplo
insert_paciente("Juan Pérez", "juan@correo.com", "600111222", "1985-04-12")
insert_paciente("María López", "maria@correo.com", "600333444", "1990-09-05")

# Tratamientos de ejemplo
insert_tratamiento("Limpieza dental", "Limpieza y pulido de dientes")
insert_tratamiento("Empaste", "Tratamiento de cavidad dental")
