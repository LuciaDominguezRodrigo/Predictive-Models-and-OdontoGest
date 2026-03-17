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
  tipo_usuario ENUM('admin','recepcion','doctor','paciente','higienista') DEFAULT 'paciente',
  banneado INT DEFAULT 1, -- 1 = Activo, 0 = Baneado
  reset_token VARCHAR(255) NULL,
  token_expiry DATETIME NULL, 
  foto_blob LONGBLOB NULL
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
  profesional_id INT NOT NULL, 
  gabinete INT NOT NULL DEFAULT 1, 
  fecha_inicio DATETIME NOT NULL,
  fecha_fin DATETIME NOT NULL,
  tipo_servicio VARCHAR(50), 
  estado ENUM('programada', 'cancelada', 'completada') DEFAULT 'programada',
  color VARCHAR(20),  
  observaciones TEXT DEFAULT NULL,
  FOREIGN KEY (paciente_id) REFERENCES usuarios(id),
  FOREIGN KEY (profesional_id) REFERENCES usuarios(id)
) DEFAULT CHARSET = utf8mb4;
")

dbExecute(pool, "
CREATE TABLE IF NOT EXISTS tratamientos (
  id INT AUTO_INCREMENT PRIMARY KEY,
  nombre VARCHAR(100) NOT NULL,
  descripcion TEXT
) DEFAULT CHARSET = utf8mb4;
")

dbExecute(pool, "
CREATE TABLE IF NOT EXISTS contacto (
  id INT AUTO_INCREMENT PRIMARY KEY,
  nombre VARCHAR(100) NOT NULL,
  email VARCHAR(100) NOT NULL,
  mensaje TEXT NOT NULL,
  fecha DATETIME DEFAULT CURRENT_TIMESTAMP,
  leido BOOLEAN DEFAULT FALSE, 
  respuesta TEXT DEFAULT NULL,
  fecha_respuesta DATETIME DEFAULT NULL
) DEFAULT CHARSET = utf8mb4;
")

# -----------------------------
#  Funciones para insertar datos iniciales
# -----------------------------
insert_user <- function(usuario, nombre, pass_plain, email="", telefono="", tipo="paciente", foto_nombre_archivo = "default_user.png"){
  pass_hash <- hashpw(pass_plain)
  
  # Construimos la ruta completa al archivo
  ruta_foto <- file.path("www", "img", foto_nombre_archivo)
  
  # Leer el archivo como binario (raw)
  foto_blob <- NULL
  if (file.exists(ruta_foto)) {
    foto_blob <- readBin(ruta_foto, "raw", file.info(ruta_foto)$size)
  }
  
  dbExecute(pool,
            "INSERT INTO usuarios (usuario, nombre, password_hash, email, telefono, tipo_usuario, banneado, foto_blob) 
             VALUES (?, ?, ?, ?, ?, ?, 1, ?)",
            params = list(usuario, nombre, pass_hash, email, telefono, tipo, list(foto_blob))
  )
  cat("Usuario creado con BLOB: ", usuario, " (Imagen: ", foto_nombre_archivo, ")\n")
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

insert_contacto <- function(nombre, email, mensaje){
  dbExecute(pool, "INSERT INTO contacto (nombre, email, mensaje) VALUES (?,?,?)",
            params=list(nombre, email, mensaje))
  cat("Mensaje de contacto guardado: ", nombre, "\n")
}

insert_cita <- function(paciente_id, profesional_id, gabinete, fecha, hora_inicio, duracion_min, servicio) {
  # Calcular tiempos
  inicio <- as.POSIXct(paste(fecha, hora_inicio))
  fin <- inicio + (duracion_min * 60)
  
  # Asignar color según gabinete (Lógica visual del RF-12)
  color <- switch(as.character(gabinete), 
                  "1" = "#7e57c2", # Púrpura
                  "2" = "#26a69a", # Verde
                  "3" = "#ffa726") # Naranja
  
  dbExecute(pool,
            "INSERT INTO citas (paciente_id, profesional_id, gabinete, fecha_inicio, fecha_fin, tipo_servicio, color) 
             VALUES (?, ?, ?, ?, ?, ?, ?)",
            params = list(paciente_id, profesional_id, gabinete, 
                          format(inicio, "%Y-%m-%d %H:%M:%S"), 
                          format(fin, "%Y-%m-%d %H:%M:%S"), 
                          servicio, color)
  )
}
# -----------------------------
#  Insertar datos iniciales (solo si RESET_DB = TRUE)
# -----------------------------
if (RESET_DB) {
  # Administrador con su foto específica
  insert_user("admin", "Administrador", "1234", "lucia.dominguez.rodrigo@gmail.com", 
              "123456789", "admin", foto_nombre_archivo = "default_admin.png")
  
  # Recepcionista con su foto
  insert_user("recepcion1", "Recepcionista", "abcd", "recepcion@clinica.com", 
              "987654321", "recepcion", foto_nombre_archivo = "default_secretary.png")
  
  # Doctor con su foto
  insert_user("doctor1", "Dr. Pérez Jiménez", "medico1", "drperez@clinica.com", 
              "555123456", "doctor", foto_nombre_archivo = "default_doctor.png")

  insert_user("higienista1", "Hg. García  Urbanos", "higienista1", "hggarcia@clinica.com", 
              "555123456", "higienista", foto_nombre_archivo = "default_doctor.png")
  
  # Usuario genérico (usará default_user.png por defecto según la función)
  insert_user("paciente1", "Juan Sin Foto", "1234", "juan@correo.com", "000000000", "paciente")
  
  message("Base de datos inicializada con éxito y fotos BLOB cargadas.")
  insert_paciente("Juan Pérez", "juan@correo.com", "600111222", "1985-04-12")
  insert_paciente("María López", "maria@correo.com", "600333444", "1990-09-05")
  
  insert_tratamiento("Limpieza dental", "Limpieza y pulido de dientes")
  insert_tratamiento("Empaste", "Tratamiento de cavidad dental")
  
  insert_contacto("Usuario Prueba", "lucia.dominguez.rodrigo@gmail.com", "Hola, me gustaría pedir información sobre ortodoncia.")
  message("Datos de contacto iniciales insertados.")
  
  # Cita en Gabinete 1 (Púrpura) para el Doctor 1
  insert_cita(5, 3, 1, Sys.Date(), "10:00:00", 60, "Limpieza Dental")
  
  # Cita en Gabinete 2 (Verde) para el Doctor 1
  insert_cita(5, 3, 2, Sys.Date(), "12:00:00", 30, "Revisión General")
  
  # Cita en Gabinete 3 (Naranja) para el Administrador (si actúa como clínico)
  insert_cita(5, 1, 3, Sys.Date(), "16:00:00", 90, "Ortodoncia")
  
  message("Citas de prueba insertadas correctamente.")

  
  message("Datos iniciales insertados.")
}
