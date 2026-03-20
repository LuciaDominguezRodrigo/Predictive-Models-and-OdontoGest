# ==============================================================================
# PROYECTO: ClinicAppTFG
# MÓDULO: db_init.R
# DESCRIPCIÓN: Este script recrea la base de datos y las tablas desde cero si RESET_DB = TRUE
# ==============================================================================
TEST_MODE <- Sys.getenv("TEST_MODE") == "true"

if (TEST_MODE) {
  message("Modo TEST: DB desactivada")
  pool <- NULL
  
} else {
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
    tipo_usuario ENUM('admin','recepcion','doctor','paciente','higienista', 'laboratorio') DEFAULT 'paciente',
    banneado INT DEFAULT 1, -- 1 = Activo, 0 = Baneado
    reset_token VARCHAR(255) NULL,
    token_expiry DATETIME NULL, 
    foto_blob LONGBLOB NULL
  ) DEFAULT CHARSET = utf8mb4;
  ")
  
  dbExecute(pool, "
  CREATE TABLE IF NOT EXISTS notas_clinicas (
    id INT AUTO_INCREMENT PRIMARY KEY,
    paciente_id INT NOT NULL,
    profesional_id INT NOT NULL,
    fecha DATETIME DEFAULT CURRENT_TIMESTAMP,
    contenido TEXT NOT NULL,
    archivo_path TEXT NULL,
    nombre_archivo VARCHAR(255) NULL,
    FOREIGN KEY (paciente_id) REFERENCES usuarios(id),
    FOREIGN KEY (profesional_id) REFERENCES usuarios(id)
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
  
  # Tabla de Pedidos Corregida (Incluye tipo_pedido)
  dbExecute(pool, "
  CREATE TABLE IF NOT EXISTS pedidos_laboratorio (
    id INT AUTO_INCREMENT PRIMARY KEY,
    doctor_id INT,
    laboratorio_id INT,
    paciente_nombre VARCHAR(100),
    descripcion TEXT,
    estado ENUM('pendiente', 'en_proceso', 'enviado', 'aceptado', 'devuelto') DEFAULT 'pendiente',
    tipo_pedido VARCHAR(20) DEFAULT 'protesis', 
    empresa_transporte VARCHAR(100),
    numero_seguimiento VARCHAR(100),
    notas_laboratorio TEXT,
    fecha_pedido TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    fecha_envio DATETIME,
    FOREIGN KEY (doctor_id) REFERENCES usuarios(id),
    FOREIGN KEY (laboratorio_id) REFERENCES usuarios(id)
  ) ENGINE=InnoDB DEFAULT CHARSET = utf8mb4;
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
  
  dbExecute(pool, "
  CREATE TABLE IF NOT EXISTS solicitudes_citas (
    id INT AUTO_INCREMENT PRIMARY KEY,
    cita_id INT,
    paciente_id INT,
    motivo TEXT,
    fecha_solicitud TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    leido BOOLEAN DEFAULT FALSE,
    FOREIGN KEY (cita_id) REFERENCES citas(id) ON DELETE CASCADE
  );")
  
  # -----------------------------
  #  Funciones para insertar datos iniciales
  # -----------------------------
  insert_user <- function(usuario, nombre, pass_plain, email="", telefono="", tipo="paciente", foto_nombre_archivo = "default_user.png"){
    pass_hash <- hashpw(pass_plain)
    ruta_foto <- file.path("www", "img", foto_nombre_archivo)
    foto_blob <- NULL
    if (file.exists(ruta_foto)) {
      foto_blob <- readBin(ruta_foto, "raw", file.info(ruta_foto)$size)
    }
    dbExecute(pool,
              "INSERT INTO usuarios (usuario, nombre, password_hash, email, telefono, tipo_usuario, banneado, foto_blob) 
               VALUES (?, ?, ?, ?, ?, ?, 1, ?)",
              params = list(usuario, nombre, pass_hash, email, telefono, tipo, list(foto_blob))
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
  
  insert_cita <- function(paciente_id, profesional_id, gabinete, fecha, hora_inicio, duracion_min, servicio) {
    inicio <- as.POSIXct(paste(fecha, hora_inicio))
    fin <- inicio + (duracion_min * 60)
    color <- switch(as.character(gabinete), "1" = "#7e57c2", "2" = "#26a69a", "3" = "#ffa726")
    dbExecute(pool,
              "INSERT INTO citas (paciente_id, profesional_id, gabinete, fecha_inicio, fecha_fin, tipo_servicio, color) 
               VALUES (?, ?, ?, ?, ?, ?, ?)",
              params = list(paciente_id, profesional_id, gabinete, 
                            format(inicio, "%Y-%m-%d %H:%M:%S"), 
                            format(fin, "%Y-%m-%d %H:%M:%S"), 
                            servicio, color)
    )
  }
  
  insert_nota_clinica <- function(paciente_id, profesional_id, contenido, fecha = NULL) {
    if (is.null(fecha)) fecha <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    dbExecute(pool, "INSERT INTO notas_clinicas (paciente_id, profesional_id, contenido, fecha) VALUES (?, ?, ?, ?)",
              params = list(paciente_id, profesional_id, contenido, fecha))
  }
  
  # FUNCIÓN CORREGIDA (Sin errores de llaves ni variables faltantes)
  insert_pedido_lab <- function(doc_id, lab_id, paciente, desc, estado = 'pendiente', 
                                tipo = 'protesis', empresa = NULL, tracking = NULL) {
    
    empresa_param  <- if (is.null(empresa)) NA else empresa
    tracking_param <- if (is.null(tracking)) NA else tracking
    
    dbExecute(pool, 
              "INSERT INTO pedidos_laboratorio (doctor_id, laboratorio_id, paciente_nombre, descripcion, estado, tipo_pedido, empresa_transporte, numero_seguimiento) 
               VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
              params = list(doc_id, lab_id, paciente, desc, estado, tipo, empresa_param, tracking_param)
    )
    cat("Pedido de laboratorio creado para paciente:", paciente, "\n")
  }
  
  # -----------------------------
  #  Insertar datos iniciales
  # -----------------------------
  if (RESET_DB) {
    insert_user("admin", "Administrador", "1234", "lucia@gmail.com", "123456789", "admin", "default_admin.png")
    insert_user("lab1", "cofares lab", "1234", "lucia@gmail.com", "123456789", "laboratorio", "default_admin.png")
    insert_user("recepcion1", "Recepcionista", "abcd", "recepcion@clinica.com", "987654321", "recepcion", "default_secretary.png")
    insert_user("doctor1", "Dr. Pérez Jiménez", "medico1", "drperez@clinica.com", "555123456", "doctor", "default_doctor.png")
    insert_user("higienista1", "Hg. García Urbanos", "higienista1", "hggarcia@clinica.com", "555123456", "higienista", "default_doctor.png")
    insert_user("paciente1", "Juan Sin Foto", "1234", "lucia.dominguez@gmail.com", "000000000", "paciente")
    
    insert_paciente("Juan Pérez", "juan@correo.com", "600111222", "1985-04-12")
    insert_paciente("María López", "maria@correo.com", "600333444", "1990-09-05")
    insert_tratamiento("Limpieza dental", "Limpieza y pulido de dientes")
    insert_tratamiento("Empaste", "Tratamiento de cavidad dental")
    
    insert_cita(5, 3, 1, Sys.Date(), "10:00:00", 60, "Limpieza Dental")
    insert_cita(5, 3, 2, Sys.Date(), "12:00:00", 30, "Revisión General")
    
    insert_nota_clinica(5, 3, "El paciente presenta inflamación. Se recomienda limpieza.")
    
    # Pedidos con la nueva función corregida
    insert_pedido_lab(3, 2, "Juan Pérez", "Corona de circonio pieza 46.", tipo = 'protesis')
    insert_pedido_lab(3, 2, "María López", "Férula de descarga.", estado = 'enviado', tipo = 'protesis', empresa = 'MRW', tracking = 'MRW12345')
    insert_pedido_lab(1, 1, "STOCK CLÍNICA", "Pedido Mensual IA: Guantes y Anestesia", tipo = 'material')
    
    message("Datos iniciales insertados con éxito.")
  }
}