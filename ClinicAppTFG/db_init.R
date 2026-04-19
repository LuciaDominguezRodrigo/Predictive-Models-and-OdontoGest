# ==============================================================================
# PROYECTO: ClinicAppTFG
# MÓDULO: db_init.R
# DESCRIPCIÓN: Este script recrea las tablas desde cero si RESET_DB = TRUE
# ==============================================================================
TEST_MODE <- Sys.getenv("TEST_MODE") == "false"

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
  RESET_DB <- TRUE   # TRUE = recrea tablas desde cero
  
  # -----------------------------
  #  Crear tablas (con borrado previo seguro para Heroku/JawsDB)
  # -----------------------------
  if (RESET_DB) {
    tryCatch({
      # Borramos las tablas en orden inverso para que no haya problemas de claves foráneas
      dbExecute(pool, "DROP TABLE IF EXISTS historico_diagnosticos;")
      dbExecute(pool, "DROP TABLE IF EXISTS historico_stock;")
      dbExecute(pool, "DROP TABLE IF EXISTS solicitudes_citas;")
      dbExecute(pool, "DROP TABLE IF EXISTS contacto;")
      dbExecute(pool, "DROP TABLE IF EXISTS tratamientos;")
      dbExecute(pool, "DROP TABLE IF EXISTS pedidos_laboratorio;")
      dbExecute(pool, "DROP TABLE IF EXISTS citas;")
      dbExecute(pool, "DROP TABLE IF EXISTS pacientes;")
      dbExecute(pool, "DROP TABLE IF EXISTS notas_clinicas;")
      dbExecute(pool, "DROP TABLE IF EXISTS usuarios;")
      message("Tablas antiguas borradas con éxito (si existían).")
    }, error = function(e) {
      message("Aviso al borrar tablas: ", e$message)
    })
  }

  dbExecute(pool, "
  CREATE TABLE IF NOT EXISTS usuarios (
    id INT AUTO_INCREMENT PRIMARY KEY,
    usuario VARCHAR(50) UNIQUE NOT NULL,
    nombre VARCHAR(100) NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    email VARCHAR(100),
    telefono VARCHAR(20),
    tipo_usuario ENUM('admin','recepcion','doctor','paciente','higienista', 'laboratorio', 'comercial') DEFAULT 'paciente',
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
  
  dbExecute(pool, "
CREATE TABLE IF NOT EXISTS historico_stock (
  id INT AUTO_INCREMENT PRIMARY KEY,
  producto VARCHAR(100),
  fecha DATE,
  stock_inicio INT,
  pacientes_atendidos INT,
  cantidad_usada INT, -- Lo que realmente se gastó
  pedidos_realizados INT -- Lo que se pidió al proveedor
) DEFAULT CHARSET = utf8mb4;
")
  
  # --- NUEVA TABLA AMPLIADA PARA ENTRENAMIENTO DE DIAGNÓSTICO IA ---
  dbExecute(pool, "
CREATE TABLE IF NOT EXISTS historico_diagnosticos (
  id INT AUTO_INCREMENT PRIMARY KEY,
  paciente_id INT,
  edad INT,
  indice_placa INT,           -- % de placa bacteriana
  sangrado_sondaje INT,       -- % de puntos de sangrado
  profundidad_bolsa_max INT,  -- mm de la bolsa más profunda
  es_fumador TINYINT,         -- 0 o 1
  nivel_glucosa INT,          -- mg/dL (Relación directa con periodontitis)
  hba1c FLOAT,                -- Hemoglobina glicosilada
  estres_percibido INT,       -- Escala 1-10 (Factor de riesgo bruxismo/encías)
  higiene_diaria INT,         -- Veces al día que se cepilla
  diagnostico_final ENUM('Normal', 'Caries', 'Periodontitis'),
  fecha_registro TIMESTAMP DEFAULT CURRENT_TIMESTAMP
) DEFAULT CHARSET = utf8mb4;
")
  
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
    
    set.seed(456)
    n_casos <- 800
    
    # Generar variables con "ruido" clínico
    edades <- sample(18:85, n_casos, replace = TRUE)
    placa <- sample(0:100, n_casos, replace = TRUE)
    sangrado <- sample(0:100, n_casos, replace = TRUE)
    bolsas <- sample(1:10, n_casos, replace = TRUE)
    fumadores <- sample(0:1, n_casos, replace = TRUE)
    
    # Nuevas variables
    glucosa <- rnorm(n_casos, 100, 30) # Media 100, SD 30
    hba1c <- 4 + (glucosa/40) + rnorm(n_casos, 0, 0.5)
    estres <- sample(1:10, n_casos, replace = TRUE)
    higiene <- sample(0:4, n_casos, replace = TRUE)
    
    # Lógica de diagnóstico "maestra" para que la IA aprenda:
    # La periodontitis ahora depende de: bolsa + sangrado + fumador + glucosa alta
    riesgo_perio <- (bolsas * 1.5) + (sangrado * 0.5) + (fumadores * 10) + (hba1c * 2)
    
    diags <- ifelse(riesgo_perio > 25, 'Periodontitis',
                    ifelse(placa > 50 & higiene < 2, 'Caries', 'Normal'))
    
    df_diags <- data.frame(
      edad = edades,
      indice_placa = placa,
      sangrado_sondaje = sangrado,
      profundidad_bolsa_max = bolsas,
      es_fumador = fumadores,
      nivel_glucosa = round(glucosa),
      hba1c = round(hba1c, 1),
      estres_percibido = estres,
      higiene_diaria = higiene,
      diagnostico_final = diags
    )
    
    dbWriteTable(pool, "historico_diagnosticos", df_diags, append = TRUE, row.names = FALSE)
    message("Base de datos actualizada: 800 casos clínicos complejos insertados.")
    
   
    
    set.seed(123)
    n_registros <- 1000
    productos_lista <- c(
      "Guantes", "Mascarillas", "Agujas", "cemento", "micro_aplicador",
      "tetric_evoceram_A2", "tetric_evoceram_A3", "tetric_evoflow_A2",
      "alginato_fast_bestdent", "silicona_putty", "temp_bond"
    )
    
    # Generamos vectores de datos
    productos_random <- sample(productos_lista, n_registros, replace = TRUE)
    stock_in <- sample(50:500, n_registros, replace = TRUE)
    pacs <- sample(20:80, n_registros, replace = TRUE)
    ped_realizados <- sample(50:400, n_registros, replace = TRUE)
    
    # Lógica: lo necesario suele ser lo pedido + un ajuste por stock bajo (ruido normal)
    ped_necesarios <- pmax(0, round(ped_realizados + 0.35 * (100 - stock_in) + rnorm(n_registros, 0, 5)))
    fechas <- seq(as.Date('2023-01-01'), by = "day", length.out = n_registros)
    
    # Inserción masiva
    df_historico <- data.frame(
      producto = productos_random,
      fecha = fechas,
      stock_inicio = stock_in,
      pacientes_atendidos = pacs,
      cantidad_usada = ped_necesarios,
      pedidos_realizados = ped_realizados
    )
    
    dbWriteTable(pool, "historico_stock", df_historico, append = TRUE, row.names = FALSE)
    message("Tabla 'historico_stock' poblada con 1000 registros para entrenamiento.")
    
    # 1. USUARIOS (El orden aquí determina el ID)
    # ID 1: Admin
    insert_user("admin", "Administrador", "1234", "lucia@gmail.com", "123456789", "admin")
    # ID 2: Laboratorio
    insert_user("lab1", "Clinident Lab", "1234", "lab@cofares.com", "123456789", "laboratorio")
    # ID 3: El Doctor (Muy importante para citas y notas)
    insert_user("doctor1", "Dr. Pérez Jiménez", "medico1", "drperez@clinica.com", "555123456", "doctor")
    # ID 4: Personal
    insert_user("recepcion1", "Recepcionista", "abcd", "recepcion@clinica.com", "987654321", "recepcion")
    # ID 5: Paciente principal (Juan)
    insert_user("paciente1", "Juan Pérez", "1234", "lucia.dominguez.rodrigo@gmail.com", "000000000", "paciente")
    # ID 6: Segundo paciente (Lucía)
    insert_user("paciente2", "Lucía Domínguez", "1234", "lucia@gmail.com", "000000000", "paciente")

    insert_user("higienista1", "Celia Garcia Urbanos", "1234", "lucia@gmail.com", "000000000", "higienista")

    insert_user("comercial1", "Depósito Dental Pro", "1234", "ventas@deposito.com", "600000000", "comercial")

    
    insert_paciente("Juan Pérez", "juan@correo.com", "600111222", "1985-04-12")
    insert_paciente("María López", "maria@correo.com", "600333444", "1990-09-05")
    insert_tratamiento("Limpieza dental", "Limpieza y pulido de dientes")
    insert_tratamiento("Empaste", "Tratamiento de cavidad dental")
    
    insert_cita(5, 3, 1, Sys.Date(), "10:00:00", 60, "Limpieza Dental")
    insert_cita(6, 3, 2, Sys.Date(), "12:00:00", 30, "Revisión General")
    
    insert_nota_clinica(5, 3, "El paciente presenta inflamación en encías. Se recomienda limpieza profunda.")
    insert_nota_clinica(6, 3, "Paciente con buena higiene general. Revisión en 6 meses.")
    # Pedidos con la nueva función corregida
    insert_pedido_lab(3, 2, "Juan Pérez", "Corona de circonio pieza 46.", tipo = 'protesis')
    insert_pedido_lab(3, 2, "María López", "Férula de descarga.", estado = 'enviado', tipo = 'protesis', empresa = 'MRW', tracking = 'MRW12345')
    insert_pedido_lab(1, 1, "STOCK CLÍNICA", "Pedido Mensual IA: Guantes y Anestesia", tipo = 'material')
    
    message("Datos iniciales insertados con éxito.")
  }
}