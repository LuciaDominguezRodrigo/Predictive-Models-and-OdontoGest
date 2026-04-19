# Librerías base (seguras)
library(shiny)
library(DBI)
library(dotenv)
library(pool)
library(digest)
library(mailR)
library(sodium)

# Detectar entorno test
is_test <- identical(Sys.getenv("TESTTHAT"), "true")

# Cargar variables de entorno
# Cambia esto:
# load_dot_env(".env")

# Por esto:
tryCatch({
  load_dot_env(".env")
}, error = function(e) {
  message("No se encontró archivo .env. Leyendo variables del sistema.")
})

db_host     <- Sys.getenv("DB_HOST")
db_port     <- as.integer(Sys.getenv("DB_PORT"))
db_name     <- Sys.getenv("DB_NAME")
db_user     <- Sys.getenv("DB_USER")
db_password <- Sys.getenv("DB_PASSWORD")

# 🔥 SOLO cargar DB si NO es test
if (!is_test) {
  library(RMariaDB)
  
  pool <- dbPool(
    drv = RMariaDB::MariaDB(),
    dbname = db_name,
    host = db_host,
    username = db_user,
    password = db_password,
    port = db_port
  )
  
} else {
  pool <- NULL
}

# Zona horaria
Sys.setenv(TZ = "Europe/Madrid")

# Solo configura la ruta de Windows si estás en entorno local
if (Sys.info()[["sysname"]] == "Windows") {
  Sys.setenv(PAGEDOWN_CHROME = "C:/Program Files/Google/Chrome/Application/chrome.exe")
} else {
  # En Linux/Docker (Heroku) usaremos la ruta por defecto de Chromium instalada
  Sys.setenv(PAGEDOWN_CHROME = "google-chrome") 
}