#carga de librerías y variables de entorno
library(shiny)
library(DBI)
library(RMariaDB)
library(dotenv)
library(pool)
library(digest)
library(mailR)
library(sodium)

# Cargar variables de entorno
load_dot_env(".env")

db_host     <- Sys.getenv("DB_HOST")
db_port     <- as.integer(Sys.getenv("DB_PORT"))
db_name     <- Sys.getenv("DB_NAME")
db_user     <- Sys.getenv("DB_USER")
db_password <- Sys.getenv("DB_PASSWORD")

# Crear pool de conexiones
pool <- dbPool(
  drv = RMariaDB::MariaDB(),
  dbname = db_name,
  host = db_host,
  username = db_user,
  password = db_password,
  port = db_port
)
