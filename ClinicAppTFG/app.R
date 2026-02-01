# ==============================================================================
# PROYECTO: ClinicAppTFG
# MÓDULO: app.R
# ==============================================================================
library(shiny)
library(DBI)
library(pool)
library(bcrypt)
library(shinyjs)

# -----------------------------
# Configuración global y base de datos
# -----------------------------
source("global.R")
source("db_init.R")    # Crea tablas y datos iniciales

# -----------------------------
# Módulos
# -----------------------------
source("modules/login/login_ui.R")
source("modules/login/login_server.R")
source("modules/index/main_ui_module.R")
source("modules/reset_password/reset_password_ui.R")
source("modules/reset_password/reset_password_server.R")
source("modules/reset_password/reset_confirm_ui.R")
source("modules/reset_password/reset_confirm_server.R")

# -----------------------------
# UI principal
# -----------------------------
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="login_style.css")),
  
  tags$script(src="https://cdn.tailwindcss.com"),
  
  # === CONFIG TAILWIND COLORES PERSONALIZADOS ===
  tags$script(HTML("
      tailwind.config = {
        theme: {
          extend: {
            colors: {
              clinicBlue: '#2563eb',
              clinicPurple: '#6a0dad'
            }
          }
        }
      }
    ")),
  
  uiOutput("ui_login"),
  uiOutput("ui_reset"),
  uiOutput("ui_reset_confirm"),
  uiOutput("ui_main")
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
  # Variables reactivas
  user_logged  <- reactiveVal(FALSE)
  current_user <- reactiveVal(NULL)
  show_view    <- reactiveVal(FALSE)  # FALSE = login, TRUE = reset, "CONFIRM" = confirmación token
  
  # -----------------------------
  # Detectar token en URL al iniciar
  # -----------------------------
  isolate({
    query <- parseQueryString(session$clientData$url_search)
    token <- query[['token']]
    page  <- query[['page']]
    
    if (!is.null(token) && page == "reset_confirm") {
      user_logged(FALSE)
      current_user(NULL)
      show_view("CONFIRM")   # mostramos vista de reset
      
      # Limpiar URL después de renderizar la vista
      shinyjs::delay(100, {
        runjs("history.replaceState({}, '', window.location.pathname);")
      })
    }
  })
  
  # -----------------------------
  # UI dinámico
  # -----------------------------
  output$ui_login <- renderUI({
    if (!user_logged() && identical(show_view(), FALSE)) loginUI("login")
  })
  
  output$ui_reset <- renderUI({
    if (identical(show_view(), TRUE)) resetPasswordUI("resetpass")
  })
  
  output$ui_reset_confirm <- renderUI({
    if (identical(show_view(), "CONFIRM")) resetConfirmUI("resetconfirm")
  })
  
  output$ui_main <- renderUI({
    if (user_logged()) mainUI("main")
  })
  
  # -----------------------------
  # Servidores
  # -----------------------------
  loginServer("login", pool, user_logged, current_user, show_view)
  mainServer("main", current_user, user_logged)
  resetPasswordServer("resetpass", pool, show_view)
  resetConfirmServer("resetconfirm", pool, show_view)
  
  # -----------------------------
  # Cerrar pool al finalizar sesión
  # -----------------------------
  session$onSessionEnded(function() {
    poolClose(pool)
  })
}

# -----------------------------
# Ejecutar la app
# -----------------------------
shinyApp(ui, server, options = list(port = 3841))
