# ==============================================================================
# PROYECTO: ClinicAppTFG
# MÓDULO: app.R (Versión Final Corregida)
# ==============================================================================
library(shiny)
library(DBI)
library(pool)
library(bcrypt)
library(shinyjs)
library(jsonlite)

# -----------------------------
# Configuración global y base de datos
# -----------------------------
source("global.R")
source("db_init.R")    

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
source("modules/user_management/user_management_ui.R")
source("modules/user_management/user_management_server.R")

# -----------------------------
# UI principal
# -----------------------------
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="login_style.css"),
    tags$script(src="https://cdn.tailwindcss.com"),
    # Script para detectar el botón "Atrás" del navegador
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        window.onpopstate = function(event) {
          Shiny.setInputValue('url_changed', window.location.search, {priority: 'event'});
        };
      });
    ")),
    # Config Tailwind
    tags$script(HTML("
      tailwind.config = {
        theme: { extend: { colors: { clinicBlue: '#2563eb', clinicPurple: '#6a0dad' } } }
      }
    "))
  ),
  
  # Contenedores dinámicos de UI
  uiOutput("ui_login"),
  uiOutput("ui_reset"),
  uiOutput("ui_reset_confirm"),
  uiOutput("ui_main")
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
  user_logged  <- reactiveVal(FALSE)
  current_user <- reactiveVal(NULL)
  show_view    <- reactiveVal(FALSE) 
  
  # --- FUNCION INTERNA PARA ACTUALIZAR URL ---
  update_url <- function(page_name) {
    runjs(sprintf("history.pushState({page: '%s'}, '', '?page=%s');", page_name, page_name))
  }
  
  # 1. Gestionar el botón "Atrás"
  observeEvent(input$url_changed, {
    query <- parseQueryString(input$url_changed)
    page <- query[['page']]
    if (!is.null(page)) {
      if (page == "login") {
        user_logged(FALSE)
        show_view(FALSE)
      } else if (page == "reset_password") {
        show_view(TRUE)
      }
    }
  })
  
  # 2. Actualizar URL cuando cambia el estado de logueo
  observeEvent(user_logged(), {
    if (user_logged()) {
      update_url("dashboard")
    } else if (identical(show_view(), FALSE)) {
      update_url("login")
    }
  }, ignoreInit = TRUE)
  
  # 3. Actualizar URL cuando cambia la vista de recuperación
  observeEvent(show_view(), {
    if (identical(show_view(), TRUE)) {
      update_url("reset_password")
    } else if (identical(show_view(), "CONFIRM")) {
      update_url("reset_confirm")
    }
  }, ignoreInit = TRUE)
  
  update_url <- function(page_name) {
    session$sendCustomMessage("update_url", page_name)
  }
  
  # --- 1. DETECCIÓN INICIAL DE URL (Crucial para el correo) ---
  # Leemos la URL nada más conectar, antes de que los observes de seguridad actúen
  isolate({
    query <- parseQueryString(session$clientData$url_search)
    token <- query[['token']]
    page  <- query[['page']]
    
    if (!is.null(token) && page == "reset_confirm") {
      show_view("CONFIRM")
      shinyjs::delay(100, {
        runjs("history.replaceState({}, '', window.location.pathname);")
      })
    }
  })
  
  # -----------------------------
  # Renders de UI
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
  # Servidores de Módulos
  # -----------------------------
  loginServer("login", pool, user_logged, current_user, show_view)
  mainServer("main", current_user, user_logged, pool)
  resetPasswordServer("resetpass", pool, show_view)
  resetConfirmServer("resetconfirm", pool, show_view)
  
  # -----------------------------
  # Cierre seguro de Base de Datos
  # -----------------------------
  session$onSessionEnded(function() {
    try({
      if (exists("pool")) poolClose(pool)
    }, silent = TRUE)
  })
  
  resetPasswordServer("resetpass", pool, show_view)
  # El servidor de confirmación ahora recibirá el token correctamente
  resetConfirmServer("resetconfirm", pool, show_view)
}

shinyApp(ui, server, options = list(port = 3841))