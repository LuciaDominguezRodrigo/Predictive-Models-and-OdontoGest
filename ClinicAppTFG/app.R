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
    # Referencia al archivo externo en www/script.js
    tags$script(src = "script.js"),
    
    tags$link(rel="stylesheet", type="text/css", href="login_style.css"),
    tags$script(src="https://cdn.tailwindcss.com"),
    
    # Configuración de Tailwind
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
  
  update_url <- function(page_name) {
    session$sendCustomMessage("update_url", page_name)
  }
  
  # --- 1. DETECCIÓN INICIAL DE URL (Crucial para el correo) ---
  # Leemos la URL nada más conectar, antes de que los observes de seguridad actúen
  isolate({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query$token) && identical(query$page, "reset_confirm")) {
      show_view("CONFIRM")
    }
  })
  
  # --- 2. REHIDRATACIÓN DE SESIÓN (F5) ---
  observeEvent(input$recovered_user, {
    if (!is.null(input$recovered_user) && input$recovered_user != "") {
      # Si ya detectamos que venimos de un correo de reset, NO auto-logueamos
      if (identical(show_view(), "CONFIRM")) return()
      
      try({
        user_data <- jsonlite::fromJSON(input$recovered_user)
        current_user(user_data)
        user_logged(TRUE)
      }, silent = TRUE)
    }
  })
  
  # --- 3. SINCRONIZACIÓN DE ESTADO Y URL ---
  observe({
    # Si estamos en modo reset (solicitud o confirmación por token), PARAMOS REDIRECCIÓN
    if (!is.null(show_view()) && show_view() != FALSE) {
      return() 
    }
    
    query <- parseQueryString(session$clientData$url_search)
    page <- query[['page']]
    
    if (!user_logged()) {
      # Si no hay usuario y no estamos reseteando, al login
      if (is.null(input$recovered_user) || input$recovered_user == "") {
        update_url("login")
      }
    } else {
      # Si hay usuario, al dashboard (a menos que estemos en reset)
      if (is.null(page) || page == "login") update_url("dashboard")
    }
  })
  
  # --- 4. RENDERIZADO DE VISTAS ---
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
  
  # --- 5. SERVIDORES ---
  loginServer("login", pool, user_logged, current_user, show_view)
  
  # Al loguearse, guardar en el navegador
  observeEvent(user_logged(), {
    if (user_logged() && !is.null(current_user())) {
      session$sendCustomMessage("save_user", current_user())
      # Solo redirigimos a dashboard si no estamos en medio de un reset
      if (identical(show_view(), FALSE)) update_url("dashboard")
    }
  }, ignoreInit = TRUE)
  
  mainServer("main", current_user, user_logged, pool)
  
  # Logout
  observeEvent(input$`main-btn_logout`, {
    session$sendCustomMessage("clear_user", "")
    user_logged(FALSE)
    current_user(NULL)
    show_view(FALSE)
    runjs("if(document.getElementById('login-usuario')) document.getElementById('login-usuario').value = '';")
    update_url("login")
  })
  
  resetPasswordServer("resetpass", pool, show_view)
  # El servidor de confirmación ahora recibirá el token correctamente
  resetConfirmServer("resetconfirm", pool, show_view)
}

shinyApp(ui, server, options = list(port = 3841))