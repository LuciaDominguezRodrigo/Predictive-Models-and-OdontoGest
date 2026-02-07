# ==============================================================================
# PROYECTO: ClinicAppTFG
# MÓDULO: app.R (Versión Final con resetConfirmServer recibiendo update_url)
# ==============================================================================
library(shiny)
library(DBI)
library(pool)
library(bcrypt)
library(shinyjs)
library(jsonlite)

# Config global y base de datos
source("global.R")
source("db_init.R")    

# Módulos
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
    tags$script(src = "script.js"),
    
    # Config Tailwind
    tags$script(HTML("
      tailwind.config = {
        theme: { extend: { colors: { clinicBlue: '#2563eb', clinicPurple: '#6a0dad' } } }
      }
    "))
  ),
  
  # UI dinámico
  uiOutput("ui_login"),
  uiOutput("ui_reset"),
  uiOutput("ui_reset_confirm"),
  uiOutput("ui_main")
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
  # Estado global
  user_logged  <- reactiveVal(FALSE)
  current_user <- reactiveVal(NULL)
  show_view    <- reactiveVal(FALSE)  # FALSE=login, TRUE=reset, "CONFIRM"=token
  
  # --- Función para actualizar URL
  update_url <- function(page_name) {
    session$sendCustomMessage("update_url", page_name)
  }
  
  # -----------------------------
  # 1. Detectar token de reset en la URL
  # -----------------------------
  isolate({
    query <- parseQueryString(session$clientData$url_search)
    token <- query[['token']]
    page  <- query[['page']]
    
    if (!is.null(token) && page == "reset_confirm") {
      show_view("CONFIRM")
      # limpiar token visible en URL
      shinyjs::delay(150, {
        runjs("history.replaceState({}, '', window.location.pathname + '?page=reset_confirm');")
      })
    }
  })
  
  # -----------------------------
  # 2. Rehidratación de sesión desde LocalStorage
  # -----------------------------
  observeEvent(input$recovered_user, {
    req(input$recovered_user)
    try({
      user <- jsonlite::fromJSON(input$recovered_user)
      current_user(user)
      user_logged(TRUE)
      update_url("dashboard")
    }, silent = TRUE)
  })
  
  # -----------------------------
  # 3. Manejo del botón atrás/adelante
  # -----------------------------
  observeEvent(input$url_changed, {
    query <- parseQueryString(input$url_changed)
    page <- query[['page']]
    
    if (is.null(page)) return()
    
    if (page == "login") {
      user_logged(FALSE)
      current_user(NULL)
      show_view(FALSE)
    } else if (page == "reset_password") {
      show_view(TRUE)
    } else if (page == "reset_confirm") {
      show_view("CONFIRM")
    } else if (page == "dashboard" && !is.null(current_user())) {
      user_logged(TRUE)
    }
  })
  
  # -----------------------------
  # 4. Guardar sesión al hacer login
  # -----------------------------
  observeEvent(user_logged(), {
    if (isTRUE(user_logged()) && !is.null(current_user())) {
      session$sendCustomMessage("save_user", current_user())
      if (identical(show_view(), FALSE)) update_url("dashboard")
    }
  }, ignoreInit = TRUE)
  
  # -----------------------------
  # 5. Logout
  # -----------------------------
  observeEvent(input$`main-btn_logout`, {
    # Borrar LocalStorage primero
    session$sendCustomMessage("clear_user", NULL)
    # Limpiar estado reactivo
    current_user(NULL)
    user_logged(FALSE)
    show_view(FALSE)
    update_url("login")
  })
  
  # -----------------------------
  # 6. Render UI dinámico
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
  # 7. Servidores de módulos
  # -----------------------------
  loginServer("login", pool, user_logged, current_user, show_view)
  mainServer("main", current_user, user_logged, pool)
  resetPasswordServer("resetpass", pool, show_view)
  
  # PASAMOS update_url al módulo para evitar errores
  resetConfirmServer("resetconfirm", pool, show_view, update_url)
  
  # -----------------------------
  # 8. Cierre seguro BD
  # -----------------------------
  session$onSessionEnded(function() {
    try({
      if (exists("pool")) poolClose(pool)
    }, silent = TRUE)
  })
}

shinyApp(ui, server, options = list(port = 3841))
