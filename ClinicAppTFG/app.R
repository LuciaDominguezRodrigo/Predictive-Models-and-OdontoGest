# ==============================================================================
# PROYECTO: ClinicAppTFG
# MÓDULO: app.R 
# ==============================================================================

# -----------------------------
# Librerías
# -----------------------------
library(shiny)
library(DBI)
library(pool)
library(bcrypt)
library(shinyjs)
library(jsonlite)


# -----------------------------
# Config global y base de datos
# -----------------------------
source("global.R")
source("db_init.R")    

# -----------------------------
# Módulos
# -----------------------------

# login
source("modules/login/login_ui.R")
source("modules/login/login_server.R")

#index
source("modules/index/main_ui_module.R")

#reset password: reset password form y envio de correo/reseteo de contraseña
source("modules/reset_password/reset_password_ui.R")
source("modules/reset_password/reset_password_server.R")
source("modules/reset_password/reset_confirm_ui.R")
source("modules/reset_password/reset_confirm_server.R")

#Creación de usuarios
source("modules/user_management/user_management_ui.R")
source("modules/user_management/user_management_server.R")

#index para no logueados
source("modules/public/public_landing_UI.R")
source("modules/public/public_landing_server.R")

# -----------------------------
# UI principal
# -----------------------------
ui <- fluidPage(
  style = "padding: 0px; margin: 0px;", 
  useShinyjs(),
  tags$head(
    tags$script(src = "script.js")
  ),  
  
  uiOutput("ui_landing"),
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
  show_view    <- reactiveVal("LANDING") 
  
  # Función para actualizar URL
  update_url <- function(page_name) {
    session$sendCustomMessage("update_url", page_name)
  }
  
  # 1. Detectar token de reset
  isolate({
    query <- parseQueryString(session$clientData$url_search)
    token <- query[['token']]
    page  <- query[['page']]
    
    if (!is.null(token) && !is.null(page) && page == "reset_confirm") {
      show_view("CONFIRM")
      shinyjs::delay(150, {
        runjs("history.replaceState({}, '', window.location.pathname + '?page=reset_confirm');")
      })
    }
  })
  
  # 2. Rehidratación de sesión
  observeEvent(input$recovered_user, {
    req(input$recovered_user)
    try({
      user <- jsonlite::fromJSON(input$recovered_user)
      current_user(user)
      user_logged(TRUE)
      update_url("dashboard")
    }, silent = TRUE)
  })
  
  # 3. Historial (atrás/adelante)
  observeEvent(input$url_changed, {
    query <- parseQueryString(input$url_changed)
    page <- query[['page']]
    
    if (is.null(page) || page == "home") {
      show_view("LANDING")
      return()
    }
    
    switch(page,
           "login" = { 
             user_logged(FALSE)
             show_view("LOGIN") 
           },
           "reset_password" = show_view("RESET"),
           "reset_confirm"  = show_view("CONFIRM"),
           "dashboard"      = if (!is.null(current_user())) user_logged(TRUE)
    )
  })
  
  # 4. Guardar sesión
  observeEvent(user_logged(), {
    if (isTRUE(user_logged()) && !is.null(current_user())) {
      session$sendCustomMessage("save_user", current_user())
      update_url("dashboard")
    }
  }, ignoreInit = TRUE)
  
  # 5. Logout
  observeEvent(input$`main-btn_logout`, {
    session$sendCustomMessage("clear_user", NULL)
    current_user(NULL)
    user_logged(FALSE)
    show_view("LANDING")
    update_url("home")
  })
  
  # 6. Render UI
  output$ui_landing <- renderUI({
    if (!user_logged() && show_view() == "LANDING") landingUI("landing")
  })
  
  output$ui_login <- renderUI({
    if (!user_logged() && show_view() == "LOGIN") loginUI("login")
  })
  
  output$ui_reset <- renderUI({
    if (show_view() == "RESET") resetPasswordUI("resetpass")
  })
  
  output$ui_reset_confirm <- renderUI({
    if (show_view() == "CONFIRM") resetConfirmUI("resetconfirm")
  })
  
  output$ui_main <- renderUI({
    if (user_logged()) mainUI("main")
  })
  
  # 7. Servidores de módulos
  # ASEGÚRATE de que estos archivos acepten estos argumentos
  landingServer("landing", show_view)
  loginServer("login", pool, user_logged, current_user, show_view, update_url)
  mainServer("main", current_user, user_logged, pool)
  resetPasswordServer("resetpass", pool, show_view, update_url)
  resetConfirmServer("resetconfirm", pool, show_view, update_url)
  
  # 8. Cierre BD
  session$onSessionEnded(function() {
    try({
      if (exists("pool")) poolClose(pool)
    }, silent = TRUE)
  })
}

# Lanzar App
shinyApp(ui, server, options = list(port = 3841))