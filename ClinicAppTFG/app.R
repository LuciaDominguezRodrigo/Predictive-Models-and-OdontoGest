library(shiny)

# Cargar configuración global y base de datos
source("global.R")
source("db_init.R")

# Cargar módulos
source("modules/login/login_ui.R")
source("modules/login/login_server.R")
source("modules/index/main_ui_module.R")

# UI principal
ui <- fluidPage(
  # CSS general
  tags$head(
   
    # CSS específico del login
    tags$link(rel="stylesheet", type="text/css", href="login_style.css")
  ),
  
  # Salidas dinámicas para login y dashboard
  uiOutput("ui_login"),
  uiOutput("ui_main")
)

# Server
server <- function(input, output, session){
  
  user_logged <- reactiveVal(FALSE)
  current_user <- reactiveVal(NULL)
  
  # Renderizar login si no está logueado
  output$ui_login <- renderUI({
    if(!user_logged()) loginUI("login")
  })
  
  # Renderizar dashboard si está logueado
  output$ui_main <- renderUI({
    if(user_logged()) mainUI("main")
  })
  
  # Lógica del login
  loginServer("login", pool, user_logged, current_user)
  
  # Lógica del dashboard / main UI
  mainServer("main", current_user, user_logged)
  
  # Cerrar pool al salir
  session$onSessionEnded(function() { poolClose(pool) })
}

# Ejecutar app
shinyApp(ui, server)
