library(shiny)

loginUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Importar CSS específico del login
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "modules/login/login_style.css")
    ),
    
    div(class="login-box",
        h2("Login Clínica"),
        textInput(ns("usuario"), "Usuario"),
        passwordInput(ns("contraseña"), "Contraseña"),
        actionButton(ns("btn_login"), "Ingresar"),
        div(class="login-msg", verbatimTextOutput(ns("login_msg")))
    )
  )
}
