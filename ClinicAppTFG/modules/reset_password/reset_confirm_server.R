library(shiny)
library(DBI)
library(bcrypt)

resetConfirmServer <- function(id, pool, show_view, token_manual = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Estado del módulo
    user_to_reset <- reactiveVal(NULL)
    feedback_msg  <- reactiveVal("") 
    
    # Lógica del Token: Prioriza el manual (test) sobre el de la URL
    current_token <- reactive({
      if (!is.null(token_manual)) return(token_manual)
      query <- parseQueryString(session$clientData$url_search)
      query[["token"]]
    })
    
    # Validación
    observeEvent(current_token(), {
      token <- current_token()
      req(token)
      
      res <- tryCatch({
        dbGetQuery(pool, 
                   "SELECT id AS id_usuario, usuario, reset_token, token_expiry 
           FROM usuarios WHERE reset_token = ?", 
                   params = list(token))
      }, error = function(e) NULL)
      
      if (!is.null(res) && nrow(res) == 1) {
        user_to_reset(res)
      } else {
        user_to_reset("INVALID")
      }
    }, ignoreInit = FALSE)
    
    # Outputs
    output$pass_msg <- renderText({ feedback_msg() })
    
    output$form_reset <- renderUI({
      u <- user_to_reset()
      if (is.data.frame(u)) {
        tagList(
          passwordInput(ns("new_pass1"), "Nueva Contraseña"),
          passwordInput(ns("new_pass2"), "Confirmar"),
          actionButton(ns("btn_set_pass"), "Guardar"),
          textOutput(ns("pass_msg"))
        )
      } else {
        span("Enlace no válido o cargando...")
      }
    })
    
    # Guardar contraseña
    observeEvent(input$btn_set_pass, {
      req(input$new_pass1, input$new_pass2)
      if (input$new_pass1 != input$new_pass2) {
        feedback_msg("❌ No coinciden")
        return()
      }
      
      hashed <- bcrypt::hashpw(input$new_pass1, bcrypt::gensalt())
      dbExecute(pool, "UPDATE usuarios SET password_hash = ?, reset_token = NULL WHERE id = ?",
                params = list(hashed, user_to_reset()$id_usuario))
      
      feedback_msg("✅ Éxito")
      if(is.function(show_view)) show_view(FALSE)
    })
    
    # Exportar para test (IMPORTANTE)
    return(list(user = user_to_reset, msg = feedback_msg))
  })
}