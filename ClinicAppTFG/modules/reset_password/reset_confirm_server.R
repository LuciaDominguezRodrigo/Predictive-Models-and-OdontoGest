library(shiny)
library(DBI)
library(bcrypt)
library(shinyjs)

resetConfirmServer <- function(id, pool, show_view, update_url, token_manual = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Estado del módulo
    user_to_reset <- reactiveVal(NULL)
    feedback_msg  <- reactiveVal("") 
    
    # Token manual para tests
    current_token <- reactive({
      if (!is.null(token_manual)) return(token_manual)
      query <- parseQueryString(session$clientData$url_search)
      query[["token"]]
    })
    
    # Validación del token
    observeEvent(current_token(), {
      req(pool)
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
          # Nueva Contraseña
          div(
            class = "mb-2",
            tags$label("Nueva Contraseña", class = "block text-sm font-bold text-gray-600 uppercase tracking-wider mb-2 ml-1"),
            passwordInput(ns("new_pass1"), label = NULL, placeholder = "••••••••")
          ),
          
          # Confirmar
          div(
            class = "mb-6",
            tags$label("Confirmar Contraseña", class = "block text-sm font-bold text-gray-600 uppercase tracking-wider mb-2 ml-1"),
            passwordInput(ns("new_pass2"), label = NULL, placeholder = "••••••••")
          ),
          
          # Botón Estilizado
          actionButton(
            ns("btn_set_pass"), 
            "Guardar Nueva Contraseña",
            class = "w-full py-5 rounded-xl text-white text-xl font-bold tracking-wide bg-clinicPurple hover:bg-purple-700 active:scale-[0.98] transition-all duration-200 shadow-lg shadow-purple-200"
          ),
          
          # Mensaje de error/éxito
          div(class = "mt-4 text-center text-lg", textOutput(ns("pass_msg")))
        )
      } else {
        div(class="p-6 bg-red-50 text-red-600 rounded-2xl text-center text-lg font-medium", 
            "⚠️ El enlace no es válido o ha expirado.")
      }
    })
    
    # Guardar contraseña
    observeEvent(input$btn_set_pass, {
      req(pool)
      req(input$new_pass1, input$new_pass2)
      
      if (input$new_pass1 != input$new_pass2) {
        feedback_msg("❌ No coinciden")
        return()
      }
      
      hashed <- bcrypt::hashpw(input$new_pass1, bcrypt::gensalt())
      dbExecute(pool,
                "UPDATE usuarios SET password_hash = ?, reset_token = NULL WHERE id = ?",
                params = list(hashed, user_to_reset()$id_usuario)
      )
      
      feedback_msg("✅ Contraseña actualizada correctamente")
      
      # Volver a login tras 1.5s
      shinyjs::delay(1500, {
        # 1. Cambiamos la URL visualmente (opcional pero recomendado)
        updateQueryString("?page=login", mode = "push", session = session)
        
        # 2. Cambiamos el estado reactivo para que app.R renderice el Login
        show_view("LOGIN")
        update_url("login")
      })
    })
    
    # Exportar para tests
    return(list(user = user_to_reset, msg = feedback_msg))
  })
}
