userManagementUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        .admin-card {
          background: white;
          border-radius: 20px;
          box-shadow: 0 10px 25px rgba(0,0,0,0.05);
          padding: 40px;
          margin-bottom: 30px;
          border: 1px solid #f0f0f0;
        }
        .form-label { font-weight: 600; color: #4b5563; margin-bottom: 8px; }
        .form-control, .selectize-input {
          padding: 12px 15px !important;
          border-radius: 12px !important;
          border: 1px solid #d1d5db !important;
          font-size: 16px !important;
        }
        .btn-register {
          background-color: #6d28d9;
          color: white;
          padding: 12px 30px;
          border-radius: 12px;
          font-weight: bold;
          border: none;
          transition: all 0.3s;
        }
        .btn-register:hover { background-color: #5b21b6; transform: translateY(-2px); color: white; }
      "))
    ),
    
    div(
      class = "container-fluid bg-light py-5", 
      div(
        class = "container",
        
        # --- SECCIÓN A: ALTA DE USUARIOS ---
        div(
          class = "admin-card mx-auto",
          style = "max-width: 900px;",
          div(class = "mb-4",
              span(class = "badge rounded-pill bg-primary mb-2", "ADMINISTRACIÓN"),
              h2(class = "fw-bold text-dark", "Alta de Nuevo Usuario"),
              p(class = "text-muted", "Gestión de credenciales para personal clínico y pacientes.")
          ),
          
          div(class = "row g-4 mb-3",
              div(class = "col-md-6",
                  tags$label("Nombre y Apellidos", class = "form-label"),
                  textInput(ns("nombre"), NULL, placeholder = "Ej. Juan Pérez", width = "100%")
              ),
              div(class = "col-md-6",
                  tags$label("Correo Electrónico", class = "form-label"),
                  textInput(ns("email"), NULL, placeholder = "correo@ejemplo.com", width = "100%")
              )
          ),
          
          div(class = "row g-4 mb-3",
              div(class = "col-md-4",
                  tags$label("Nombre de Usuario", class = "form-label"),
                  textInput(ns("usuario"), NULL, placeholder = "usuario123", width = "100%")
              ),
              div(class = "col-md-4",
                  tags$label("Teléfono", class = "form-label"),
                  textInput(ns("telefono"), NULL, placeholder = "600000000", width = "100%")
              ),
              div(class = "col-md-4",
                  tags$label("Contraseña Temporal", class = "form-label"),
                  passwordInput(ns("password"), NULL, placeholder = "••••••••", width = "100%")
              )
          ),
          
          div(class = "row mb-4",
              div(class = "col-12",
                  tags$label("Rol asignado en el sistema", class = "form-label"),
                  selectInput(ns("tipo_usuario"), NULL, 
                              choices = c("Paciente" = "paciente", 
                                          "Recepción" = "recepcion", 
                                          "Doctor" = "doctor",
                                          "Higienista dental" = "higienista", 
                                          "Laboratorio" = "laboratorio",
                                          "Comercial" = "comercial"
                                          ), 
                              width = "100%")
              )
          ),
          
          div(class = "d-flex justify-content-end border-top pt-4",
              actionButton(ns("btn_save_user"), "Registrar Usuario", class = "btn-register")
          )
        ),
        
        # --- SECCIÓN B: PANEL DE GESTIÓN (Baneo y Eliminación) ---
        uiOutput(ns("admin_panel_ui"))
      )
    )
  )
}