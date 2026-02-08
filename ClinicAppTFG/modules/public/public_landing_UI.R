landingUI <- function(id) {
  ns <- NS(id)
  
  # Usamos fluidRow y columnas de Bootstrap para el layout
  tagList(
    # --- HERO SECTION ---
    div(
      style = "background: linear-gradient(135deg, #0f172a 0%, #1e293b 100%); 
               color: white; padding: 100px 0; margin-top: -20px;",
      div(
        class = "container text-center",
        h1(style = "font-size: 4rem; font-weight: 800; margin-bottom: 20px;",
           "Tu sonrisa, el reflejo de ", 
           span(style = "color: #60a5fa;", "tu bienestar.")),
        p(class = "lead", style = "font-size: 1.5rem; color: #cbd5e1; max-width: 800px; margin: 0 auto 40px;",
          "Combinamos calidez humana con tecnología de vanguardia para ofrecerte la experiencia dental que mereces."),
        div(
          actionButton(ns("go_to_login"), "Acceso Profesionales", 
                       class = "btn-lg btn-light", 
                       style = "padding: 15px 30px; font-weight: bold; margin-right: 10px; border-radius: 10px;"),
          tags$button("Solicitar Cita Online", 
                      class = "btn btn-lg btn-outline-light", 
                      style = "padding: 15px 30px; font-weight: bold; border-radius: 10px;")
        )
      )
    ),
    
    # --- SECTION: SERVICIOS ---
    div(
      class = "container-fluid", 
      style = "background-color: #f8fafc; padding: 80px 0;",
      div(
        class = "container",
        div(class = "text-center", style = "margin-bottom: 60px;",
            span(style = "color: #2563eb; font-weight: bold; letter-spacing: 2px; text-transform: uppercase;", "Cuidado Preventivo"),
            h2(style = "font-size: 2.5rem; font-weight: 700; color: #0f172a;", "Consejos para tu Salud Bucal")
        ),
        
        div(class = "row",
            # Card 1
            div(class = "col-md-4",
                div(class = "panel panel-default", style = "border-radius: 20px; border: none; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.1); padding: 30px;",
                    div(style = "font-size: 40px; margin-bottom: 20px;", "🪥"),
                    h3(style = "font-weight: 700;", "Técnica de Cepillado"),
                    p(style = "color: #64748b;", "La clave no es la fuerza, sino el movimiento. Usa círculos suaves para proteger tus encías.")
                )
            ),
            # Card 2
            div(class = "col-md-4",
                div(class = "panel panel-default", style = "border-radius: 20px; border: none; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.1); padding: 30px;",
                    div(style = "font-size: 40px; margin-bottom: 20px;", "🍎"),
                    h3(style = "font-weight: 700;", "Alimentación"),
                    p(style = "color: #64748b;", "Los alimentos ricos en fibra ayudan a limpiar los dientes de forma natural.")
                )
            ),
            # Card 3
            div(class = "col-md-4",
                div(class = "panel panel-default", style = "border-radius: 20px; border: none; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.1); padding: 30px;",
                    div(style = "font-size: 40px; margin-bottom: 20px;", "📅"),
                    h3(style = "font-weight: 700;", "Revisiones"),
                    p(style = "color: #64748b;", "La prevención es el tratamiento más efectivo. Te esperamos cada 6 meses.")
                )
            )
        )
      )
    ),
    
    # --- FOOTER ---
    div(
      class = "container-fluid", 
      style = "background-color: white; padding: 40px 0; border-top: 1px solid #e2e8f0;",
      div(class = "container text-center", style = "color: #94a3b8;",
          p("© 2026 Clínica Dental Bienestar. Excelencia en Odontología.")
      )
    )
  )
}