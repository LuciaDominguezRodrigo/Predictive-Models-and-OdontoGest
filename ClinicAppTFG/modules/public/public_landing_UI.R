landingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;700;800&display=swap"),
      tags$style(HTML("
        body { font-family: 'Inter', sans-serif; color: #334155; }
        
        /* Tipografía Principal */
        h1 { font-size: 4rem !important; font-weight: 800; line-height: 1.1; color: #ffffff; text-shadow: 2px 2px 10px rgba(0,0,0,0.3); }
        h2 { font-size: 2.8rem !important; font-weight: 700; color: #1e293b; }
        h3 { font-size: 1.8rem !important; font-weight: 700; color: #6a0dad; }
        p { font-size: 1.4rem !important; line-height: 1.7; color: #475569; }
        
        /* HERO SECTION: Filtro mucho más suave para ver la foto */
        .hero-section {
          background: linear-gradient(rgba(45, 10, 80, 0.35), rgba(45, 10, 80, 0.35)), 
                      url('https://images.unsplash.com/photo-1629909613654-28e377c37b09?q=80&w=2070&auto=format&fit=crop');
          background-size: cover;
          background-position: center;
          padding: 160px 0;
          backdrop-filter: contrast(110%); /* Resalta los detalles de la foto */
        }

        /* Tarjetas con Altura Igualada */
        .equal-height-row {
          display: flex;
          flex-wrap: wrap;
        }
        
        .card-dental {
          border: none;
          border-radius: 25px;
          overflow: hidden;
          background: #ffffff;
          box-shadow: 0 10px 40px rgba(0,0,0,0.06);
          margin-bottom: 30px;
          display: flex;
          flex-direction: column;
          height: 100%; 
          transition: transform 0.3s ease;
        }
        
        .card-dental:hover { transform: translateY(-10px); }
        
        .card-img {
          height: 260px;
          background-size: cover;
          background-position: center;
          flex-shrink: 0;
        }

        .card-body {
          padding: 40px;
          flex-grow: 1; 
          display: flex;
          flex-direction: column;
        }

        /* Footer Morado Blanco Puro */
        .footer {
          background-color: #2d0a50; 
          color: white !important;
          padding: 80px 0 40px;
        }
        .footer h4 { color: #ffffff !important; font-weight: 800; font-size: 2rem !important; margin-bottom: 25px; }
        .footer p { color: #ffffff !important; font-size: 1.4rem !important; opacity: 0.9; }
      "))
    ),
    
    # --- HERO SECTION ---
    div(class = "hero-section text-center",
        div(class = "container",
            h1("Salud dental de vanguardia"),
            p(style = "max-width: 900px; margin: 25px auto; color: #ffffff !important; font-size: 2.4rem !important; font-weight: 500; text-shadow: 1px 1px 5px rgba(0,0,0,0.2);", 
              "Expertos en odontología avanzada con tecnología de última generación."),
            div(style = "margin-top: 50px;",
                actionButton(ns("go_to_login"), "Acceso Profesionales", class = "btn", 
                             style = "padding: 20px 50px; font-size: 1.8rem; font-weight: 700; border-radius: 15px; background: #6a0dad; color: white; border: none; box-shadow: 0 10px 20px rgba(106, 13, 173, 0.4);"),
                tags$button("Solicitar Cita", class = "btn btn-outline-light", 
                            style = "padding: 20px 50px; font-size: 1.8rem; font-weight: 700; border-radius: 15px; margin-left: 20px; border-width: 2px;")
            )
        )
    ),
    
    # --- SERVICIOS ---
    div(class = "container", style = "margin-top: 100px; margin-bottom: 100px;",
        div(class = "text-center", style = "margin-bottom: 80px;",
            h2("Servicios Especializados")
        ),
        div(class = "row equal-height-row",
            div(class = "col-md-4",
                div(class = "card-dental",
                    div(class = "card-img", style = "background-image: url('https://images.unsplash.com/photo-1598256989800-fe5f95da9787?q=80&w=600&auto=format&fit=crop');"),
                    div(class = "card-body",
                        h3("Gabinete Dental"),
                        p("Instalaciones equipadas para realizar tratamientos complejos con la máxima higiene.")
                    )
                )
            ),
            div(class = "col-md-4",
                div(class = "card-dental",
                    div(class = "card-img", style = "background-image: url('https://images.unsplash.com/photo-1600170311833-c2cf5280ce49?q=80&w=500&auto=format&fit=crop');"),
                    div(class = "card-body",
                        h3("Diagnóstico 3D"),
                        p("Radiología digital y escaneo intraoral para un diagnóstico exacto y rápido.")
                    )
                )
            ),
            div(class = "col-md-4",
                div(class = "card-dental",
                    div(class = "card-img", style = "background-image: url('https://images.unsplash.com/photo-1445527815219-ecbfec67492e?q=80&w=500&auto=format&fit=crop');"),
                    div(class = "card-body",
                        h3("Ortodoncia"),
                        p("Sistemas modernos de alineación dental diseñados para tu comodidad diaria.")
                    )
                )
            )
        )
    ),
    
    # --- FOOTER ---
    tags$footer(class = "footer",
                div(class = "container",
                    div(class = "row",
                        div(class = "col-md-4",
                            h4("Clínica Bienestar"),
                            p("Innovación médica para tu salud bucodental.")
                        ),
                        div(class = "col-md-4",
                            h4("Contacto"),
                            p("📍 Centro Médico, Planta 2"),
                            p("📞 900 123 456"),
                            p("✉️ contacto@bienestardental.com")
                        ),
                        div(class = "col-md-4",
                            h4("Horarios"),
                            p("Lunes a Viernes: 8:00 - 21:00"),
                            p("Sábados: Urgencias")
                        )
                    ),
                    div(class = "text-center", 
                        style = "border-top: 1px solid rgba(255,255,255,0.2); margin-top: 50px; padding-top: 30px;",
                        p(style = "opacity: 0.9; font-size: 1.2rem !important; color: white !important;", 
                          "© 2026 Clínica Dental Bienestar. Registro Sanitario Nº 12345.")
                    )
                )
    )
  )
}