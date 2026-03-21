# ==============================================================================
# PROYECTO: ClinicAppTFG | MÓDULO: diagnostico_ui.R
# ==============================================================================

diagnosticoUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2(icon("user-md", class = "text-purple"), "IA: Asistente de Diagnóstico Clínico"),
    p("Evaluación de riesgo de patologías orales mediante inteligencia artificial."),
    hr(),
    
    fluidRow(
      # Panel de Control (Izquierda)
      column(width = 4,
             wellPanel(
               style = "border-top: 3px solid #8e44ad;",
               h4("Exploración Clínica"),
               numericInput(ns("edad"), "Edad del Paciente:", 30, min = 0),
               sliderInput(ns("placa"), "Índice de Placa (%):", 0, 100, 20),
               sliderInput(ns("sangrado"), "Sangrado al Sondaje (%):", 0, 100, 10),
               numericInput(ns("bolsa"), "Profundidad Bolsa (mm):", 2, min = 1, max = 15),
               sliderInput(ns("glucosa"), "Nivel de Glucosa (mg/dL):", 70, 250, 100),
               sliderInput(ns("higiene"), "Cepillados al día:", 0, 4, 2),
               checkboxInput(ns("fumador"), "Paciente Fumador", FALSE),
               br(),
               actionButton(ns("btn_analizar"), "Ejecutar Diagnóstico IA", 
                            class = "btn-purple btn-block", icon = icon("microchip"))
             ),
             uiOutput(ns("alerta_resultado"))
      ),
      
      # Panel de Visualización (Derecha)
      column(width = 8,
             tabsetPanel(
               tabPanel(icon("chart-pie"), "Probabilidades", 
                        br(),
                        plotly::plotlyOutput(ns("plot_probs"))),
               tabPanel(icon("search"), "Explicación del Modelo", 
                        br(),
                        plotly::plotlyOutput(ns("plot_explicacion")),
                        p(class="small text-muted", "Este gráfico muestra qué factores clínicos están influyendo más en la predicción actual."))
             )
      )
    )
  )
}