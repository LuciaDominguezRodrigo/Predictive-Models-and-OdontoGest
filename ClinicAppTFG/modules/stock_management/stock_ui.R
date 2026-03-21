# ==============================================================================
# PROYECTO: ClinicAppTFG | MÓDULO: stock_ui.R
# ==============================================================================

stockUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2(icon("brain", class = "text-purple"), "IA: Análisis Predictivo de Suministros"),
    p("El sistema analiza el histórico real de la base de datos para optimizar el inventario."),
    hr(),
    
    fluidRow(
      # Panel de Control (Izquierda)
      column(width = 4,
             wellPanel(
               style = "border-top: 3px solid #8e44ad;",
               h4("Parámetros de Entrada"),
               sliderInput(ns("num_pacientes"), "Pacientes previstos (Próximo Mes):", 
                           min = 10, max = 500, value = 100, step = 10),
               br(),
               actionButton(ns("btn_predecir"), "Activar Cerebro IA", 
                            class = "btn-purple btn-block", 
                            icon = icon("microchip")),
               hr(),
               div(
                 class = "alert alert-info",
                 style = "padding: 10px; font-size: 0.9em; border-left: 5px solid #2980b9;",
                 icon("info-circle"), 
                 " ", # Espacio
                 strong("Aviso del sistema:"), 
                 br(),
                 "Al activar la IA, se consultará la tabla ", code("historico_stock"), 
                 " con registros de los últimos 24 meses."
               )
             )
      ),
      
      # Panel de Resultados (Derecha)
      column(width = 8,
             tabsetPanel(
               # Pestaña 1: Tabla Editable y Añadir Productos
               tabPanel(icon("table"), "Propuesta Detallada", 
                        br(), 
                        h4("Sugerencias de la IA (Haz doble clic para editar):"),
                        
                        # Tabla Principal
                        DT::DTOutput(ns("tabla_editable_ia")),
                        
                        br(),
                        
                        # --- SECCIÓN PARA AÑADIR PRODUCTOS MANUALMENTE ---
                        wellPanel(
                          h5(icon("plus"), "Añadir producto manualmente (Fuera de IA):"),
                          fluidRow(
                            column(6, textInput(ns("nuevo_prod_nombre"), NULL, placeholder = "Nombre del producto")),
                            column(4, numericInput(ns("nuevo_prod_cant"), NULL, value = 1, min = 1)),
                            column(2, actionButton(ns("btn_add_fila"), "", icon = icon("plus"), class = "btn-info"))
                          )
                        ),
                        # -------------------------------------------------
                        
                        br(),
                        
                        # --- NUEVA SECCIÓN DE BOTONES FINALES ---
                        fluidRow(
                          column(6, uiOutput(ns("ui_boton_confirmar"))),   # Botón Enviar a BBDD
                          column(6, uiOutput(ns("ui_boton_descargar")))   # Botón Excel
                        )
                        # ---------------------------------------
               ),
               
               # Pestaña 2: Gráfico de contraste
               tabPanel(icon("chart-bar"), "Visualización", 
                        br(),
                        plotOutput(ns("grafico_ia"), height = "400px")
               )
             )
      )
    )
  )
}