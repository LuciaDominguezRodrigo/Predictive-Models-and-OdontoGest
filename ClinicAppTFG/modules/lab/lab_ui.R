labUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Gestión de Trabajos de Laboratorio", class = "text-purple mb-4"),
    
    # UI condicional: Solo doctores y admin pueden crear pedidos
    uiOutput(ns("create_btn_container")),
    
    hr(),
    
    # Tabla de pedidos
    div(class = "table-responsive",
        tableOutput(ns("tabla_pedidos"))
    )
  )
}