library(shiny)
library(toastui)
library(shinyWidgets)

appointmentUI <- function(id){
  ns <- NS(id)
  
  tagList(
        shinyjs::useShinyjs(),
        tags$head(
      tags$style(HTML(sprintf("
        /* Asegura que las citas parezcan clicables y reciban el evento */
        .toastui-calendar-weekday-event, 
        .toastui-calendar-event-time {
          cursor: pointer !important;
          pointer-events: auto !important;
        }
        /* Evita que el contenedor del calendario bloquee clics hijos */
        .toastui-calendar-panel-resizer {
          pointer-events: none !important;
        }
      ")))
    ),
    
    div(class="container-fluid",
        div(class="row mb-3 align-items-end",
            div(class="col-md-3",
                pickerInput(
                  ns("filtro_gabinete"), "Gabinete",
                  choices = c("Todos"="0", "Gabinete 1"="1", "Gabinete 2"="2", "Gabinete 3"="3"),
                  selected="0"
                )
            ),
            div(class="col-md-3", uiOutput(ns("ui_filtro_doctor"))),
            div(class="col-md-3",
                actionButton(ns("crear_manual"), "Nueva cita", class="btn-primary")
            )
        ),
        div(
          class="shadow-sm border rounded bg-white p-2",
          calendarOutput(ns("cal"), height="750px")
        )
    )
  )
}