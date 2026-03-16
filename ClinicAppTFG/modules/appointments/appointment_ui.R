library(shiny)
library(toastui)
library(shinyWidgets)

appointmentUI <- function(id){
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
        .toastui-calendar-weekday-event, 
        .toastui-calendar-event-time { cursor: pointer !important; }
        .toastui-calendar-panel-resizer { pointer-events: none !important; }
        
      "))
    ),
    
    div(class="container-fluid",
        # Controles que aparecen solo para staff
        uiOutput(ns("controles_staff")),
        
        div(
          class="shadow-sm border rounded bg-white p-2",
          calendarOutput(ns("cal"), height="750px")
        )
    )
  )
}