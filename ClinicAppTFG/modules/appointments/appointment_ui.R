# En appointment_ui.R
appointmentUI <- function(id){
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
        /* Estilo para el botón morado corporativo */
        .btn-purple {
          background-color: #7e57c2 !important;
          border-color: #7e57c2 !important;
          color: white !important;
          border-radius: 50px !important; /* Bordes redondeados */
          padding: 8px 20px !important;
          font-weight: 600 !important;
          transition: all 0.3s ease;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .btn-purple:hover {
          background-color: #6a49a8 !important;
          transform: translateY(-1px);
          box-shadow: 0 4px 8px rgba(0,0,0,0.15);
        }
        
        /* Ajuste para que el calendario y los filtros respiren mejor */
        .filters-container {
          background: #f8f9fa;
          padding: 15px;
          border-radius: 12px;
          margin-bottom: 20px;
          border: 1px solid #eef0f2;
        }

        .toastui-calendar-weekday-event, 
        .toastui-calendar-event-time { cursor: pointer !important; }
        .toastui-calendar-panel-resizer { pointer-events: none !important; }
      
      
      /* Botón Negro (para Cerrar/Cancelar) */
        .btn-dark-custom {
          background-color: #2d2d2d !important;
          border-color: #2d2d2d !important;
          color: white !important;
          border-radius: 50px !important;
          padding: 8px 20px !important;
          font-weight: 600 !important;
          transition: all 0.3s ease;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .btn-dark-custom:hover {
          background-color: #000000 !important;
          transform: translateY(-1px);
        }
        
        /* Botón Naranja Pastel (para Anular Cita) */
        .btn-orange-pastel {
          background-color: #ffb38a !important; /* Naranja suave */
          border-color: #ffb38a !important;
          color: white !important;
          border-radius: 50px !important;
          padding: 8px 20px !important;
          font-weight: 600 !important;
          transition: all 0.3s ease;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .btn-orange-pastel:hover {
          background-color: #ffa06d !important;
          transform: translateY(-1px);
        }
      "))
    ),
    
    div(class="container-fluid",
        # Envolvemos el output en un div con la nueva clase
        div(class="filters-container shadow-sm", 
            uiOutput(ns("controles_staff"))
        ),
        
        div(
          class = "shadow-sm border rounded bg-white p-2",
          style = "height: 650px; overflow: hidden;",  
          
          calendarOutput(ns("cal"), height = "100%")
        ), 
        uiOutput(ns("lista_solicitudes_pendientes"))
    )
  )
}