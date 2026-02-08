// -----------------------------
// Funciones de Persistencia en sesión
// -----------------------------

// Guardar usuario en sessionStorage al hacer login
Shiny.addCustomMessageHandler('save_user', function(data) {
    sessionStorage.setItem('clinic_session', JSON.stringify(data));
});

// Borrar usuario de sessionStorage al hacer logout
Shiny.addCustomMessageHandler('clear_user', function(data) {
    sessionStorage.removeItem('clinic_session');
});

// Al conectar, recuperar datos de sessionStorage y enviarlos a Shiny
$(document).on('shiny:connected', function() {
    var user = sessionStorage.getItem('clinic_session');
    if (user) {
        Shiny.setInputValue('recovered_user', user);
    }
});

// Detectar botón 'Atrás' del navegador
window.onpopstate = function(event) {
    Shiny.setInputValue('url_changed', window.location.search, {priority: 'event'});
};

// Función para actualizar la URL sin recargar
Shiny.addCustomMessageHandler('update_url', function(page_name) {
    history.pushState({page: page_name}, '', '?page=' + page_name);
});
