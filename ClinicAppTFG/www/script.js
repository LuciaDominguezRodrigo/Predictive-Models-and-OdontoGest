// Funciones de Persistencia (LocalStorage)
Shiny.addCustomMessageHandler('save_user', function(data) {
    localStorage.setItem('clinic_session', JSON.stringify(data));
});

Shiny.addCustomMessageHandler('clear_user', function(data) {
    localStorage.removeItem('clinic_session');
});

// Al conectar, recuperar datos y enviarlos a Shiny
$(document).on('shiny:connected', function() {
    var user = localStorage.getItem('clinic_session');
    Shiny.setInputValue('recovered_user', user);
});

// Detectar botón 'Atrás' del navegador
window.onpopstate = function(event) {
    Shiny.setInputValue('url_changed', window.location.search, {priority: 'event'});
};

// Función para actualizar la URL sin recargar
Shiny.addCustomMessageHandler('update_url', function(page_name) {
    history.pushState({page: page_name}, '', '?page=' + page_name);
});