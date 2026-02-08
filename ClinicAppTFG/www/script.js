// -----------------------------
// Persistencia de sesión
// -----------------------------

// Guardar usuario en sessionStorage al hacer login
Shiny.addCustomMessageHandler('save_user', function(data) {
    sessionStorage.setItem('clinic_session', JSON.stringify(data));
});

// Borrar usuario de sessionStorage al hacer logout
Shiny.addCustomMessageHandler('clear_user', function(page) {
    sessionStorage.clear();
    
    // Si no viene página, por defecto vamos a login
    const targetPage = page || "login";
    const cleanUrl = window.location.protocol + "//" + window.location.host + window.location.pathname + "?page=" + targetPage;
    
    window.history.replaceState({}, document.title, cleanUrl);
    
    var passInput = document.getElementById('login-contraseña'); // Ojo con el namespace
    if(passInput) passInput.value = '';
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
    let query = new URLSearchParams(window.location.search);
    let page = query.get('page');

    // Evitar acceder a dashboard si no hay usuario
    if (page === "dashboard" && !sessionStorage.getItem('clinic_session')) {
        page = "login";
        history.replaceState({}, document.title, "?page=login");
    }

    Shiny.setInputValue('url_changed', window.location.search, {priority: 'event'});
};

// Función para actualizar la URL sin recargar
Shiny.addCustomMessageHandler('update_url', function(page_name) {
    history.pushState({page: page_name}, '', '?page=' + page_name);
});
