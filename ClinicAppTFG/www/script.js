// -----------------------------
// Persistencia de sesión
// -----------------------------

// Guardar usuario en sessionStorage al hacer login
Shiny.addCustomMessageHandler('save_user', function(data) {
    sessionStorage.setItem('clinic_session', JSON.stringify(data));
});

// Borrar usuario de sessionStorage al hacer logout
// ... (Tus otros handlers de save_user se ven bien)

Shiny.addCustomMessageHandler('clear_user', function(page) {
    sessionStorage.removeItem('clinic_session'); // Borrado específico
    sessionStorage.clear(); // Borrado total por seguridad
    
    // IMPORTANTE: Informar a Shiny inmediatamente que el usuario recuperado ya es NULL
    // Esto evita que R intente usar un valor antiguo en el mismo ciclo de vida
    Shiny.setInputValue('recovered_user', null);

    const targetPage = page || "home"; // Cambiado a home por tu flujo
    const cleanUrl = window.location.protocol + "//" + window.location.host + window.location.pathname + "?page=" + targetPage;
    window.history.replaceState({}, document.title, cleanUrl);
});

$(document).on('shiny:connected', function() {
    var user = sessionStorage.getItem('clinic_session');
    // Enviamos el valor (sea el JSON o sea null) para que R sepa el estado real
    Shiny.setInputValue('recovered_user', user);
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
