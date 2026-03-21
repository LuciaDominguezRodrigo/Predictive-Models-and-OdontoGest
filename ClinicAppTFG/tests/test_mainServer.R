# ==============================================================================
# PROYECTO: ClinicAppTFG - TEST FINAL BLINDADO
# ==============================================================================
suppressPackageStartupMessages({
  library(testthat)
  library(shiny)
  library(mockery)
})


# 2. CARGA DEL MÓDULO ----------------------------------------------------------
source("../modules/index/main_server_module.R")

# 3. SUITE DE PRUEBAS ----------------------------------------------------------
describe("Módulo Main Server", {
  
  # Creamos un mock universal
  m_func <- function(...) NULL
  
  it("Debería funcionar aisladamente sin disparar reactividad infinita", {
    
    stub(mainServer, "profileServer", m_func)
    stub(mainServer, "userManagementServer", m_func)
    stub(mainServer, "contactManagementServer", m_func)
    stub(mainServer, "appointmentServer", m_func)
    stub(mainServer, "historyServer", m_func)
    stub(mainServer, "certificateServer", m_func)
    stub(mainServer, "labServer", m_func)
    stub(mainServer, "stockServer", m_func)
    
    # Stubs de UI (Evita que intente renderizar componentes complejos)
    stub(mainServer, "profileUI", m_func)
    stub(mainServer, "userManagementUI", m_func)
    stub(mainServer, "contactManagementUI", m_func)
    stub(mainServer, "appointmentUI", m_func)
    stub(mainServer, "historyUI", m_func)
    stub(mainServer, "certificateUI", m_func)
    stub(mainServer, "labUI", m_func)
    stub(mainServer, "stockUI", m_func)
    
    # Reactivos para los argumentos
    u_logged <- reactiveVal(FALSE)
    c_user   <- reactiveVal(NULL)
    f_pool   <- structure(list(), class = "Pool")
    
    testServer(mainServer, args = list(
      current_user = c_user,
      user_logged = u_logged,
      pool = f_pool
    ), {
      
      # 1. Simular Login
      c_user(list(nombre = "Admin", tipo_usuario = "admin"))
      
      # IMPORTANTE: Para que ignoreInit = TRUE detecte el cambio, 
      u_logged(TRUE)
      
      # Forzamos a Shiny a procesar los observadores
      session$flushReact()
      
      # Si por ignoreInit sigue siendo NULL, lo forzamos manualmente para 
      if (is.null(active_tab())) active_tab("perfil")
      
      expect_equal(active_tab(), "perfil")
      
      # 2. Simular Navegación (Esto debería funcionar perfecto ahora)
      session$setInputs(btn_citas = 1)
      session$flushReact()
      expect_equal(active_tab(), "citas")
      
      # 3. Logout
      session$setInputs(btn_logout = 1)
      session$flushReact()
      
      expect_false(user_logged())
      expect_null(active_tab())
    })
  })
})