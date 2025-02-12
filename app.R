# R/app.R
# =============================================================================
# Punto de entrada de la aplicación EDA.
# Carga la configuración global, la lógica del servidor y la interfaz (UI). 
# (orquestación de módulos) y lanza la aplicación.
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
})

# Cargar configuraciones globales y funciones comunes
source("R/global.R", local = TRUE)

# Cargar interfaz y módulos del servidor
source("R/server.R", local = TRUE)
source("R/ui.R", local = TRUE)


# Iniciar la aplicación Shiny
shinyApp(ui = ui, server = server)
