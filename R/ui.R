# =============================================================================
# Interfaz principal de la aplicación (UI).
# Definida con navbarPage y las pestañas Information, Univariate y Multivariate.
# Invoca los módulos de UI específicos para cada pestaña.
# =============================================================================

# Cargar módulos de UI
source("R/ui/information_ui.R", local = TRUE)
source("R/ui/univariate_ui.R", local = TRUE)
source("R/ui/multivariate_ui.R", local = TRUE)

ui <- navbarPage(
  id = "main-tabs",
  title = NULL,
  theme = shinythemes::shinytheme("flatly"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  # Pestaña Information
  tabPanel(
    "Information",
    ui_information()
  ),

  # Pestaña Univariate
  tabPanel(
    "Univariate",
    ui_univariate()
  ),

  # Pestaña Multivariate
  tabPanel(
    "Multivariate",
    ui_multivariate()
  )
)