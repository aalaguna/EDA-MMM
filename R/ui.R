# UI.R

# =============================================================================
# Main interface of the application (UI) with navbarPage and the tabs
# Information, Univariate, Multivariate, and KPI Transformation.
# =============================================================================

# Load the UI modules
source("R/ui/information_ui.R", local = TRUE)
source("R/ui/univariate_ui.R", local = TRUE)
source("R/ui/multivariate_ui.R", local = TRUE)
source("R/ui/kpi_transformation_ui.R", local = TRUE) # KPI Transformation tab

ui <- navbarPage(
  id    = "main-tabs",
  title = "EDA Analytics Dashboard",  # Añadir un título explícito
  theme = shinythemes::shinytheme("flatly"),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "custom.js") # Include custom JavaScript
  ),
  
  # Information tab (usar tabPanel en lugar de simplemente incluir contenido)
  tabPanel(
    title = "Information",
    ui_information()
  ),
  
  # Univariate tab
  tabPanel(
    title = "Univariate",
    ui_univariate()
  ),
  
  # Multivariate tab
  tabPanel(
    title = "Multivariate",
    ui_multivariate()
  ),
  
  # KPI Transformation tab
  tabPanel(
    title = "KPI",
    ui_kpi_transformation()
  )
)