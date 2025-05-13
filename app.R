# =============================================================================
# Entry point of the EDA application.
# Loads global configuration, server logic, and user interface (UI),
# then launches the application.
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
})

# Global configuration and common functions (styles are included)
source("R/global.R", local = TRUE)

# Load UI and server modules
source("R/server.R", local = TRUE)
source("R/ui.R", local = TRUE)

# Start the Shiny application
shinyApp(ui = ui, server = server)
