# =============================================================================
# Global configuration of the EDA application.
# Defines options and loads the necessary libraries.
# =============================================================================

options(shiny.maxRequestSize = 30 * 1024^2)       # Upload limit set to 30 MB
options(future.globals.maxSize = 1000 * 1024^2)   # Handle large datasets
options(DT.options = list(pageLength = 25))
options(scipen = 999)                             # Avoid scientific notation
options(lubridate.week.start = 1)                 # Week starts on Monday

# Función para verificar e instalar paquetes faltantes
ensure_packages <- function(packages) {
  missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
  if (length(missing_packages) > 0) {
    message("Instalando paquetes faltantes: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages)
  }
}

# Lista de todos los paquetes necesarios
required_packages <- c(
  "shiny", "dplyr", "tidyr", "ggplot2", "corrplot", "zoo", "stringr", 
  "DT", "plotly", "shinyjs", "gridExtra", "scales", "purrr", "shinythemes", 
  "shinyWidgets", "lubridate", "moments", "jsonlite", "RColorBrewer", 
  "shinycssloaders"
)

# Verificar e instalar paquetes faltantes
ensure_packages(required_packages)

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(corrplot)
  library(zoo)
  library(stringr)
  library(DT)
  library(plotly)
  library(shinyjs)
  library(gridExtra)
  library(scales)
  library(purrr)
  library(shinythemes)
  library(shinyWidgets)
  library(lubridate)  # Asegurarnos de que este paquete se carga
  library(moments)    # Para funciones estadísticas (skewness, kurtosis)
})

theme_set(theme_minimal())
Sys.setlocale("LC_TIME", "en_US.UTF-8")