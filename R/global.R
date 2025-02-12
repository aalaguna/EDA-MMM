# R/global.R
# =============================================================================
# Configuración Global de la aplicación EDA.
# Se definen opciones, se cargan las librerías necesarias y se configuran
# funciones comunes.
# =============================================================================

options(shiny.maxRequestSize = 30 * 1024^2)       # Límite de carga a 30 MB
options(future.globals.maxSize = 1000 * 1024^2)      # Manejo de datos grandes
options(DT.options = list(pageLength = 25))
options(scipen = 999)                              # Evitar notación científica
options(lubridate.week.start = 1)                  # La semana inicia en lunes

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
})

theme_set(theme_minimal())
Sys.setlocale("LC_TIME", "es_ES.UTF-8")

app_colors <- c(
  "primary"   = "#0072B2",
  "secondary" = "#E69F00",
  "tertiary"  = "#009E73",
  "warning"   = "#D55E00",
  "info"      = "#56B4E9"
)

format_num <- function(x) {
  if (is.na(x)) return(NA)
  if (!is.numeric(x)) return(x)
  if (abs(x) >= 1e9) {
    sprintf("%.2fB", x / 1e9)
  } else if (abs(x) >= 1e6) {
    sprintf("%.2fM", x / 1e6)
  } else if (abs(x) >= 1e3) {
    sprintf("%.2fK", x / 1e3)
  } else {
    format(round(x, 2), big.mark = ",", scientific = FALSE)
  }
}

log_message <- function(message) {
  cat(paste(Sys.time(), "- LOG:", message, "\n"))
}

error_handler <- function(e) {
  cat(paste(Sys.time(), "- ERROR:", e$message, "\n"))
}

notifyUser <- function(message, type = "message", duration = 4) {
  showNotification(message, type = type, duration = duration)
  log_message(message)
}

source("R/modules/common/transformations.R", local = TRUE)

log_message("✅ Archivo global.R cargado correctamente.")
