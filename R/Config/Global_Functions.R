# Global_Functions.R

# =============================================================================
# Global functions used throughout the EDA application.
# =============================================================================

# Helper function to format numbers with K, M, B, etc.
format_num <- function(x) {
  # Formats a number with K, M, B suffixes for thousands, millions, and billions
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

# Logging function
log_message <- function(message) {
  # Logs a message to the console with a timestamp
  cat(paste(Sys.time(), "- LOG:", message, "\n"))
}

# Error handler
error_handler <- function(e) {
  # Logs an error to the console with a timestamp
  cat(paste(Sys.time(), "- ERROR:", e$message, "\n"))
}

# Show notification and log
notifyUser <- function(message, type = "message", duration = 4) {
  # Displays a notification to the user and logs the message
  showNotification(message, type = type, duration = duration)
  log_message(message)
}
