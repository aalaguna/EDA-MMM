# R/modules/univariate/transformations_summary.R
# =============================================================================
# Módulo: transformations_summary
# Muestra información y resumen de la transformación aplicada
# =============================================================================

# render_transformations_summary <- function(df, input) {
#   req(input$transformation_univ, df, input$variable_univ)
#   validate(
#     need(input$variable_univ != "N/A", "Seleccione una variable válida para el resumen.")
#   )
#   
#   box_data <- df[[input$variable_univ]]
#   box_max <- max(box_data, na.rm = TRUE)
#   
#   cat("Transformación Seleccionada:", input$transformation_univ, "\n")
#   if (input$transformation_univ %in% c("S Origin", "S Shaped")) {
#     cat("Alpha:", input$alpha_univ, "\n")
#     cat("Beta:", input$beta_univ, "\n")
#     cat("% MaxVal:", input$maxval_univ, "\n")
#   }
#   cat("Decay:", input$decay_univ, "\n")
#   cat("Lag:", input$lag_univ, "\n")
#   cat("Máximo (Boxplot):", box_max, "\n")
#   cat("Geography Seleccionada:", input$geography_univ, "\n")
# }

render_transformations_summary <- function(df, input) {
  req(input$transformation_univ, df, input$variable_univ)
  validate(
    need(input$variable_univ != "N/A", "Seleccione una variable válida para el resumen.")
  )
  
  # Obtener los datos de la variable seleccionada
  box_data <- df[[input$variable_univ]]
  box_data <- box_data[!is.na(box_data)]  # Filtrar NA
  
  # Verificar que hay suficientes datos
  if (length(box_data) == 0) {
    cat("No hay suficientes datos para calcular los valores sugeridos.\n")
    return()
  }
  
  # Calcular la media, la desviación estándar y el máximo
  data_mean <- mean(box_data, na.rm = TRUE)
  data_sd <- sd(box_data, na.rm = TRUE)
  box_max <- max(box_data, na.rm = TRUE)
  
  # 1️⃣ Max Value % (Boxplot, sin outliers)
  q1 <- quantile(box_data, 0.25, na.rm = TRUE)
  q3 <- quantile(box_data, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  upper_limit <- q3 + 1.5 * iqr
  max_not_outlier <- ifelse(box_max > upper_limit, upper_limit, box_max)
  maxval_percent_boxplot <- (max_not_outlier / box_max) * 100
  
  # 2️⃣ Max Value % (Three SD)
  three_sd_value <- data_mean + 3 * data_sd
  maxval_percent_three_sd <- (three_sd_value / box_max) * 100
  
  # 3️⃣ Max Value % (Average Mean-Max)
  avg_mean_max <- (data_mean + box_max) / 2
  maxval_percent_avg <- (avg_mean_max / box_max) * 100
  
  # Mostrar los valores calculados en porcentaje
  cat("🔹 Max Value % (Boxplot, sin outliers):", round(maxval_percent_boxplot, 2), "%\n")
  cat("🔹 Max Value % (Three SD):", round(maxval_percent_three_sd, 2), "%\n")
  cat("🔹 Max Value % (Average Mean-Max):", round(maxval_percent_avg, 2), "%\n")
}
