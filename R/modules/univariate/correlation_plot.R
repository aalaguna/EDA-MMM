# R/modules/univariate/correlation_plot.R
# =============================================================================
# Módulo: correlation_plot
# Renderiza el gráfico de correlación entre KPI y variable
# =============================================================================

render_correlation_plot <- function(df, kpi_univ, variable_univ) {
  req(df, kpi_univ, variable_univ)
  validate(
    need(variable_univ != "N/A", "Seleccione una variable válida para calcular la correlación.")
  )
  
  kpi_data <- df[[kpi_univ]]
  var_data <- df[[variable_univ]]
  valid_idx <- complete.cases(kpi_data, var_data)
  
  if(sum(valid_idx) < 2){
    plot.new()
    title("Datos insuficientes para calcular correlación.")
    return()
  }
  
  corr_val <- tryCatch({
    if (sd(kpi_data[valid_idx], na.rm = TRUE) == 0 || sd(var_data[valid_idx], na.rm = TRUE) == 0) NA 
    else cor(kpi_data[valid_idx], var_data[valid_idx], use = "complete.obs")
  }, error = function(e) {
    notifyUser(paste("Error al calcular correlación:", e$message), "error")
    NA
  })
  
  plot(kpi_data, var_data,
       main = paste("Correlación:", ifelse(is.na(corr_val), "NA", round(corr_val, 2))),
       xlab = kpi_univ, ylab = variable_univ, pch = 19, col = "darkblue")
}