# R/modules/univariate/s_curve_helper.R
# =============================================================================
# Módulo: s_curve_helper
# Funciones auxiliares para la creación de gráficos S-Curve
# =============================================================================

create_flighting_chart <- function(data_chart, alpha, beta, max_val_pct, decay, lag, var_name) {
  req(data_chart)
  
  tryCatch({
    plot_ly(data_chart, x = ~Period) %>%
      add_lines(y = ~value, name = var_name, line = list(color = "blue")) %>%
      layout(
        title = "Variable en el Tiempo",
        xaxis = list(title = "Periodo"),
        yaxis = list(title = var_name),
        showlegend = TRUE
      )
  }, error = function(e) {
    notifyUser(paste("Error en create_flighting_chart:", e$message), "error")
    return(NULL)
  })
}

create_s_curve_chart <- function(data_chart, alpha, beta, max_val_pct, decay, lag, var_name) {
  req(data_chart)
  
  data_values <- data_chart$value[!is.na(data_chart$value)]
  if (length(data_values) == 0) {
    notifyUser("No hay datos válidos para crear S-Curve.", "error")
    return(NULL)
  }
  
  x_values <- seq(0, max(data_values, na.rm = TRUE) * 1.2, length.out = 100)
  y_values <- s_curve_transform(x_values, "s-shaped", alpha, beta, max_val_pct)
  
  df_curve <- data.frame(x = x_values, y = y_values)
  
  tryCatch({
    plot_ly(df_curve, x = ~x) %>%
      add_lines(y = ~y, name = "S-Curve", line = list(color = "red")) %>%
      layout(
        title = "S-Curve",
        xaxis = list(title = "Valor Original"),
        yaxis = list(title = "Valor Transformado"),
        showlegend = TRUE
      )
  }, error = function(e) {
    notifyUser(paste("Error en create_s_curve_chart:", e$message), "error")
    return(NULL)
  })
}

s_curve_transform <- function(x, type = c("s-origin", "s-shaped"), alpha, beta, max_val_pct) {
  type <- match.arg(type)
  
  if (type == "s-origin") {
    return(max_val_pct * (1 / (1 + exp(-alpha * (x - beta)))))
  } else {
    return(max_val_pct * (1 / (1 + (x/beta)^(-alpha))))
  }
}