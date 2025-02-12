# R/modules/multivariate/sum_variables_charts.R
# =============================================================================
# Módulo: sum_variables_charts
# Renderiza los gráficos de variables sumadas
# =============================================================================

render_sum_variables_chart <- function(df) {
  req(df)

  date_col <- if ("Period" %in% names(df)) "Period" else "periodo"

  plot_ly(df, x = as.formula(paste0("~", date_col)), y = ~sum_vars,
          type = 'scatter', mode = 'lines+markers',
          line = list(color = 'blue'),
          marker = list(color = 'blue')) %>%
    layout(title = "Variables Sumadas - Flighting Lineal",
           xaxis = list(title = "Tiempo"),
           yaxis = list(title = "Valor Sumado"),
           showlegend = FALSE)
}

render_transformed_variables_chart <- function(df) {
  req(df)

  date_col <- if ("Period" %in% names(df)) "Period" else "periodo"

  plot_ly(df, x = as.formula(paste0("~", date_col)), y = ~trans_sum_vars,
          type = 'scatter', mode = 'lines+markers',
          line = list(color = 'red'),
          marker = list(color = 'red')) %>%
    layout(title = "Variables Sumadas Transformadas",
           xaxis = list(title = "Tiempo"),
           yaxis = list(title = "Valor Transformado"),
           showlegend = FALSE)
}




