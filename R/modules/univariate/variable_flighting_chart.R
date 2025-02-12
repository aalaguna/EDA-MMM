# R/modules/univariate/variable_flighting_chart.R
# =============================================================================
# Módulo: variable_flighting_chart
# Renderiza el gráfico de flighting que compara KPI y Variable
# =============================================================================

render_variable_flighting <- function(df, kpi_univ, variable_univ, geography_univ) {
  req(df, kpi_univ, variable_univ)
  validate(
    need(variable_univ != "N/A", "Seleccione una variable válida para el análisis.")
  )
  
  date_col <- if ("Period" %in% names(df)) "Period" else if ("periodo" %in% names(df)) "periodo" else NULL
  req(date_col)
  
  data_to_plot <- df %>% 
    select(!!sym(date_col), KPI = !!sym(kpi_univ), Variable = !!sym(variable_univ))
  validate(
    need(nrow(data_to_plot) > 0, "No hay datos disponibles para graficar.")
  )
  
  tryCatch({
    p <- plot_ly(data_to_plot, x = ~get(date_col)) %>%
      add_lines(y = ~KPI, name = "KPI", line = list(color = "blue"), mode = "lines") %>%
      add_lines(y = ~Variable, name = "Variable", yaxis = "y2", line = list(color = "red"), mode = "lines") %>%
      layout(
        title = paste("KPI y Variable en el Tiempo (Geography:", geography_univ, ")"),
        xaxis = list(title = "Tiempo"),
        yaxis = list(title = "KPI", side = "left"),
        yaxis2 = list(title = "Variable", overlaying = "y", side = "right"),
        legend = list(orientation = "h", x = 0.3, y = -0.2),
        hovermode = "x unified"
      )
    p
  }, error = function(e) {
    notifyUser(paste("Error al renderizar flighting chart:", e$message), "error")
    return(NULL)
  })
}