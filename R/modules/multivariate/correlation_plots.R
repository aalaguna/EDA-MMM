# R/modules/multivariate/correlation_plots.R
# =============================================================================
# Módulo: correlation_plots
# Renderiza los gráficos de correlación
# =============================================================================

render_kpi_correlation <- function(df, kpi) {
  req(df, kpi)
  validate(
    need(kpi %in% names(df), "KPI no encontrado en los datos")
  )
  
  kpi_data <- df[[kpi]]
  sum_data <- df$sum_vars
  
  corr_val <- cor(kpi_data, sum_data, use = "complete.obs")
  
  plot_ly(x = ~sum_data, y = ~kpi_data, type = 'scatter', mode = 'markers',
          marker = list(color = 'darkgreen', size = 8)) %>%
    layout(title = paste("Correlación:", round(corr_val, 2)),
           xaxis = list(title = "Variable Sumada"),
           yaxis = list(title = kpi),
           showlegend = FALSE)
}

render_correlation_matrix <- function(df, input) {
  req(df)
  
  vars <- c(input$var1_multi, input$var2_multi, input$var3_multi)
  if (!is.null(input$var4_multi) && input$var4_multi != "None") {
    vars <- c(vars, input$var4_multi)
  }
  vars <- intersect(vars, names(df))
  
  corr_matrix <- cor(df[vars], use = "complete.obs")
  
  plot_ly(
    x = vars,
    y = vars,
    z = corr_matrix,
    type = "heatmap",
    colors = colorRamp(c("blue", "white", "red"))
  ) %>%
    layout(
      title = "Matriz de Correlación",
      xaxis = list(title = ""),
      yaxis = list(title = "")
    )
}