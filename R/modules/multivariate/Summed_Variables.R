# Summed_Variables.R

# =============================================================================
# Function that renders a flighting chart for summed variables
# =============================================================================

render_sum_variables_chart <- function(df) {
  req(df)
  if (!"sum_vars" %in% names(df)) {
    return(NULL)
  }
  
  date_col <- if ("Period" %in% names(df)) "Period" else "periodo"
  req(date_col)
  
 
  df2 <- df %>%
    mutate(
      date = as.Date(.data[[date_col]])
    ) %>%
    arrange(date)
  
  # Gráfico con fechas reales y eje X cada 3 meses
  plot_ly(
    df2,
    x = ~date,
    y = ~sum_vars,
    type = 'scatter',
    mode = 'lines+markers',
    line = list(color = 'blue'),
    marker = list(color = 'blue'),
    hovertemplate = paste(
      "Date: %{x|%Y-%m-%d}<br>",
      "Summed Value: %{y:,.2f}<extra></extra>"
    )
  ) %>%
    layout(
      title = "Summed Variables - Linear Flighting",
      xaxis = list(
        title = list(text = "Time", standoff = 35),
        type = "date",
        tickformat = "%Y-%m",
        dtick = "M3",
        tickangle = -70,
        tickfont = list(size = 10),
        automargin = TRUE
      ),
      yaxis = list(
        title = list(text = "Summed Value", standoff = 20),
        automargin = TRUE
      ),
      margin = list(l = 70, r = 40, t = 60, b = 120),
      showlegend = FALSE
    )
  
}
