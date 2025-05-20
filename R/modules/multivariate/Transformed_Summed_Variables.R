# Transformed_Summed_Variables.R


# Function that renders a chart for transformed summed variables
render_transformed_variables_chart <- function(df) {
  req(df)
  if (!"trans_sum_vars" %in% names(df)) {
    return(NULL)
  }
  
  date_col <- if ("Period" %in% names(df)) "Period" else "periodo"
  req(date_col)

  df2 <- df %>%
    mutate(
      date = as.Date(.data[[date_col]])
    ) %>%
    arrange(date)
  
  # Crear gráfico con fechas reales y eje limpio
  plot_ly(
    df2, 
    x = ~date, 
    y = ~trans_sum_vars,
    type = 'scatter', 
    mode = 'lines+markers',
    line = list(color = 'red'),
    marker = list(color = 'red'),
    hovertemplate = paste(
      "Date: %{x|%Y-%m-%d}<br>",
      "Transformed Sum: %{y:,.2f}<extra></extra>"
    )
  ) %>%
    layout(
      title = "Transformed Summed Variables",
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
        title = list(text = "Transformed Sum", standoff = 20),
        automargin = TRUE
      ),
      margin = list(l = 70, r = 40, t = 60, b = 120),
      showlegend = FALSE
    )
  
}