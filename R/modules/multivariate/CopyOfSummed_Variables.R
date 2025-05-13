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
  
  # # Summarize by quarter, labeling with the initial month (01, 04, 07, 10)
  # df2 <- df %>%
  #   mutate(
  #     year = format(as.Date(.data[[date_col]]), "%Y"),
  #     quarter = lubridate::quarter(as.Date(.data[[date_col]])),
  #     month_label = case_when(
  #       quarter == 1 ~ "01",
  #       quarter == 2 ~ "04",
  #       quarter == 3 ~ "07",
  #       quarter == 4 ~ "10"
  #     ),
  #     label = paste0(year, "-", month_label)
  #   ) %>%
  #   group_by(label) %>%
  #   summarise(sum_vars = sum(sum_vars, na.rm = TRUE), .groups = "drop")
  # 
  # plot_ly(
  #   df2,
  #   x = ~label,
  #   y = ~sum_vars,
  #   type = 'scatter',
  #   mode = 'lines+markers',
  #   line = list(color = 'blue'),
  #   marker = list(color = 'blue')
  # ) %>%
  #   layout(
  #     title = "Summed Variables - Linear Flighting",
  #     xaxis = list(
  #       title = list(
  #         text = "Time",
  #         standoff = 35  # Increase distance between title and axis values
  #       ),
  #       type = "category",
  #       tickangle = -70,  # Steeper angle to prevent overlapping
  #       tickfont = list(size = 10),  # Smaller font for tick labels
  #       automargin = TRUE,  # Auto-adjust margins to prevent overlap
  #       nticks = max(8, floor(length(unique(df2$label)) / 2))  # Show fewer tick labels
  #     ),
  #     yaxis = list(
  #       title = list(
  #         text = "Summed Value",
  #         standoff = 20  # Increase distance between title and axis values
  #       ),
  #       automargin = TRUE  # Auto-adjust margins to prevent overlap
  #     ),
  #     margin = list(l = 70, r = 40, t = 60, b = 120),  # Increased bottom margin
  #     showlegend = FALSE
  #   )
  # Preparar los datos con la fecha real
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
