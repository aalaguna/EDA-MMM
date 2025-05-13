# Transformed_Summed_Variables.R


# Function that renders a chart for transformed summed variables
render_transformed_variables_chart <- function(df) {
  req(df)
  if (!"trans_sum_vars" %in% names(df)) {
    return(NULL)
  }
  
  date_col <- if ("Period" %in% names(df)) "Period" else "periodo"
  req(date_col)
  
  # # Summarize by quarter, labeling with the initial month of the quarter
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
  #   summarise(trans_sum_vars = sum(trans_sum_vars, na.rm = TRUE), .groups = "drop")
  # 
  # plot_ly(
  #   df2, 
  #   x = ~label, 
  #   y = ~trans_sum_vars,
  #   type = 'scatter', 
  #   mode = 'lines+markers',
  #   line = list(color = 'red'),
  #   marker = list(color = 'red')
  # ) %>%
  #   layout(
  #     title = "Transformed Summed Variables",
  #     xaxis = list(
  #       title = list(
  #         text = "Time",
  #         standoff = 35  # Increase distance between title and axis values
  #       ),
  #       type = "category", 
  #       tickangle = -70,  # Steeper angle to prevent overlapping
  #       tickfont = list(size = 10),  # Smaller font for tick labels
  #       automargin = TRUE,  # Auto-adjust margins to prevent overlap
  #       nticks = max(8, floor(length(unique(df2$label))/2))  # Show fewer tick labels
  #     ),
  #     yaxis = list(
  #       title = list(
  #         text = "Transformed Sum",
  #         standoff = 20  # Increase distance between title and axis values
  #       ),
  #       automargin = TRUE  # Auto-adjust margins to prevent overlap
  #     ),
  #     margin = list(l = 70, r = 40, t = 60, b = 120),  # Increased bottom margin
  #     showlegend = FALSE
  #   )
  # Preparar los datos con fecha real
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