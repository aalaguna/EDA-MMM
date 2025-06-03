# =============================================================================
# Renders a flighting chart comparing KPI and the selected variable.
# =============================================================================

render_variable_flighting <- function(df, kpi_univ, variable_univ, geography_univ) {
  # Generates a chart comparing KPI and selected variable
  #
  # Args:
  #   df: Dataframe with data
  #   kpi_univ: Name of the KPI variable
  #   variable_univ: Name of the variable to compare
  #   geography_univ: Selected geography (for title)
  #
  # Returns:
  #   ggplot object with comparison chart
  
  req(df, kpi_univ, variable_univ)
  validate(
    need(variable_univ != "N/A", "Please select a valid variable for analysis.")
  )

  date_col <- if ("Period" %in% names(df)) "Period" else "periodo"
  req(date_col)
  
  if (!kpi_univ %in% names(df) || !variable_univ %in% names(df)) {
    return(plotly::plot_ly() %>% 
           plotly::layout(title = "KPI or Variable column not found in data",
                         xaxis = list(showticklabels = FALSE),
                         yaxis = list(showticklabels = FALSE)))
  }

  data_to_plot <- df %>%
    select(!!sym(date_col), KPI = !!sym(kpi_univ), Variable = !!sym(variable_univ))

  validate(
    need(nrow(data_to_plot) > 0, "No data available for plotting.")
  )
  
  # 
  data_to_plot <- data_to_plot %>%
    mutate(date = as.Date(get(date_col)))
  
  # NoNA
  data_to_plot <- data_to_plot %>%
    filter(!is.na(date))
  

  if (nrow(data_to_plot) == 0) {
    return(plotly::plot_ly() %>% 
           plotly::layout(title = "No valid data after filtering",
                         xaxis = list(showticklabels = FALSE),
                         yaxis = list(showticklabels = FALSE)))
  }
  
  # Label trimestrales
  data_to_plot <- data_to_plot %>%
    mutate(
      year = format(date, "%Y"),
      month = as.integer(format(date, "%m")),
      quarter = ceiling(month / 3),
      month_label = case_when(
        quarter == 1 ~ "01",
        quarter == 2 ~ "04",
        quarter == 3 ~ "07",
        quarter == 4 ~ "10",
        TRUE ~ "01" 
      ),
      label = paste0(year, "-", month_label)
    )
  
  # data time sort
  data_to_plot <- data_to_plot %>% arrange(date)
  
  # GRAPH
  
  # rescale ranges
  max_var <- max(data_to_plot$Variable, na.rm = T)
  max_kpi <- max(data_to_plot$KPI, na.rm = T)

  scale_factor <- max_var / max_kpi
  
  # data_to_plot$date <- as.Date(data_to_plot$date)
  
  p_static <- ggplot2::ggplot(
    data = data_to_plot,
    aes(x = date)) +
    geom_line(aes(y = Variable, group = 1), 
              color = 'red', linewidth = 1, name = 'Variable') +
    geom_line(aes(y = KPI * scale_factor, group = 2),
              color = 'blue', linewidth = 1, name = 'KPI') +
    scale_x_date(date_labels = '%Y-%m', date_breaks = '3 months') +
    labs(
      title = paste("KPI vs. Variable (Geography:", geography_univ, ")"),
      x = 'Time'
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_text(margin = margin(t = 25)),
      legend.position = 'right'
    )
  
  # convert to ggplotly
  p <- ggplotly(p_static)
  
  # Modify hover texts
  p$x$data[[1]]$hovertemplate <- paste0("Variable: %{customdata[0]}<br>Date: %{x|%Y-%m-%d}<extra></extra>")
  p$x$data[[1]]$customdata <- lapply(data_to_plot$Variable, function(x) list(x))
  
  p$x$data[[2]]$hovertemplate <- paste0("KPI: %{customdata[0]}<br>Date: %{x|%Y-%m-%d}<extra></extra>")
  p$x$data[[2]]$customdata <- lapply(data_to_plot$KPI, function(x) list(x))
  
  # p <- plotly::plot_ly(data_to_plot, x = ~date) %>%
  #   plotly::add_trace(
  #     y = ~Variable, 
  #     name = "Variable", 
  #     type = "scatter",
  #     mode = "lines+markers",
  #     line = list(color = "red", width = 2), 
  #     marker = list(color = "red", size = 6),
  #     yaxis = "y",
  #     hovertemplate = paste(
  #       "Date: %{x|%Y-%m-%d}<br>",
  #       "Variable: %{y:.2f}<extra></extra>"
  #     )
  #   ) %>%
  #   plotly::add_trace(
  #     y = ~KPI, 
  #     name = "KPI", 
  #     type = "scatter",
  #     mode = "lines+markers",
  #     line = list(color = "blue", width = 2), 
  #     marker = list(color = "blue", size = 6),
  #     yaxis = "y2",
  #     hovertemplate = paste(
  #       "Date: %{x|%Y-%m-%d}<br>",
  #       "KPI: %{y:.2f}<extra></extra>"
  #     )
  #   ) %>%
  #   plotly::layout(
  #     title = list(
  #       text = paste("KPI vs. Variable (Geography:", geography_univ, ")"),
  #       font = list(size = 16)
  #     ),
  #     xaxis = list(
  #       title = list(text = "Time", standoff = 25),
  #       type = "date",
  #       tickformat = "%Y-%m",
  #       dtick = "M3",
  #       tickangle = -45,
  #       automargin = TRUE
  #     ),
  #     yaxis = list(
  #       title = list(text = "Variable", standoff = 20),
  #       side = "left",
  #       automargin = TRUE
  #     ),
  #     yaxis2 = list(
  #       title = list(text = "KPI", standoff = 20),
  #       overlaying = "y", 
  #       side = "right",
  #       automargin = TRUE
  #     ),
  #     legend = list(
  #       orientation = "v",
  #       xanchor = "left",
  #       x = 1.02,
  #       y = 1,
  #       bgcolor = "rgba(255,255,255,0.95)"
  #     ),
  #     margin = list(l = 70, r = 120, t = 60, b = 80),
  #     hovermode = "x unified"
  #   )
  
  return(p)
}