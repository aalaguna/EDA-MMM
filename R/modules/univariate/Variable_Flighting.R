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
  
  # map KPI range to Variable range
  min_var <- min(data_to_plot_full$Variable, na.rm = TRUE)
  max_var <- max(data_to_plot_full$Variable, na.rm = TRUE)
  min_kpi <- min(data_to_plot_full$KPI, na.rm = TRUE)
  max_kpi <- max(data_to_plot_full$KPI, na.rm = TRUE)
  
  scale_factor <- if ((max_kpi - min_kpi) != 0) {
    (max_var - min_var) / (max_kpi - min_kpi)
  } else {
    1
  }
  if (is.infinite(scale_factor) || is.na(scale_factor) || scale_factor == 0) {
    scale_factor <- 1
  }
  
  # convert data to long format
  data_long <- data_to_plot %>% 
    mutate(KPI_scaled = (KPI - min_kpi) * scale_factor + min_var) %>% 
    pivot_longer(
      cols = c('Variable', 'KPI_scaled'),
      names_to = 'metric_type',
      values_to = 'plot_value'
    ) %>% 
    mutate(
      metric_name_display = ifelse(metric_type == 'Variable', 'Variable', 'KPI'),
      original_value = ifelse(
        metric_type == 'Variable',
        plot_value,
        data_to_plot$KPI[match(date, data_to_plot$date)]
      ),
      hover_text = paste0('Date: ', format(date, '%Y-%m-%d'), '<br>',
                          metric_name_display, ': ', sprintf('%.2f', original_value))
    )
  
  # GRAPH
  p_static <- ggplot2::ggplot(
    data = data_long,
    aes(
      x = date,
      y = plot_value,
      color = metric_name_display,
      group = metric_name_display,
      text = hover_text
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 1) +
    scale_color_manual(
      name = NULL,
      values = c('Variable' = 'red', 'KPI' = 'blue')
    ) +
    labs(
      title = paste("KPI vs. Variable (Geography:", geography_univ, ")"),
         x = 'Time',
         y = "Variable"
      ) +
    scale_x_date(
      date_labels = '%Y-%m',
      date_breaks = '3 months'
    ) +
    scale_y_continuous(
      name = 'Variable',
      sec.axis = sec_axis(
        trans = ~ (. - min_var) / scale_factor + min_kpi,
        name = 'KPI',
        labels = scales::number_format(accuracy = 0.01)
      )
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 0),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y.left = element_text(margin = margin(r = 10), color = 'red'),
      axis.title.y.right = element_text(margin = margin(l = 10), color = 'blue'),
      legend.position = c(1.05, 1),
      legend.justification = c('left', 'top'),
      legend.background = element_rect(fill = 'white', color = 'grey90'),
      plot.margin = margin(t = 20, r = 100, b = 30, l = 30)
    )
  
  # convert to ggplotly
  p <- ggplotly(p_static, tooltip = 'text') %>%
    layout(hovermode = 'x unified')
  
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