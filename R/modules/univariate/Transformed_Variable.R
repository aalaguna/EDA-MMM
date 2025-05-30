# Transformed_Variable.R

# =============================================================================
# Renders the chart of the transformed variable over time.
# =============================================================================



render_transformation_chart <- function(df, kpi_univ, variable_univ, transformation_univ,
                                        lag_univ, decay_univ,
                                        alpha_univ, beta_univ, maxval_univ,
                                        geography_univ) {

  req(df, kpi_univ, variable_univ, transformation_univ)
  validate(
    need(variable_univ != "N/A", "Please select a valid variable for transformation."),
    need(kpi_univ != "N/A", "Please select a valid KPI.")
  )

  date_col <- if ("Period" %in% names(df)) "Period" else "periodo"
  req(date_col)

  # --- KPI Vector ---
  kpi_vec <- as.numeric(df[[kpi_univ]])

  # --- Transform Variable ---
  data_vec <- as.numeric(df[[variable_univ]])

  data_trans <- data_vec

  # Apply Lag
  if (lag_univ > 0) {
    if (lag_univ >= length(data_trans)) {
      data_trans <- rep(NA, length(data_trans))
    } else {
      data_trans <- c(rep(NA, lag_univ), head(data_trans, -lag_univ))
    }
  }

  # # Apply Decay
  # data_trans <- data_trans * decay_univ


  # Apply Decay (carry-over effect)
  if (!is.na(decay_univ) && decay_univ < 1 && decay_univ > 0) {
    for (i in 2:length(data_trans)) {
      if (!is.na(data_trans[i - 1])) {
        data_trans[i] <- data_trans[i] + decay_univ * data_trans[i - 1]
      }
    }
  }


  # Apply Transformation
  transformed_data <- tryCatch({
    switch(transformation_univ,
           "Linear" = data_trans,
           "S Origin" = {
             if (max(data_trans, na.rm = TRUE) != 0) {
               beta_scaled <- beta_univ / (10^9)
               scaled_x <- 100 * data_trans / (max(data_trans, na.rm = TRUE) * maxval_univ)
               ((beta_scaled)^((alpha_univ^scaled_x))) - beta_scaled
             } else data_trans
           },
           "S Shaped" = {
             if (max(data_trans, na.rm = TRUE) != 0) {
               beta_scaled <- beta_univ / (10^10)
               scaled_x <- 100 * data_trans / (max(data_trans, na.rm = TRUE) * maxval_univ)
               (beta_scaled)^(alpha_univ^scaled_x)
             } else data_trans
           },
           "Index Exp" = {
             if (max(data_trans, na.rm = TRUE) != 0) {
               1 - exp((-alpha_univ * (100 * data_trans / (max(data_trans, na.rm = TRUE) * maxval_univ))) / 10)
             } else {
               data_trans
             }
           },
           "Log" = log1p(data_trans),
           "Exp" = exp(data_trans),
           "Power" = data_trans^alpha_univ,
           "Moving Avg" = if (length(data_trans) < 3) rep(NA, length(data_trans)) else zoo::rollmean(data_trans, k = 3, fill = NA, align = "right"),
           data_trans
    )
  }, error = function(e) {
    notifyUser(paste("Transformation error:", e$message), "error")
    data_trans
  })

  # --- date ---

  df_plot <- df %>%
    mutate(
      date = as.Date(.data[[date_col]]),
      KPI = kpi_vec,
      Variable_Transformed = transformed_data,
      year = format(as.Date(.data[[date_col]]), "%Y"),
      month = as.integer(format(as.Date(.data[[date_col]]), "%m")),
      quarter = ceiling(month / 3),
      month_label = case_when(
        quarter == 1 ~ "01",
        quarter == 2 ~ "04",
        quarter == 3 ~ "07",
        quarter == 4 ~ "10",
        TRUE ~ "01"
      ),
      label = paste0(year, "-", month_label)
    ) %>%
    arrange(date)



  # --- Plot ---
  
  # rescale ranges
  max_var <- max(df_plot$Variable_Transformed, na.rm = T)
  max_kpi <- max(df_plot$KPI, na.rm = T)
  
  scale_factor <- max_var / max_kpi
  
  p_static <- ggplot2::ggplot(
    data = df_plot,
    aes(x = date)) +
    geom_line(aes(y = Variable_Transformed), 
              color = 'red', linewidth = 1) +
    geom_line(aes(y = KPI * scale_factor), 
              color = 'blue', linewidth = 1) +
    scale_x_date(date_labels = '%Y-%m', date_breaks = '3 months') +
    labs(
      title = paste("Transformed Variable vs KPI (Geography:", geography_univ, ")"),
      x = 'Time'
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_text(margin = margin(t = 25)),
      legend.position = 'right'
    )
  
  p <- ggplotly(p_static)
  
  return(p)
  
  # plot_ly(df_plot, x = ~date) %>%
  #   add_trace(
  #     y = ~Variable_Transformed,
  #     name = "Variable (Trans)",
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
  #   add_trace(
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
  #   layout(
  #     title = list(text = paste("Transformed Variable vs KPI (Geography:", geography_univ, ")")),
  #     xaxis = list(
  #       title = list(text = "Time", standoff = 25),
  #       type = "date",
  #       tickformat = "%Y-%m",
  #       dtick = "M3",
  #       tickangle = -45,
  #       automargin = TRUE
  #     ),
  #     yaxis = list(
  #       title = list(text = "Transformed Variable", standoff = 20),
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
  #       y = 1
  #     ),
  #     margin = list(l = 70, r = 120, t = 60, b = 80),
  #     hovermode = "x unified"
  #   )

}