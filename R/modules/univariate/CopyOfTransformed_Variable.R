# Transformed_Variable.R

# =============================================================================
# Renders the chart of the transformed variable over time.
# =============================================================================

render_transformation_chart <- function(df, variable_univ, transformation_univ,
                                        lag_univ, decay_univ,
                                        alpha_univ, beta_univ, maxval_univ,
                                        geography_univ) {
  # Generates a chart of the transformed variable
  #
  # Args:
  #   df: Dataframe with data
  #   variable_univ: Name of the variable to transform
  #   transformation_univ, lag_univ, decay_univ, alpha_univ, beta_univ, maxval_univ:
  #     Transformation parameters
  #   geography_univ: Selected geography (for title)
  #
  # Returns:
  #   plotly object with transformed variable chart
  
  req(df, variable_univ, transformation_univ)
  validate(
    need(variable_univ != "N/A", "Please select a valid variable for transformation.")
  )

  var_name <- variable_univ
  data_vec <- df[[var_name]]
  validate(
    need(length(data_vec) > 0, "No data available for transformation.")
  )

  data_trans <- as.numeric(data_vec)
  if (lag_univ > 0) {
    if (lag_univ >= length(data_trans)) {
      data_trans <- rep(NA, length(data_trans))
    } else {
      data_trans <- c(rep(NA, lag_univ), head(data_trans, -lag_univ))
    }
  }
  data_trans <- data_trans * decay_univ

  transformed_data <- tryCatch({
    switch(transformation_univ,
           "Linear"   = data_trans,
           "S Origin" = s_curve_transform(data_trans, "s-origin", alpha_univ, beta_univ, maxval_univ),
           "S Shaped" = s_curve_transform(data_trans, "s-shaped", alpha_univ, beta_univ, maxval_univ),
           "Index Exp"= 1 - exp(-(alpha_univ / 10) * data_trans),
           "Log"      = log1p(data_trans),
           "Exp"      = exp(data_trans),
           "Power"    = data_trans^alpha_univ,
           "Moving Avg" = {
             if (length(data_trans) < 3) rep(NA, length(data_trans))
             else zoo::rollmean(data_trans, k = 3, fill = NA, align = "right")
           },
           data_trans
    )
  }, error = function(e) {
    notifyUser(paste("Transformation error:", e$message), "error")
    data_trans
  })

  if (length(transformed_data) != nrow(df)) {
    notifyUser("Error: discrepancy between transformed data length and # rows.", "error")
    return(NULL)
  }

  date_col <- if ("Period" %in% names(df)) "Period" else "periodo"
  req(date_col)

  # # Create dataframe with transformed data
  # df_trans <- df %>%
  #   mutate(Transformed = transformed_data)
  # 
  # # Apply quarterly logic as in sum_variables_chart
  # df_trans <- df_trans %>%
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
  #   summarise(Transformed = mean(Transformed, na.rm = TRUE), .groups = "drop")
  # 
  # # Use plotly directly instead of converting from ggplot
  # p <- plot_ly(
  #   df_trans, 
  #   x = ~label, 
  #   y = ~Transformed, 
  #   type = 'scatter', 
  #   mode = 'lines+markers', 
  #   line = list(color = 'red')
  # ) %>%
  #   layout(
  #     title = paste("Transformed Variable (Geography:", geography_univ, ")"),
  #     xaxis = list(
  #       title = list(
  #         text = "Time",
  #         standoff = 25  # Increase space between title and axis
  #       ),
  #       type = "category", 
  #       tickangle = -45,
  #       automargin = TRUE
  #     ),
  #     yaxis = list(
  #       title = list(
  #         text = "Transformed Value",
  #         standoff = 20  # Increase space between title and axis
  #       ),
  #       automargin = TRUE
  #     ),
  #     margin = list(l = 70, r = 40, t = 60, b = 80),  # Increased margins
  #     showlegend = FALSE
  #   )
  # 
  # return(p)
  # Create dataframe with transformed data
  df_trans <- df %>%
    mutate(
      date = as.Date(.data[[date_col]]),
      Transformed = transformed_data
    ) %>%
    arrange(date)
  
  # Use plotly directly with real date and quarterly labels
  p <- plot_ly(
    df_trans, 
    x = ~date, 
    y = ~Transformed, 
    type = 'scatter', 
    mode = 'lines+markers', 
    line = list(color = 'red'),
    hovertemplate = paste(
      "Date: %{x|%Y-%m-%d}<br>",
      "Transformed: %{y:,.2f}<extra></extra>"
    )
  ) %>%
    layout(
      title = paste("Transformed Variable (Geography:", geography_univ, ")"),
      xaxis = list(
        title = list(text = "Time", standoff = 25),
        type = "date",
        tickformat = "%Y-%m",
        dtick = "M3",
        tickangle = -45,
        automargin = TRUE
      ),
      yaxis = list(
        title = list(text = "Transformed Value", standoff = 20),
        automargin = TRUE
      ),
      margin = list(l = 70, r = 40, t = 60, b = 80),
      showlegend = FALSE
    )
  
  return(p)
  
}