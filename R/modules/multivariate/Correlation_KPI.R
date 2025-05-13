# =============================================================================
# Function that renders a correlation plot between the KPI and the sum of variables
# =============================================================================

render_kpi_correlation <- function(df, kpi) {
  # Ensure that the dataframe and KPI are valid
  req(df, kpi)
  validate(
    need(kpi %in% names(df), "KPI not found in the dataset.")
  )
  
  # Attempt to use 'trans_sum_vars'; if not available, fallback to 'sum_vars'
  sum_col <- "trans_sum_vars"
  if (!sum_col %in% names(df)) {
    if ("sum_vars" %in% names(df)) {
      sum_col <- "sum_vars"
    } else {
      return(plotly::plot_ly() %>% 
             plotly::layout(title = "Sum/transformed column not found.",
                           xaxis = list(showticklabels = FALSE),
                           yaxis = list(showticklabels = FALSE)))
    }
  }
  
  kpi_data <- df[[kpi]]
  sum_data <- df[[sum_col]]
  
  # Check for complete cases
  valid_idx <- complete.cases(kpi_data, sum_data)
  if (sum(valid_idx) < 3) {
    return(plotly::plot_ly() %>% 
           plotly::layout(title = "Not enough data for correlation (min 3 points required).",
                         xaxis = list(showticklabels = FALSE),
                         yaxis = list(showticklabels = FALSE)))
  }
  
  # Compute correlation
  corr_val <- tryCatch({
    cor(kpi_data[valid_idx], sum_data[valid_idx], use = "complete.obs")
  }, error = function(e) {
    return(NA)
  })
  
  # Check if correlation is valid
  if (is.na(corr_val)) {
    return(plotly::plot_ly() %>% 
           plotly::layout(title = "Could not compute correlation (possibly constant values).",
                         xaxis = list(showticklabels = FALSE),
                         yaxis = list(showticklabels = FALSE)))
  }
  
  # Create scatter plot data
  plot_data <- data.frame(
    KPI = kpi_data[valid_idx],
    Sum = sum_data[valid_idx]
  )
  
  # Use direct plotly instead of ggplot for better control - with explicit type/mode
  p <- plotly::plot_ly(
    data = plot_data,
    x = ~KPI, 
    y = ~Sum, 
    type = 'scatter', 
    mode = 'markers',
    marker = list(
      color = 'darkgreen', 
      size = 10,
      opacity = 0.7,
      line = list(color = 'darkgreen', width = 1)
    ),
    hovertemplate = paste(
      "KPI: %{x:.2f}<br>",
      "Sum: %{y:.2f}<extra></extra>"
    )
  ) %>%
    plotly::layout(
      title = list(
        text = paste("Correlation:", round(corr_val, 2))
      ),
      xaxis = list(
        title = list(
          text = kpi,
          standoff = 25
        ),
        zeroline = TRUE,
        zerolinecolor = '#969696',
        zerolinewidth = 1,
        gridcolor = '#ddd',
        automargin = TRUE
      ),
      yaxis = list(
        title = list(
          text = "Sum / Transformed Sum",
          standoff = 25
        ),
        zeroline = TRUE,
        zerolinecolor = '#969696',
        zerolinewidth = 1,
        gridcolor = '#ddd',
        automargin = TRUE
      ),
      margin = list(l = 70, r = 30, t = 60, b = 60),
      showlegend = FALSE
    )
  
  # Add a fitted line if we have enough points
  if (sum(valid_idx) >= 5) {
    # Try to fit a linear model
    fit <- tryCatch({
      lm(Sum ~ KPI, data = plot_data)
    }, error = function(e) {
      return(NULL)
    })
    
    # Only add line if fit was successful
    if (!is.null(fit)) {
      # Generate predicted values
      x_range <- seq(min(plot_data$KPI), max(plot_data$KPI), length.out = 100)
      predicted <- predict(fit, newdata = data.frame(KPI = x_range))
      
      # Add fitted line with explicit type/mode
      p <- p %>% plotly::add_trace(
        x = x_range,
        y = predicted,
        type = 'scatter',
        mode = 'lines',
        line = list(
          color = 'red',
          width = 2,
          dash = 'solid'
        ),
        hoverinfo = 'none',
        showlegend = FALSE
      )
    }
  }
  
  return(p)
}