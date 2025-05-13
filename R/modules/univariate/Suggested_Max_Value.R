# Suggested_Max_Value_Univariate.R

# =============================================================================
# Displays some suggested maximum value percentages for s-curves or 
# general transformations.
# =============================================================================

render_transformations_summary <- function(df, input) {
  # Generates suggestions for transformation parameters
  #
  # Args:
  #   df: Dataframe with data
  #   input: Shiny input object with variable selection
  #
  # Returns:
  #   Text with parameter suggestions
  
  req(input$transformation_univ, df, input$variable_univ)
  validate(
    need(input$variable_univ != "N/A", "Please select a valid variable for the summary.")
  )

  box_data <- df[[input$variable_univ]]
  box_data <- box_data[!is.na(box_data)]

  if (length(box_data) == 0) {
    cat("Not enough data to calculate suggestions.\n")
    return()
  }

  data_mean <- mean(box_data, na.rm = TRUE)
  data_sd   <- sd(box_data,   na.rm = TRUE)
  box_max   <- max(box_data,  na.rm = TRUE)

  # Boxplot outlier-based approach
  q1 <- quantile(box_data, 0.25, na.rm = TRUE)
  q3 <- quantile(box_data, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  upper_limit <- q3 + 1.5 * iqr
  max_not_outlier <- ifelse(box_max > upper_limit, upper_limit, box_max)
  maxval_percent_boxplot <- (max_not_outlier / box_max) * 100

  # 3 standard deviations approach
  three_sd_value <- data_mean + 3 * data_sd
  maxval_percent_three_sd <- (three_sd_value / box_max) * 100

  # Average between mean and max
  avg_mean_max <- (data_mean + box_max) / 2
  maxval_percent_avg <- (avg_mean_max / box_max) * 100

  cat("Suggested %MaxVal:\n")
  cat("Based on boxplot (no-outlier cap):", round(maxval_percent_boxplot, 2), "%\n")
  cat("Three Standard Deviations:",       round(maxval_percent_three_sd, 2), "%\n")
  cat("Average of Mean and Maximum:",     round(maxval_percent_avg, 2), "%\n")
}