# Suggested_Max_Value_Multivariate.R

# =============================================================================
# Displays some suggested maximum value percentages for s-curves or 
# general transformations in multivariate mode.
# =============================================================================

render_transformations_summary_multi <- function(df, input) {
  #' Generates suggestions for transformation parameters in multivariate mode.
  #'
  #' Args:
  #'   df: Dataframe containing the data.
  #'   input: Shiny input object with variable selections.
  #'
  #' Returns:
  #'   Text with parameter suggestions.
  
  req(df)
  
  # Check if we are in sum or individual mode
  if (input$sum_all_vars == "true") {
    # Sum mode - we need to calculate the sum first
    vars_selected <- c(input$var1_multi, input$var2_multi, input$var3_multi)
    if (!is.null(input$var4_multi) && input$var4_multi != "None" && input$var4_multi != "") {
      vars_selected <- c(vars_selected, input$var4_multi)
    }
    vars_selected <- vars_selected[vars_selected != "None"]
    vars_selected <- intersect(vars_selected, names(df))  # Ensure selected vars exist in df
    
    if (length(vars_selected) < 1) {
      cat("No variables selected for calculating suggestions.\n")
      return()
    }
    
    # Calculate the sum of the selected variables
    df$sum_vars <- rowSums(df[, vars_selected, drop = FALSE], na.rm = TRUE)
    box_data <- df$sum_vars
  } else {
    # Individual mode - use the first variable
    if (is.null(input$var1_multi) || input$var1_multi == "None") {
      cat("Please select a valid variable for the summary.\n")
      return()
    }
    
    box_data <- df[[input$var1_multi]]
  }
  
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
  
  cat("%MaxVal Suggestions:\n")
  cat("Boxplot-based (cap non-outlier):", round(maxval_percent_boxplot, 2), "%\n")
  cat("Three Standard Deviations:",          round(maxval_percent_three_sd, 2), "%\n")
  cat("Average of Mean and Maximum:",       round(maxval_percent_avg, 2), "%\n")
  
}