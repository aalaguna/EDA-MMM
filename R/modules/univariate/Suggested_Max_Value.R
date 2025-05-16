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
  
  # Check for required inputs
  req(input$transformation_univ, df, input$variable_univ)
  validate(
    need(input$variable_univ != "N/A", "Please select a valid variable for the summary.")
  )

  # Check if variable exists in data
  if (!input$variable_univ %in% names(df)) {
    cat("Variable", input$variable_univ, "not found in the data.\n")
    return()
  }

  # Extract data for analysis
  box_data <- df[[input$variable_univ]]
  
  # Handle missing values
  box_data <- box_data[!is.na(box_data)]

  # Check if we have enough data
  if (length(box_data) == 0) {
    cat("Not enough data to calculate suggestions. All values are NA.\n")
    return()
  }
  
  # Handle negative values if needed for certain transformations
  if (input$transformation_univ %in% c("Log", "S Origin", "S Shaped", "Power")) {
    negative_count <- sum(box_data < 0, na.rm = TRUE)
    if (negative_count > 0) {
      cat("Warning: Found", negative_count, "negative values.\n")
      cat("These will be ignored for", input$transformation_univ, "transformation.\n\n")
      box_data <- box_data[box_data >= 0]
      
      # Check again if we have enough data after filtering negatives
      if (length(box_data) == 0) {
        cat("Not enough non-negative data for the suggestions.\n")
        return()
      }
    }
  }

  # Calculate basic statistics
  data_mean <- mean(box_data, na.rm = TRUE)
  data_median <- median(box_data, na.rm = TRUE)
  data_sd <- sd(box_data, na.rm = TRUE)
  box_max <- max(box_data, na.rm = TRUE)
  
  # Header with transformation info
  cat("===================================\n")
  cat("Transformation:", input$transformation_univ, "\n")
  cat("Variable:", input$variable_univ, "\n")
  cat("===================================\n\n")
  
  # Basic statistics
  cat("Basic Statistics:\n")
  cat("- Mean:  ", format(round(data_mean, 2), nsmall = 2), "\n")
  cat("- Median:", format(round(data_median, 2), nsmall = 2), "\n")
  cat("- StdDev:", format(round(data_sd, 2), nsmall = 2), "\n")
  cat("- Max:   ", format(round(box_max, 2), nsmall = 2), "\n\n")

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
  
  # 90th percentile approach
  p90_value <- quantile(box_data, 0.90, na.rm = TRUE)
  maxval_percent_p90 <- (p90_value / box_max) * 100

  # Ensure all percentages are between 1-100 for usability
  maxval_percent_boxplot <- pmin(pmax(maxval_percent_boxplot, 1), 100)
  maxval_percent_three_sd <- pmin(pmax(maxval_percent_three_sd, 1), 100)
  maxval_percent_avg <- pmin(pmax(maxval_percent_avg, 1), 100)
  maxval_percent_p90 <- pmin(pmax(maxval_percent_p90, 1), 100)

  # Suggested MaxVal sections
  cat("Suggested %MaxVal:\n")
  cat("1. Boxplot (no-outlier cap):", format(round(maxval_percent_boxplot, 1), nsmall = 1), "%\n")
  cat("   → Use to reduce impact of outliers\n")
  cat("2. Three Standard Deviations:", format(round(maxval_percent_three_sd, 1), nsmall = 1), "%\n")
  cat("   → For normally distributed data\n")
  cat("3. 90th Percentile:", format(round(maxval_percent_p90, 1), nsmall = 1), "%\n")
  cat("   → A balanced approach for skewed data\n")
  cat("4. Average of Mean and Maximum:", format(round(maxval_percent_avg, 1), nsmall = 1), "%\n")
  cat("   → Simple approach, works in many cases\n\n")

  # Specific recommendations based on transformation type
  if (input$transformation_univ %in% c("S Origin", "S Shaped")) {
    cat("S-Curve Specific Recommendations:\n")
    
    # Calculate coefficient of variation to determine data distribution
    cv <- data_sd / data_mean
    
    # Determine skewness - approximate approach
    skewness <- if (data_mean > data_median) "right-skewed" else "left-skewed or normal"
    
    if (cv > 1) {
      # High variability data
      if (skewness == "right-skewed") {
        # Right-skewed with high variability
        cat("- Data is right-skewed with high variability\n")
        cat("- Recommended %MaxVal: ", format(round(maxval_percent_p90, 1), nsmall = 1), "%\n")
        cat("- Try Alpha: 0.80-0.90, Beta: 1.0-2.0\n")
      } else {
        # Less skewed with high variability
        cat("- Data has high variability\n")
        cat("- Recommended %MaxVal: ", format(round(maxval_percent_three_sd, 1), nsmall = 1), "%\n")
        cat("- Try Alpha: 0.85-0.95, Beta: 0.5-1.5\n")
      }
    } else {
      # Lower variability data
      if (skewness == "right-skewed") {
        # Right-skewed with lower variability
        cat("- Data is moderately skewed\n")
        cat("- Recommended %MaxVal: ", format(round(maxval_percent_avg, 1), nsmall = 1), "%\n")
        cat("- Try Alpha: 0.85-0.90, Beta: 0.5-1.0\n")
      } else {
        # More normal distribution
        cat("- Data is relatively normally distributed\n")
        cat("- Recommended %MaxVal: ", format(round(maxval_percent_three_sd, 1), nsmall = 1), "%\n")
        cat("- Try Alpha: 0.90-0.95, Beta: 0.5-1.0\n")
      }
    }
  } else if (input$transformation_univ == "Power") {
    # Calculate quartile ratio to determine shape of distribution
    q_ratio <- q3 / q1
    
    cat("Power Transformation Recommendations:\n")
    if (q_ratio > 3) {
      cat("- Data is highly skewed (Q3/Q1 =", round(q_ratio, 2), ")\n")
      cat("- Try Alpha values < 1 (e.g., 0.3-0.5) for right-skewed data\n")
      cat("- Try Alpha values > 1 (e.g., 1.5-2.0) for left-skewed data\n")
    } else {
      cat("- Data is moderately skewed (Q3/Q1 =", round(q_ratio, 2), ")\n")
      cat("- Try Alpha values around 0.5-0.7 for right-skewed data\n")
      cat("- Try Alpha values around 1.2-1.5 for left-skewed data\n")
    }
  } else if (input$transformation_univ == "Log") {
    cat("Logarithmic Transformation Notes:\n")
    # Suggestions for log transformation
    if (data_mean / data_median > 1.5) {
      cat("- Data is highly right-skewed (Mean/Median =", round(data_mean / data_median, 2), ")\n")
      cat("- Logarithmic transformation is well-suited for this distribution\n")
    } else {
      cat("- Data has moderate skew (Mean/Median =", round(data_mean / data_median, 2), ")\n")
      cat("- Consider if logarithmic transformation is necessary\n")
    }
  } else if (input$transformation_univ == "Exp") {
    cat("Exponential Transformation Notes:\n")
    # Suggestions for exponential transformation
    if (data_median / data_mean > 1.2) {
      cat("- Data is left-skewed (Median/Mean =", round(data_median / data_mean, 2), ")\n")
      cat("- Exponential transformation can amplify this skew\n")
      cat("- Consider using a lower decay value for stabilization\n")
    } else {
      cat("- Data is suitable for exponential transformation\n")
      cat("- Watch for extreme values in the result\n")
    }
  }
}