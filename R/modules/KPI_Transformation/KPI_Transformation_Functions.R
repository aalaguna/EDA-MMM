# =============================================================================
# Helper functions for KPI transformation and normalization
# =============================================================================

# --------------------------
# Transformation Functions
# --------------------------

# Apply logarithmic transformation to KPI values
apply_log_transformation <- function(data, kpi_column) {
  # Check if the KPI column exists in the data
  if (!kpi_column %in% names(data)) {
    notifyUser(paste("KPI column", kpi_column, "not found in the data."), "error")
    return(data)
  }
  
  # Create a copy of the data to avoid modifying the original
  transformed_data <- data
  
  # Extract the KPI values
  kpi_values <- transformed_data[[kpi_column]]
  
  # Apply log transformation only to positive values
  # For zero or negative values, replace with NA
  transformed_values <- rep(NA_real_, length(kpi_values))
  positive_idx <- which(kpi_values > 0)
  
  if (length(positive_idx) > 0) {
    transformed_values[positive_idx] <- log(kpi_values[positive_idx])
  }
  
  # Add the transformed values to the data
  transformed_column <- paste0(kpi_column, "_log")
  transformed_data[[transformed_column]] <- transformed_values
  
  return(transformed_data)
}

# Apply moving average transformation to KPI values
apply_moving_average <- function(data, kpi_column, window_size = 3) {
  # Check if the KPI column exists in the data
  if (!kpi_column %in% names(data)) {
    notifyUser(paste("KPI column", kpi_column, "not found in the data."), "error")
    return(data)
  }
  
  # Ensure window_size is a positive integer
  window_size <- max(1, as.integer(window_size))
  
  # Create a copy of the data to avoid modifying the original
  transformed_data <- data
  
  # Apply moving average using zoo::rollmean
  # Align "right" means the window is the current point and (window_size-1) previous points
  # Fill NA for points where a full window is not available
  tryCatch({
    # First, ensure data is ordered by date
    date_col <- if ("Period" %in% names(data)) "Period" else if ("periodo" %in% names(data)) "periodo" else NULL
    
    if (!is.null(date_col) && date_col %in% names(data)) {
      # Convert to Date if it's not already
      if (!inherits(data[[date_col]], "Date")) {
        data[[date_col]] <- as.Date(data[[date_col]])
      }
      
      # Sort by date
      idx <- order(data[[date_col]])
      sorted_values <- data[[kpi_column]][idx]
      
      # Apply moving average
      ma_values <- rep(NA_real_, length(sorted_values))
      
      # Only apply if we have enough data points
      if (length(sorted_values) >= window_size) {
        ma_result <- zoo::rollmean(sorted_values, k = window_size, fill = NA, align = "right")
        ma_values[idx] <- ma_result
      }
      
      transformed_data[[paste0(kpi_column, "_ma", window_size)]] <- ma_values
    } else {
      # If no date column, just apply rolling mean without sorting
      transformed_data[[paste0(kpi_column, "_ma", window_size)]] <- 
        zoo::rollmean(transformed_data[[kpi_column]], k = window_size, fill = NA, align = "right")
    }
  }, error = function(e) {
    warning(paste("Error applying moving average:", e$message))
    # In case of error, create the column but fill with NA
    transformed_data[[paste0(kpi_column, "_ma", window_size)]] <- NA_real_
  })
  
  return(transformed_data)
}

# --------------------------
# Normalization Functions
# --------------------------

# Normalize KPI by dividing by the mean (Scale to mean=1 range)
normalize_by_division <- function(data, kpi_column, transformed_column = NULL) {
  # If transformed_column is provided, use it as the source column
  # Otherwise use the original kpi_column
  source_column <- if (!is.null(transformed_column) && transformed_column %in% names(data)) {
    transformed_column
  } else {
    kpi_column
  }
  
  # Check if the source column exists in the data
  if (!source_column %in% names(data)) {
    notifyUser(paste("Source column", source_column, "not found in the data."), "error")
    return(data)
  }
  
  # Create a copy of the data to avoid modifying the original
  normalized_data <- data
  
  # Calculate the mean of the source column (ignoring NA values)
  kpi_mean <- mean(normalized_data[[source_column]], na.rm = TRUE)
  
  if (is.na(kpi_mean) || kpi_mean == 0) {
    notifyUser("Unable to normalize: mean is zero or NA.", "warning")
    normalized_data[[paste0(source_column, "_norm_div")]] <- NA_real_
    return(normalized_data)
  }
  
  # Normalize by division
  normalized_data[[paste0(source_column, "_norm_div")]] <- 
    normalized_data[[source_column]] / kpi_mean
  
  return(normalized_data)
}

# Normalize KPI by subtracting the mean (Center at zero)
normalize_by_subtraction <- function(data, kpi_column, transformed_column = NULL) {
  # If transformed_column is provided, use it as the source column
  # Otherwise use the original kpi_column
  source_column <- if (!is.null(transformed_column) && transformed_column %in% names(data)) {
    transformed_column
  } else {
    kpi_column
  }
  
  # Check if the source column exists in the data
  if (!source_column %in% names(data)) {
    notifyUser(paste("Source column", source_column, "not found in the data."), "error")
    return(data)
  }
  
  # Create a copy of the data to avoid modifying the original
  normalized_data <- data
  
  # Calculate the mean of the source column (ignoring NA values)
  kpi_mean <- mean(normalized_data[[source_column]], na.rm = TRUE)
  
  if (is.na(kpi_mean)) {
    notifyUser("Unable to normalize: mean is NA.", "warning")
    normalized_data[[paste0(source_column, "_norm_sub")]] <- NA_real_
    return(normalized_data)
  }
  
  # Normalize by subtraction
  normalized_data[[paste0(source_column, "_norm_sub")]] <- 
    normalized_data[[source_column]] - kpi_mean
  
  return(normalized_data)
}

# Helper function to get the appropriate column name for transformed/normalized data
get_column_name <- function(kpi_column, transformation, normalization, ma_window = 3) {
  # First determine the transformed column (if any)
  transformed_column <- switch(transformation,
                              "Logarithmic" = paste0(kpi_column, "_log"),
                              "Moving Average" = paste0(kpi_column, "_ma", ma_window),
                              kpi_column)  # Default to original KPI if no transformation
  
  # Then determine the normalized column (if any)
  final_column <- switch(normalization,
                        "Division by Mean" = paste0(transformed_column, "_norm_div"),
                        "Subtraction of Mean" = paste0(transformed_column, "_norm_sub"),
                        transformed_column)  # Default to transformed column if no normalization
  
  return(final_column)
}

# Apply full transformation and normalization pipeline
apply_full_kpi_processing <- function(data, kpi_column, 
                                     transformation = "None", 
                                     normalization = "None",
                                     ma_window = 3) {
  # Apply transformation if selected
  processed_data <- data
  
  if (transformation == "Logarithmic") {
    processed_data <- apply_log_transformation(processed_data, kpi_column)
    transformed_column <- paste0(kpi_column, "_log")
  } else if (transformation == "Moving Average") {
    processed_data <- apply_moving_average(processed_data, kpi_column, ma_window)
    transformed_column <- paste0(kpi_column, "_ma", ma_window)
  } else {
    # No transformation
    transformed_column <- kpi_column
  }
  
  # Apply normalization if selected
  if (normalization == "Division by Mean") {
    processed_data <- normalize_by_division(processed_data, kpi_column, transformed_column)
  } else if (normalization == "Subtraction of Mean") {
    processed_data <- normalize_by_subtraction(processed_data, kpi_column, transformed_column)
  }
  
  return(processed_data)
}

# Get final processed column name based on selected options
get_processed_column_name <- function(kpi_column, transformation, normalization, ma_window = 3) {
  # Determine transformed column name
  if (transformation == "Logarithmic") {
    transformed_column <- paste0(kpi_column, "_log")
  } else if (transformation == "Moving Average") {
    transformed_column <- paste0(kpi_column, "_ma", ma_window)
  } else {
    transformed_column <- kpi_column
  }
  
  # Determine normalized column name
  if (normalization == "Division by Mean") {
    final_column <- paste0(transformed_column, "_norm_div")
  } else if (normalization == "Subtraction of Mean") {
    final_column <- paste0(transformed_column, "_norm_sub")
  } else {
    final_column <- transformed_column
  }
  
  return(final_column)
}