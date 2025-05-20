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
  } else {
    notifyUser("No positive values found for logarithmic transformation.", "warning")
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
  
  # Check if required packages are available
  if (!requireNamespace("zoo", quietly = TRUE)) {
    notifyUser("Package 'zoo' is required for moving average calculation.", "error")
    return(data)
  }
  
  # Create a copy of the data to avoid modifying the original
  transformed_data <- data
  
  # Apply moving average using zoo::rollmean
  tryCatch({
    # First, ensure data is ordered by date
    date_col <- if ("Period" %in% names(data)) "Period" else if ("periodo" %in% names(data)) "periodo" else NULL
    
    if (!is.null(date_col) && date_col %in% names(data)) {
      # Convert to Date if it's not already
      if (!inherits(data[[date_col]], "Date")) {
        transformed_data[[date_col]] <- as.Date(data[[date_col]])
      }
      
      # Sort by date for proper moving average calculation
      original_order <- seq_len(nrow(data))
      sorted_indices <- order(transformed_data[[date_col]])
      sorted_data <- transformed_data[sorted_indices, ]
      
      # Apply moving average to sorted data
      if (nrow(sorted_data) >= window_size) {
        kpi_values <- sorted_data[[kpi_column]]
        ma_values <- zoo::rollmean(kpi_values, k = window_size, fill = NA, align = "right")
        
        # Create a new column for the MA values in the sorted data
        sorted_data[[paste0(kpi_column, "_ma", window_size)]] <- ma_values
        
        # Restore the original order
        reordered_indices <- order(match(seq_len(nrow(sorted_data)), original_order))
        transformed_data <- sorted_data[reordered_indices, ]
      } else {
        # Not enough data for the window size
        transformed_data[[paste0(kpi_column, "_ma", window_size)]] <- rep(NA_real_, nrow(transformed_data))
      }
    } else {
      # If no date column, just apply rolling mean without sorting
      kpi_values <- transformed_data[[kpi_column]]
      transformed_data[[paste0(kpi_column, "_ma", window_size)]] <- 
        zoo::rollmean(kpi_values, k = window_size, fill = NA, align = "right")
    }
  }, error = function(e) {
    warning(paste("Error applying moving average:", e$message))
    # In case of error, create the column but fill with NA
    transformed_data[[paste0(kpi_column, "_ma", window_size)]] <- rep(NA_real_, nrow(transformed_data))
  })
  
  return(transformed_data)
}

# --------------------------
# Normalization Functions
# --------------------------

# Normalize KPI by dividing by the mean (Scale to mean=1 range)
normalize_by_division <- function(df, column_name, transformed_col = NULL) {
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }
  
  # If transformed_col is provided, use it as the source column
  source_col <- if (!is.null(transformed_col) && transformed_col %in% names(df)) {
    transformed_col
  } else {
    column_name
  }
  
  if (!source_col %in% names(df)) {
    warning(paste("Column", source_col, "not found in dataframe"))
    return(df)
  }
  
  # Extract column and convert to numeric if needed
  col_data <- tryCatch({
    as.numeric(df[[source_col]])
  }, error = function(e) {
    warning(paste("Error converting column to numeric:", e$message))
    return(NULL)
  })
  
  if (is.null(col_data)) {
    return(df)  # Return original if conversion failed
  }
  
  # Calculate mean safely
  valid_data <- col_data[is.finite(col_data) & !is.na(col_data)]
  if (length(valid_data) == 0) {
    warning("No valid data for normalization")
    norm_col_name <- paste0(source_col, "_norm_div")
    df[[norm_col_name]] <- NA
    return(df)
  }
  
  col_mean <- mean(valid_data, na.rm = TRUE)
  
  # Check for zero mean
  if (is.na(col_mean) || col_mean == 0) {
    warning("Mean is NA or zero, cannot normalize by division")
    norm_col_name <- paste0(source_col, "_norm_div")
    df[[norm_col_name]] <- NA
    return(df)
  }
  
  # Create normalized column
  norm_col_name <- paste0(source_col, "_norm_div")
  df[[norm_col_name]] <- col_data / col_mean
  
  return(df)
}

# Normalize KPI by subtracting the mean (Center at zero)
normalize_by_subtraction <- function(df, column_name, transformed_col = NULL) {
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }
  
  # If transformed_col is provided, use it as the source column
  source_col <- if (!is.null(transformed_col) && transformed_col %in% names(df)) {
    transformed_col
  } else {
    column_name
  }
  
  if (!source_col %in% names(df)) {
    warning(paste("Column", source_col, "not found in dataframe"))
    return(df)
  }
  
  # Extract column and convert to numeric if needed
  col_data <- tryCatch({
    as.numeric(df[[source_col]])
  }, error = function(e) {
    warning(paste("Error converting column to numeric:", e$message))
    return(NULL)
  })
  
  if (is.null(col_data)) {
    return(df)  # Return original if conversion failed
  }
  
  # Calculate mean safely
  valid_data <- col_data[is.finite(col_data) & !is.na(col_data)]
  if (length(valid_data) == 0) {
    warning("No valid data for normalization")
    norm_col_name <- paste0(source_col, "_norm_sub")
    df[[norm_col_name]] <- NA
    return(df)
  }
  
  col_mean <- mean(valid_data, na.rm = TRUE)
  
  # Create normalized column
  norm_col_name <- paste0(source_col, "_norm_sub")
  df[[norm_col_name]] <- col_data - col_mean
  
  return(df)
}

# Normalize KPI using the specified method
normalize_kpi <- function(df, kpi_col, method = "None") {
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }
  
  if (!kpi_col %in% names(df)) {
    warning(paste("KPI column", kpi_col, "not found in dataframe"))
    return(df)
  }
  
  # Apply normalization based on method
  if (method == "Division") {
    df <- normalize_by_division(df, kpi_col)
  } else if (method == "Subtraction") {
    df <- normalize_by_subtraction(df, kpi_col)
  }
  # For "None", just return the original dataframe
  
  return(df)
}

# Get the name of the normalized KPI column based on the method
get_normalized_kpi_name <- function(kpi_col, method = "None") {
  if (method == "Division") {
    return(paste0(kpi_col, "_norm_div"))
  } else if (method == "Subtraction") {
    return(paste0(kpi_col, "_norm_sub"))
  }
  return(kpi_col)  # For "None", return the original column name
}

# Apply full transformation and normalization pipeline
apply_full_kpi_processing <- function(data, kpi_column, 
                                     transformation = "None", 
                                     normalization = "None",
                                     ma_window = 3,
                                     decay_rate = 1,
                                     lag_value = 0) {
  # Check if the KPI column exists in the data
  if (!kpi_column %in% names(data)) {
    warning(paste("KPI column", kpi_column, "not found in dataframe"))
    return(list(data = data, final_column = kpi_column))
  }
  
  # Step 1: Apply transformation
  processed_data <- data
  transformed_column <- kpi_column  # Default to original column if no transformation
  
  if (transformation == "Logarithmic") {
    processed_data <- apply_log_transformation(processed_data, kpi_column)
    transformed_column <- paste0(kpi_column, "_log")
  } else if (transformation == "Moving Average") {
    processed_data <- apply_moving_average(processed_data, kpi_column, ma_window)
    transformed_column <- paste0(kpi_column, "_ma", ma_window)
  }
  
  # Check if the transformed column exists
  if (!transformed_column %in% names(processed_data)) {
    warning(paste("Transformed column", transformed_column, "not found after transformation"))
    transformed_column <- kpi_column  # Fall back to original column
  }
  
  # Step 2: Apply lag to the transformed column
  lagged_column <- transformed_column  # Default to transformed column if no lag
  
  if (lag_value > 0) {
    # Extract transformed values
    trans_values <- processed_data[[transformed_column]]
    
    # Apply lag
    lagged_values <- rep(NA_real_, length(trans_values))
    if (lag_value < length(trans_values)) {
      lagged_values[(lag_value+1):length(trans_values)] <- trans_values[1:(length(trans_values)-lag_value)]
    }
    
    # Create lagged column
    lagged_column <- paste0(transformed_column, "_lag", lag_value)
    processed_data[[lagged_column]] <- lagged_values
  }
  
  # Check if the lagged column exists
  if (!lagged_column %in% names(processed_data)) {
    warning(paste("Lagged column", lagged_column, "not found after applying lag"))
    lagged_column <- transformed_column  # Fall back to transformed column
  }
  
  # Step 3: Apply accumulative decay to the lagged column
  decayed_column <- lagged_column  # Default to lagged column if no decay
  
  if (decay_rate != 1) {
    # Extract lagged values
    lagged_values <- processed_data[[lagged_column]]
    
    # Apply accumulative decay
    decayed_values <- rep(NA_real_, length(lagged_values))
    
    # Find first non-NA value for initialization
    first_valid_idx <- which(!is.na(lagged_values))
    if (length(first_valid_idx) > 0) {
      first_valid_idx <- min(first_valid_idx)
      
      # Initialize first value
      decayed_values[first_valid_idx] <- lagged_values[first_valid_idx]
      
      # Apply decay formula for remaining values
      if (first_valid_idx < length(lagged_values)) {
        for (i in (first_valid_idx+1):length(lagged_values)) {
          # If current value is NA, use 0 in the formula (or keep as NA)
          current_val <- if (is.na(lagged_values[i])) 0 else lagged_values[i]
          decayed_values[i] <- current_val + decay_rate * decayed_values[i-1]
        }
      }
    }
    
    # Create decayed column
    decayed_column <- paste0(lagged_column, "_decay", decay_rate)
    processed_data[[decayed_column]] <- decayed_values
  }
  
  # Check if the decayed column exists
  if (!decayed_column %in% names(processed_data)) {
    warning(paste("Decayed column", decayed_column, "not found after applying decay"))
    decayed_column <- lagged_column  # Fall back to lagged column
  }
  
  # Step 4: Apply normalization
  final_column <- decayed_column  # Default to decayed column if no normalization
  
  if (normalization != "None") {
    if (normalization == "Division") {
      processed_data <- normalize_by_division(processed_data, kpi_column, decayed_column)
      final_column <- paste0(decayed_column, "_norm_div")
    } else if (normalization == "Subtraction") {
      processed_data <- normalize_by_subtraction(processed_data, kpi_column, decayed_column)
      final_column <- paste0(decayed_column, "_norm_sub")
    }
  }
  
  # Check if the final column exists
  if (!final_column %in% names(processed_data)) {
    warning(paste("Final column", final_column, "not found after normalization"))
    final_column <- decayed_column  # Fall back to decayed column
  }
  
  # Return both the processed data and the final column name
  return(list(data = processed_data, final_column = final_column))
}

# Get processed column name for a given transformation pipeline
get_processed_column_name <- function(kpi_column, transformation, normalization, 
                                     ma_window = 3, lag_value = 0, decay_rate = 1) {
  # Start with the original column
  result_column <- kpi_column
  
  # Apply transformation naming
  if (transformation == "Logarithmic") {
    result_column <- paste0(result_column, "_log")
  } else if (transformation == "Moving Average") {
    result_column <- paste0(result_column, "_ma", ma_window)
  }
  
  # Apply lag naming
  if (lag_value > 0) {
    result_column <- paste0(result_column, "_lag", lag_value)
  }
  
  # Apply decay naming
  if (decay_rate != 1) {
    result_column <- paste0(result_column, "_decay", decay_rate)
  }
  
  # Apply normalization naming
  if (normalization == "Division") {
    result_column <- paste0(result_column, "_norm_div")
  } else if (normalization == "Subtraction") {
    result_column <- paste0(result_column, "_norm_sub")
  }
  
  return(result_column)
}

# Safe function to notify users - falls back gracefully if shiny is not available
notifyUser <- function(message, type = "message", duration = 5) {
  if (exists("showNotification") && is.function(get("showNotification"))) {
    if (type == "error") {
      showNotification(message, type = "error", duration = duration)
    } else if (type == "warning") {
      showNotification(message, type = "warning", duration = duration)
    } else {
      showNotification(message, type = "message", duration = duration)
    }
  } else {
    # Fallback if showNotification is not available
    if (type == "error") {
      warning(message)
    } else if (type == "warning") {
      warning(message)
    } else {
      message(message)
    }
  }
}