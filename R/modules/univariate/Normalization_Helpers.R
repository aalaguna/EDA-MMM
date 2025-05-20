# =============================================================================
# Normalization helper functions for KPI and variables in univariate analysis
# =============================================================================

#' Normalize data by division (division by mean)
#'
#' @param df Dataframe containing the data
#' @param column_name Name of the column to normalize
#' @return Dataframe with added normalized column
normalize_by_division <- function(df, column_name) {
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }
  
  if (!column_name %in% names(df)) {
    warning(paste("Column", column_name, "not found in dataframe"))
    return(df)
  }
  
  # Extract column and convert to numeric if needed
  col_data <- tryCatch({
    as.numeric(df[[column_name]])
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
    norm_col_name <- paste0(column_name, "_norm_div")
    df[[norm_col_name]] <- NA
    return(df)
  }
  
  col_mean <- mean(valid_data, na.rm = TRUE)
  
  # Check for zero mean
  if (is.na(col_mean) || col_mean == 0) {
    warning("Mean is NA or zero, cannot normalize by division")
    norm_col_name <- paste0(column_name, "_norm_div")
    df[[norm_col_name]] <- NA
    return(df)
  }
  
  # Create normalized column
  norm_col_name <- paste0(column_name, "_norm_div")
  df[[norm_col_name]] <- col_data / col_mean
  
  return(df)
}

#' Normalize data by subtraction (subtraction of mean)
#'
#' @param df Dataframe containing the data
#' @param column_name Name of the column to normalize
#' @return Dataframe with added normalized column
normalize_by_subtraction <- function(df, column_name) {
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }
  
  if (!column_name %in% names(df)) {
    warning(paste("Column", column_name, "not found in dataframe"))
    return(df)
  }
  
  # Extract column and convert to numeric if needed
  col_data <- tryCatch({
    as.numeric(df[[column_name]])
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
    norm_col_name <- paste0(column_name, "_norm_sub")
    df[[norm_col_name]] <- NA
    return(df)
  }
  
  col_mean <- mean(valid_data, na.rm = TRUE)
  
  # Create normalized column
  norm_col_name <- paste0(column_name, "_norm_sub")
  df[[norm_col_name]] <- col_data - col_mean
  
  return(df)
}

#' Normalize KPI using the specified method
#'
#' @param df Dataframe containing the data
#' @param kpi_col Name of the KPI column to normalize
#' @param method Normalization method: "None", "Division", or "Subtraction"
#' @return Dataframe with added normalized KPI column
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

#' Get the name of the normalized KPI column based on the method
#'
#' @param kpi_col Original KPI column name
#' @param method Normalization method: "None", "Division", or "Subtraction"
#' @return Name of the normalized column, or original if "None"
get_normalized_kpi_name <- function(kpi_col, method = "None") {
  if (method == "Division") {
    return(paste0(kpi_col, "_norm_div"))
  } else if (method == "Subtraction") {
    return(paste0(kpi_col, "_norm_sub"))
  }
  return(kpi_col)  # For "None", return the original column name
}