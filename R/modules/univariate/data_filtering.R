# =============================================================================
# Filtering logic for univariate analysis, specifically by geography.
# =============================================================================

filter_geography_data <- function(data, geography_univ) {
  # Filters the dataframe by the selected geography
  #
  # Args:
  #   data: Dataframe to filter
  #   geography_univ: Selected geography value
  #
  # Returns:
  #   Dataframe filtered by geography
  
  # Check for required inputs
  if (is.null(data) || nrow(data) == 0) {
    warning("No data provided for filtering")
    return(data)
  }
  
  if (is.null(geography_univ) || geography_univ == "N/A") {
    return(data)  # Return original data if no geography selected
  }
  
  # Make a copy to avoid modifying the original
  df <- data
  
  # Identify date column
  date_col <- if ("Period" %in% names(df)) {
    "Period"
  } else if ("periodo" %in% names(df)) {
    "periodo"
  } else {
    NULL
  }
  
  # Ensure date column is properly formatted
  if (!is.null(date_col) && date_col %in% names(df)) {
    # Convert date column to Date type if it's not already
    if (!inherits(df[[date_col]], "Date")) {
      tryCatch({
        df[[date_col]] <- as.Date(df[[date_col]])
      }, error = function(e) {
        warning(paste("Error converting date column to Date type:", e$message))
      })
    }
  }
  
  # Apply geography filtering
  if (geography_univ != "Total" && geography_univ != "N/A") {
    # Find geography column
    geo_col <- if ("Geography" %in% names(df)) {
      "Geography"
    } else if ("Geografia" %in% names(df)) {
      "Geografia"
    } else {
      NULL
    }
    
    # Filter by geography if column exists
    if (!is.null(geo_col) && geo_col %in% names(df)) {
      # Check if the geography value exists in the data
      if (geography_univ %in% unique(df[[geo_col]])) {
        df <- df[df[[geo_col]] == geography_univ, ]
      } else {
        warning(paste("Geography value", geography_univ, "not found in the data"))
      }
    } else {
      warning("Geography column not found in the data")
    }
  } else if (geography_univ == "Total") {
    # Aggregate data by date for "Total" geography
    
    # We need the date column for aggregation
    if (is.null(date_col) || !date_col %in% names(df)) {
      warning("Date column not found, cannot aggregate for 'Total' geography")
      return(df)
    }
    
    # Identify numeric columns for aggregation
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    
    if (length(numeric_cols) == 0) {
      warning("No numeric columns found for aggregation")
      return(df)
    }
    
    # Use dplyr for aggregation if available, otherwise use base R
    if (requireNamespace("dplyr", quietly = TRUE)) {
      tryCatch({
        df <- df %>%
          dplyr::group_by(across(all_of(date_col))) %>%
          dplyr::summarise(across(all_of(numeric_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
      }, error = function(e) {
        warning(paste("Error in dplyr aggregation:", e$message))
        # Fallback to base R aggregation
        aggregate_df <- aggregate(df[numeric_cols], by = list(date = df[[date_col]]), FUN = sum, na.rm = TRUE)
        names(aggregate_df)[1] <- date_col
        df <- aggregate_df
      })
    } else {
      # Base R aggregation
      aggregate_df <- aggregate(df[numeric_cols], by = list(date = df[[date_col]]), FUN = sum, na.rm = TRUE)
      names(aggregate_df)[1] <- date_col
      df <- aggregate_df
    }
  }
  
  # Check if we still have data after filtering
  if (nrow(df) == 0) {
    warning("No data remains after applying geography filter")
  }
  
  return(df)
}