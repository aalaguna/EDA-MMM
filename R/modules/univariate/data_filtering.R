# Data_Filtering.R

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
  
  req(data, geography_univ)
  df <- data
  date_col <- if ("Period" %in% names(df)) "Period" else if ("periodo" %in% names(df)) "periodo" else NULL
  req(date_col)

  if (geography_univ != "Total" && geography_univ != "N/A") {
    geo_col <- if ("Geography" %in% names(df)) "Geography" else if ("Geografia" %in% names(df)) "Geografia" else NULL
    if (!is.null(geo_col)) {
      df <- df %>% filter(.data[[geo_col]] == geography_univ)
    }
  } else if (geography_univ == "Total") {
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    df <- df %>%
      group_by(across(all_of(date_col))) %>%
      summarise(across(all_of(numeric_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  }

  return(df)
}