# R/modules/univariate/data_filtering.R
# =============================================================================
# Módulo: data_filtering
# Maneja la lógica de filtrado de datos
# =============================================================================

filter_geography_data <- function(data, geography_univ) {
  req(data, geography_univ)
  df <- data
  date_col <- if ("Period" %in% names(df)) "Period" else if ("periodo" %in% names(df)) "periodo" else NULL
  req(date_col)
  
  if (geography_univ != "Total" && geography_univ != "N/A") {
    geo_col <- if("Geography" %in% names(df)) "Geography" else if("Geografia" %in% names(df)) "Geografia" else NULL
    if (!is.null(geo_col)) {
      df <- df %>% filter(.data[[geo_col]] == geography_univ)
    }
  } else if (geography_univ == "Total") {
    # Agrupar y sumar datos de todas las geografías por fecha
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    df <- df %>%
      group_by(across(all_of(date_col))) %>%
      summarise(across(all_of(numeric_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  }
  df
}