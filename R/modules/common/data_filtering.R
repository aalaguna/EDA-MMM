# R/modules/common/data_filtering.R
# =============================================================================
# Módulo: data_filtering
# Funciones comunes para filtrar datos según geografía y fecha, manteniendo la lógica original.
# =============================================================================

filter_by_geography <- function(data, geography_input, geo_col_names = c("Geography", "Geografia"), total_value = "Total") {
  req(data, geography_input)
  geo_col <- intersect(geo_col_names, names(data))
  if (length(geo_col) > 0) {
    geo_col <- geo_col[1]
    if (geography_input != total_value && geography_input != "N/A") {
      data <- data %>% filter(.data[[geo_col]] == geography_input)
    } else if (geography_input == total_value) {
      # Agrupar y sumar datos de todas las geografías por fecha
      date_col <- if ("Period" %in% names(data)) "Period" else if ("periodo" %in% names(data)) "periodo" else NULL
      if (!is.null(date_col)) {
        numeric_cols <- names(data)[sapply(data, is.numeric)]
        data <- data %>%
          group_by(across(all_of(date_col))) %>%
          summarise(across(all_of(numeric_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
      }
    }
  }
  return(data)
}

filter_by_date <- function(data, start_date, end_date) {
  req(data)
  date_col <- if ("Period" %in% names(data)) "Period" else if ("periodo" %in% names(data)) "periodo" else NULL
  if (!is.null(date_col)) {
    data <- data %>% filter(as.Date(.data[[date_col]]) >= start_date, as.Date(.data[[date_col]]) <= end_date)
  }
  return(data)
}
