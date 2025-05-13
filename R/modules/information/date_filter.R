# Date_Filter.R

# =============================================================================
# Common function to filter data by date range from the "Information" tab.
# =============================================================================

filter_data_by_date <- function(input, rv) {
  # Filters data by date range and updates reactive values
  #
  # Args:
  #   input: Shiny input object
  #   rv: Shared reactive values

  observe({
    req(rv$data, input$date_range_filter)
    df <- rv$data

    # Try to detect date column from rv$date_col
    date_col <- rv$date_col
    if (!is.null(date_col) && date_col %in% names(df)) {
      # Ensure column is Date
      df[[date_col]] <- as.Date(df[[date_col]])
      # Filter
      df <- filter_by_date(df, input$date_range_filter[1], input$date_range_filter[2])
      rv$filtered_data <- df  # Update globally filtered data
    }
    notifyUser("Data filtered by date range.", "message", duration = 2)
  })
}

