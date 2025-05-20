# Dimensions.R

# =============================================================================
# Renders the outputs to detect temporal and cross-sectional
# dimensions of the dataset.
# =============================================================================

dimensions_ui_module_server <- function(input, output, session, rv) {
  # Server module to display information about dataset dimensions
  #
  # Args:
  #   input: Shiny input object
  #   output: Shiny output object
  #   session: Shiny session object
  #   rv: Shared reactive values
  
  output$temporal_dimension_ui <- renderUI({
    req(rv$date_col)
    div(rv$date_col)
  })
  
  output$cross_sectional_dimension_ui <- renderUI({
    req(rv$data)
    keywords <- c("Geography", "Geografia", "Product", "Campaign", "Outlet", "Creative")
    available_columns <- names(rv$data)[names(rv$data) %in% keywords]
    if (length(available_columns) > 0) {
      div(paste(available_columns, collapse = ", "))
    } else {
      div("No cross-sectional dimensions were detected in the dataset.")
    }
  })
}
