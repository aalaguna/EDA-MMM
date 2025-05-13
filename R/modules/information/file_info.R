# File_Info.R

# =============================================================================
# Displays basic information about the uploaded file.
# =============================================================================

file_info_module_server <- function(input, output, session, rv) {
  # Server module to display basic file information
  #
  # Args:
  #   input: Shiny input object
  #   output: Shiny output object
  #   session: Shiny session object
  #   rv: Shared reactive values
  
  output$file_details <- renderPrint({
    req(rv$data)
    df <- rv$data
    cat("Number of Rows:", nrow(df), "\n")
    cat("Number of Columns:", ncol(df), "\n")
  })
}
