# "# R/modules/common/dimensions_ui.R"
# =============================================================================
# Módulo: dimensions_ui
# Renderiza las salidas de la detección de dimensiones temporales y
# transversales del dataset.  Se corrige para usar la columna de fecha
# detectada y se simplifica.
# =============================================================================

dimensions_ui_module_server <- function(input, output, session, rv) {

  output$temporal_dimension_ui <- renderUI({
    req(rv$date_col)  # Usar la columna detectada
    div(paste(rv$date_col, collapse = ", "))
  })


  output$cross_sectional_dimension_ui <- renderUI({
    req(rv$data)
    keywords <- c("Geography", "Geografia", "Product", "Campaign", "Outlet", "Creative")
    available_columns <- names(rv$data)[names(rv$data) %in% keywords]
    if (length(available_columns) > 0) {
      div(paste(available_columns, collapse = ", "))
    } else {
      div("No se encontró dimensión transversal en el dataset.")
    }
  })
}