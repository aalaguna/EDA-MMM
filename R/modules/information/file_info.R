# "# R/modules/common/file_info.R"
# =============================================================================
# Módulo: file_info
# Muestra información básica del archivo cargado.
# =============================================================================

file_info_module_server <- function(input, output, session, rv) {
  output$file_details <- renderPrint({
    req(rv$data)
    df <- rv$data
    cat("Número de filas:", nrow(df), "\n")
    cat("Número de columnas:", ncol(df), "\n")
    # cat("Columnas:", paste(names(df), collapse = ", "), "\n")
  })
}