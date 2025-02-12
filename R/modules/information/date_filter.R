# "# R/modules/common/date_filter.R"
# =============================================================================
# Módulo: date_filter
# Función para filtrar los datos por rango de fechas.  Se asegura de que
# la columna de fecha sea tratada correctamente.
# =============================================================================

filter_data_by_date <- function(input, rv) {
  observe({
    req(rv$data, input$date_range_filter)

    df <- rv$data
     #Usar la columna detectada previamente
    date_col <- rv$date_col 

    if (!is.null(date_col)) {
      # Asegurarse de que la columna sea de tipo Date
      df[[date_col]] <- as.Date(df[[date_col]])

      df <- df %>%
        filter(.data[[date_col]] >= as.Date(input$date_range_filter[1]) &
                 .data[[date_col]] <= as.Date(input$date_range_filter[2]))

       rv$filtered_data <- df  # Actualizar *después* de filtrar
    }
      notifyUser("Datos filtrados por rango de fechas.", "message", duration = 2)

  })
}