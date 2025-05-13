# =============================================================================
# Download handlers for KPI transformation and normalization
# =============================================================================

kpi_transformation_download_handler <- function(input, output, session, rv) {
  # Download handler for transformed KPI
  output$download_transformed_kpi <- downloadHandler(
    filename = function() {
      # Generate filename based on KPI and transformation
      kpi_name <- gsub("[^a-zA-Z0-9]", "_", input$kpi_selection)
      transform_type <- gsub(" ", "_", input$transformation_kpi)
      paste0("kpi_", kpi_name, "_", transform_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      tryCatch({
        withProgress(message = 'Preparing transformed KPI download...', value = 0.1, {
          # Get filtered data
          data <- rv$kpi_filtered_data()
          
          if (is.null(data) || nrow(data) == 0) {
            stop("No data available for download.")
          }
          
          # Determine which columns to include
          date_col <- if ("Period" %in% names(data)) "Period" else if ("periodo" %in% names(data)) "periodo" else NULL
          kpi_col <- input$kpi_selection
          
          # Skip if no KPI selected
          if (is.null(kpi_col) || kpi_col == "") {
            stop("No KPI selected for transformation.")
          }
          
          # Apply transformation
          transformed_data <- data
          
          incProgress(0.3)
          
          if (input$transformation_kpi == "Logarithmic") {
            transformed_data <- apply_log_transformation(transformed_data, kpi_col)
            transformed_col <- paste0(kpi_col, "_log")
          } else if (input$transformation_kpi == "Moving Average") {
            window_size <- input$ma_window
            transformed_data <- apply_moving_average(transformed_data, kpi_col, window_size)
            transformed_col <- paste0(kpi_col, "_ma", window_size)
          } else {
            # If no transformation, use original KPI
            transformed_col <- kpi_col
          }
          
          # Select columns for download
          if (!is.null(date_col)) {
            download_data <- transformed_data[, c(date_col, kpi_col, transformed_col)]
          } else {
            download_data <- transformed_data[, c(kpi_col, transformed_col)]
          }
          
          incProgress(0.3)
          
          # Create metadata for the transformation
          metadata <- data.frame(
            Parameter = c(
              "KPI", 
              "Transformation", 
              "Window Size (MA only)",
              "Start Date", 
              "End Date", 
              "Download Date"
            ),
            Value = c(
              kpi_col,
              input$transformation_kpi,
              ifelse(input$transformation_kpi == "Moving Average", as.character(input$ma_window), "N/A"),
              ifelse(!is.null(input$date_range_kpi), as.character(input$date_range_kpi[1]), "N/A"),
              ifelse(!is.null(input$date_range_kpi), as.character(input$date_range_kpi[2]), "N/A"),
              as.character(Sys.Date())
            )
          )
          
          incProgress(0.2)
          
          # Write data and metadata to files
          write.csv(download_data, file, row.names = FALSE, na = "")
          
          # Create metadata file with the same name but with suffix _metadata
          metadata_file <- sub("\\.csv$", "_metadata.csv", file)
          write.csv(metadata, metadata_file, row.names = FALSE)
          
          incProgress(0.1)
        })
        
        notifyUser("Transformed KPI data downloaded successfully.", "message", duration = 4)
        
      }, error = function(e) {
        notifyUser(paste("Error downloading transformed KPI data:", e$message), "error", duration = 5)
      })
    }
  )
  
  # Download handler for normalized KPI
  output$download_normalized_kpi <- downloadHandler(
    filename = function() {
      # Generate filename based on KPI, transformation, and normalization
      kpi_name <- gsub("[^a-zA-Z0-9]", "_", input$kpi_selection)
      transform_type <- gsub(" ", "_", input$transformation_kpi)
      normalize_type <- gsub(" ", "_", input$normalization_kpi)
      paste0("kpi_", kpi_name, "_", transform_type, "_", normalize_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      tryCatch({
        withProgress(message = 'Preparing normalized KPI download...', value = 0.1, {
          # Get filtered data
          data <- rv$kpi_filtered_data()
          
          if (is.null(data) || nrow(data) == 0) {
            stop("No data available for download.")
          }
          
          # Determine which columns to include
          date_col <- if ("Period" %in% names(data)) "Period" else if ("periodo" %in% names(data)) "periodo" else NULL
          kpi_col <- input$kpi_selection
          
          # Skip if no KPI selected
          if (is.null(kpi_col) || kpi_col == "") {
            stop("No KPI selected for normalization.")
          }
          
          # First apply transformation (if any)
          transformed_data <- data
          
          if (input$transformation_kpi == "Logarithmic") {
            transformed_data <- apply_log_transformation(transformed_data, kpi_col)
            transformed_col <- paste0(kpi_col, "_log")
          } else if (input$transformation_kpi == "Moving Average") {
            window_size <- input$ma_window
            transformed_data <- apply_moving_average(transformed_data, kpi_col, window_size)
            transformed_col <- paste0(kpi_col, "_ma", window_size)
          } else {
            # If no transformation, use original KPI
            transformed_col <- kpi_col
          }
          
          incProgress(0.3)
          
          # Then apply normalization
          normalized_data <- transformed_data
          
          if (input$normalization_kpi == "Division by Mean") {
            normalized_data <- normalize_by_division(normalized_data, kpi_col, transformed_col)
            normalized_col <- paste0(transformed_col, "_norm_div")
          } else if (input$normalization_kpi == "Subtraction of Mean") {
            normalized_data <- normalize_by_subtraction(normalized_data, kpi_col, transformed_col)
            normalized_col <- paste0(transformed_col, "_norm_sub")
          } else {
            # If no normalization, use transformed column
            normalized_col <- transformed_col
          }
          
          # Select columns for download
          if (!is.null(date_col)) {
            download_data <- normalized_data[, c(date_col, kpi_col, transformed_col, normalized_col)]
          } else {
            download_data <- normalized_data[, c(kpi_col, transformed_col, normalized_col)]
          }
          
          incProgress(0.3)
          
          # Create metadata for the transformation and normalization
          metadata <- data.frame(
            Parameter = c(
              "KPI", 
              "Transformation", 
              "Window Size (MA only)",
              "Normalization",
              "Start Date", 
              "End Date", 
              "Download Date"
            ),
            Value = c(
              kpi_col,
              input$transformation_kpi,
              ifelse(input$transformation_kpi == "Moving Average", as.character(input$ma_window), "N/A"),
              input$normalization_kpi,
              ifelse(!is.null(input$date_range_kpi), as.character(input$date_range_kpi[1]), "N/A"),
              ifelse(!is.null(input$date_range_kpi), as.character(input$date_range_kpi[2]), "N/A"),
              as.character(Sys.Date())
            )
          )
          
          incProgress(0.2)
          
          # Write data and metadata to files
          write.csv(download_data, file, row.names = FALSE, na = "")
          
          # Create metadata file with the same name but with suffix _metadata
          metadata_file <- sub("\\.csv$", "_metadata.csv", file)
          write.csv(metadata, metadata_file, row.names = FALSE)
          
          incProgress(0.1)
        })
        
        notifyUser("Normalized KPI data downloaded successfully.", "message", duration = 4)
        
      }, error = function(e) {
        notifyUser(paste("Error downloading normalized KPI data:", e$message), "error", duration = 5)
      })
    }
  )
}