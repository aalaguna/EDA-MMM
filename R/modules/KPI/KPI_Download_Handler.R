# =============================================================================
# Download handlers for KPI transformation and normalization
# =============================================================================

kpi_transformation_download_handler <- function(input, output, session, rv) {
  # Create a sanitized filename from inputs
  create_sanitized_filename <- function(kpi_name, transform_type, normalize_type = NULL, date_suffix = TRUE) {
    # Replace special characters with underscores
    kpi_name <- gsub("[^a-zA-Z0-9]", "_", kpi_name)
    transform_type <- gsub(" ", "_", transform_type)
    
    if (!is.null(normalize_type)) {
      normalize_type <- gsub(" ", "_", normalize_type)
      base_name <- paste0("kpi_", kpi_name, "_", transform_type, "_", normalize_type)
    } else {
      base_name <- paste0("kpi_", kpi_name, "_", transform_type)
    }
    
    # Add date suffix if requested
    if (date_suffix) {
      return(paste0(base_name, "_", format(Sys.Date(), "%Y%m%d"), ".csv"))
    } else {
      return(paste0(base_name, ".csv"))
    }
  }
  
  # Helper function to create metadata for downloads
  create_metadata_dataframe <- function(kpi_col, transformation, normalization = NULL, 
                                        window_size = NULL, date_range = NULL) {
    # Base parameters
    params <- c(
      "KPI", 
      "Transformation", 
      "Download Date"
    )
    
    values <- c(
      kpi_col,
      transformation,
      as.character(Sys.Date())
    )
    
    # Add window size if provided and applicable
    if (!is.null(window_size) && transformation == "Moving Average") {
      params <- c(params, "Window Size (MA)")
      values <- c(values, as.character(window_size))
    }
    
    # Add normalization if provided
    if (!is.null(normalization) && normalization != "None") {
      params <- c(params, "Normalization")
      values <- c(values, normalization)
    }
    
    # Add date range if provided
    if (!is.null(date_range) && length(date_range) == 2) {
      params <- c(params, "Start Date", "End Date")
      values <- c(values, as.character(date_range[1]), as.character(date_range[2]))
    }
    
    # Create metadata dataframe
    metadata <- data.frame(
      Parameter = params,
      Value = values,
      stringsAsFactors = FALSE
    )
    
    return(metadata)
  }
  
  # Common error handling function for downloads
  handle_download_errors <- function(expr, success_message, error_prefix) {
    tryCatch({
      expr
      notifyUser(success_message, "message", duration = 4)
    }, error = function(e) {
      notifyUser(paste(error_prefix, e$message), "error", duration = 5)
    }, warning = function(w) {
      notifyUser(paste("Warning:", w$message), "warning", duration = 4)
    })
  }
  
  # Download handler for transformed KPI
  output$download_transformed_kpi <- downloadHandler(
    filename = function() {
      create_sanitized_filename(
        kpi_name = input$kpi_selection,
        transform_type = input$transformation_kpi
      )
    },
    content = function(file) {
      handle_download_errors({
        withProgress(message = 'Preparing transformed KPI download...', value = 0.1, {
          # Get filtered data with error checking
          data <- tryCatch({
            rv$kpi_filtered_data()
          }, error = function(e) {
            stop(paste("Error accessing filtered data:", e$message))
          })
          
          if (is.null(data) || nrow(data) == 0) {
            stop("No data available for download.")
          }
          
          # Determine which columns to include
          date_col <- if ("Period" %in% names(data)) "Period" else if ("periodo" %in% names(data)) "periodo" else NULL
          kpi_col <- input$kpi_selection
          
          # Skip if no KPI selected
          if (is.null(kpi_col) || kpi_col == "" || !kpi_col %in% names(data)) {
            stop("No valid KPI selected for transformation.")
          }
          
          # Apply transformation
          transformed_data <- data
          
          incProgress(0.3)
          
          if (input$transformation_kpi == "Logarithmic") {
            transformed_data <- apply_log_transformation(transformed_data, kpi_col)
            transformed_col <- paste0(kpi_col, "_log")
          } else if (input$transformation_kpi == "Moving Average") {
            window_size <- as.integer(input$ma_window)
            if (is.na(window_size) || window_size < 1) {
              window_size <- 3
              notifyUser("Invalid moving average window size. Using default value of 3.", "warning")
            }
            transformed_data <- apply_moving_average(transformed_data, kpi_col, window_size)
            transformed_col <- paste0(kpi_col, "_ma", window_size)
          } else {
            # If no transformation, use original KPI
            transformed_col <- kpi_col
            notifyUser("No transformation selected. Downloading original KPI data.", "warning")
          }
          
          # Check if transformation was successful
          if (transformed_col != kpi_col && !transformed_col %in% names(transformed_data)) {
            stop(paste("Transformation failed. Column", transformed_col, "not found in the processed data."))
          }
          
          # Select columns for download
          if (!is.null(date_col) && date_col %in% names(transformed_data)) {
            download_data <- transformed_data[, c(date_col, kpi_col, transformed_col)]
          } else {
            download_data <- transformed_data[, c(kpi_col, transformed_col)]
          }
          
          incProgress(0.3)
          
          # Create metadata for the transformation
          metadata <- create_metadata_dataframe(
            kpi_col = kpi_col,
            transformation = input$transformation_kpi,
            window_size = if (input$transformation_kpi == "Moving Average") input$ma_window else NULL,
            date_range = input$date_range_kpi
          )
          
          incProgress(0.2)
          
          # Write data and metadata to files
          write.csv(download_data, file, row.names = FALSE, na = "")
          
          # Create metadata file with the same name but with suffix _metadata
          metadata_file <- sub("\\.csv$", "_metadata.csv", file)
          write.csv(metadata, metadata_file, row.names = FALSE)
          
          incProgress(0.1)
        })
      },
      success_message = "Transformed KPI data downloaded successfully.",
      error_prefix = "Error downloading transformed KPI data:"
      )
    }
  )
  
  # Download handler for normalized KPI
  output$download_normalized_kpi <- downloadHandler(
    filename = function() {
      create_sanitized_filename(
        kpi_name = input$kpi_selection,
        transform_type = input$transformation_kpi,
        normalize_type = input$normalization_kpi
      )
    },
    content = function(file) {
      handle_download_errors({
        withProgress(message = 'Preparing normalized KPI download...', value = 0.1, {
          # Get filtered data with error checking
          data <- tryCatch({
            rv$kpi_filtered_data()
          }, error = function(e) {
            stop(paste("Error accessing filtered data:", e$message))
          })
          
          if (is.null(data) || nrow(data) == 0) {
            stop("No data available for download.")
          }
          
          # Determine which columns to include
          date_col <- if ("Period" %in% names(data)) "Period" else if ("periodo" %in% names(data)) "periodo" else NULL
          kpi_col <- input$kpi_selection
          
          # Skip if no KPI selected
          if (is.null(kpi_col) || kpi_col == "" || !kpi_col %in% names(data)) {
            stop("No valid KPI selected for normalization.")
          }
          
          # Skip if no normalization selected
          if (is.null(input$normalization_kpi) || input$normalization_kpi == "None") {
            stop("No normalization method selected.")
          }
          
          # First apply transformation (if any)
          transformed_data <- data
          
          if (input$transformation_kpi == "Logarithmic") {
            transformed_data <- apply_log_transformation(transformed_data, kpi_col)
            transformed_col <- paste0(kpi_col, "_log")
          } else if (input$transformation_kpi == "Moving Average") {
            window_size <- as.integer(input$ma_window)
            if (is.na(window_size) || window_size < 1) {
              window_size <- 3
              notifyUser("Invalid moving average window size. Using default value of 3.", "warning")
            }
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
            stop("No normalization method selected.")
          }
          
          # Check if normalization was successful
          if (!normalized_col %in% names(normalized_data)) {
            stop(paste("Normalization failed. Column", normalized_col, "not found in the processed data."))
          }
          
          # Select columns for download
          columns_to_include <- c()
          if (!is.null(date_col) && date_col %in% names(normalized_data)) {
            columns_to_include <- c(columns_to_include, date_col)
          }
          columns_to_include <- c(columns_to_include, kpi_col)
          
          # Add transformed column if different from KPI
          if (transformed_col != kpi_col && transformed_col %in% names(normalized_data)) {
            columns_to_include <- c(columns_to_include, transformed_col)
          }
          
          # Add normalized column
          columns_to_include <- c(columns_to_include, normalized_col)
          
          # Create download data with selected columns
          download_data <- normalized_data[, columns_to_include, drop = FALSE]
          
          incProgress(0.3)
          
          # Create metadata for the transformation and normalization
          metadata <- create_metadata_dataframe(
            kpi_col = kpi_col,
            transformation = input$transformation_kpi,
            normalization = input$normalization_kpi,
            window_size = if (input$transformation_kpi == "Moving Average") input$ma_window else NULL,
            date_range = input$date_range_kpi
          )
          
          incProgress(0.2)
          
          # Write data and metadata to files
          write.csv(download_data, file, row.names = FALSE, na = "")
          
          # Create metadata file with the same name but with suffix _metadata
          metadata_file <- sub("\\.csv$", "_metadata.csv", file)
          write.csv(metadata, metadata_file, row.names = FALSE)
          
          incProgress(0.1)
        })
      },
      success_message = "Normalized KPI data downloaded successfully.",
      error_prefix = "Error downloading normalized KPI data:"
      )
    }
  )
  
  # Download handler for dimension analysis
  output$download_dimension_analysis <- downloadHandler(
    filename = function() {
      dimension <- input$dimension_select
      kpi_name <- input$kpi_selection
      
      sanitized_dim <- gsub("[^a-zA-Z0-9]", "_", dimension)
      sanitized_kpi <- gsub("[^a-zA-Z0-9]", "_", kpi_name)
      
      paste0("dimension_analysis_", sanitized_dim, "_", sanitized_kpi, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      handle_download_errors({
        withProgress(message = 'Preparing dimension analysis download...', value = 0.1, {
          # Get dimension data
          dim_data <- kpi_module_rv$dimension_data
          
          if (is.null(dim_data) || nrow(dim_data) == 0) {
            stop("No dimension analysis data available for download.")
          }
          
          incProgress(0.5)
          
          # Create metadata for the dimension analysis
          metadata <- data.frame(
            Parameter = c(
              "KPI", 
              "Dimension",
              "Transformation",
              "Normalization",
              "Download Date"
            ),
            Value = c(
              input$kpi_selection,
              input$dimension_select,
              input$transformation_kpi,
              input$normalization_kpi,
              as.character(Sys.Date())
            ),
            stringsAsFactors = FALSE
          )
          
          incProgress(0.3)
          
          # Write data and metadata to files
          write.csv(dim_data, file, row.names = FALSE, na = "")
          
          # Create metadata file with the same name but with suffix _metadata
          metadata_file <- sub("\\.csv$", "_metadata.csv", file)
          write.csv(metadata, metadata_file, row.names = FALSE)
          
          incProgress(0.2)
        })
      },
      success_message = "Dimension analysis data downloaded successfully.",
      error_prefix = "Error downloading dimension analysis data:"
      )
    }
  )
}