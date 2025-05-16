# =============================================================================
# Manages the univariate panel logic with KPI transformation module.
# =============================================================================

# -- AUXILIARY MODULES (load them here) --
# Load all required modules for the univariate analysis
# Each module contains functions for specific analysis tasks
source("R/modules/univariate/Boxplot.R", local = TRUE)
source("R/modules/univariate/Data_Filtering.R", local = TRUE)
source("R/modules/univariate/KPI_Variable_Correlation.R", local = TRUE)
source("R/modules/univariate/Normalization_Helpers.R", local = TRUE)
source("R/modules/univariate/S_Curve_EDA.R", local = TRUE)
source("R/modules/univariate/Suggested_Max_Value.R", local = TRUE)
source("R/modules/univariate/Transformed_Variable.R", local = TRUE)
source("R/modules/univariate/Variable_Flighting.R", local = TRUE)

# Load KPI transformation functions
source("R/modules/kpi/KPI_Transformation_Functions.R", local = TRUE)

#' Server module for univariate analysis
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param rv Shared reactive values from the main application
#' @return None (side effects only)
univariate_module_server <- function(input, output, session, rv) {
  # Server module for univariate analysis
  
  # Local reactive values for this module
  univ_rv <- reactiveValues(
    kpi_transformation = "None",     # Type of transformation to apply to KPI
    kpi_normalization = "None",       # Type of normalization to apply to KPI
    filtered_data = NULL,             # Data after geography filtering
    transformed_data = NULL,          # Data after KPI transformation
    normalized_data = NULL,           # Data after KPI normalization
    transformation_cache = list()      # Cache for transformed variables to improve performance
  )
  
  # Observer for the KPI transformation selection in univariate
  observeEvent(input$kpi_transformation_univ, {
    univ_rv$kpi_transformation <- input$kpi_transformation_univ
    # Clear transformation cache when transformation changes
    univ_rv$transformation_cache <- list()
  }, ignoreInit = TRUE)
  
  # Observer for the KPI normalization selection in univariate
  observeEvent(input$kpi_normalization_univ, {
    univ_rv$kpi_normalization <- input$kpi_normalization_univ
    # Clear transformation cache when normalization changes
    univ_rv$transformation_cache <- list()
  }, ignoreInit = TRUE)
  
  # 1. Filter geography using *globally date-filtered data* (rv$filtered_data), not raw data rv$data
  filtered_geography_data <- reactive({
    req(rv$filtered_data, input$geography_univ)
    df <- filter_geography_data(rv$filtered_data, input$geography_univ)
    univ_rv$filtered_data <- df
    return(df)
  })
  
  # Apply KPI transformation to the filtered data
  transformed_kpi_data <- reactive({
    req(filtered_geography_data(), input$kpi_univ)
    
    df <- filtered_geography_data()
    kpi_col <- input$kpi_univ
    
    if (!kpi_col %in% names(df)) {
      warning(paste("KPI column", kpi_col, "not found in data"))
      univ_rv$transformed_data <- df
      return(df)
    }
    
    # Get the transformation method
    transformation_type <- univ_rv$kpi_transformation
    
    if (transformation_type == "None") {
      univ_rv$transformed_data <- df
      return(df)
    }
    
    # Get transformation parameters
    ma_window <- if (transformation_type == "Moving Average") {
      as.integer(input$ma_window_univ)
    } else {
      3 # Default value
    }
    
    # Get lag and decay parameters
    lag_value <- as.integer(input$lag_kpi_univ)
    decay_rate <- as.numeric(input$decay_kpi_univ)
    
    # Validate parameters
    if (is.na(ma_window) || ma_window < 2) ma_window <- 3
    if (is.na(lag_value) || lag_value < 0) lag_value <- 0
    if (is.na(decay_rate) || decay_rate < 0 || decay_rate > 1) decay_rate <- 1
    
    # Apply transformation using the KPI transformation functions
    # This will apply the transformation, then lag, then decay (in that order)
    result <- apply_full_kpi_processing(
      data = df,
      kpi_column = kpi_col,
      transformation = transformation_type,
      normalization = "None", # Will apply normalization separately
      ma_window = ma_window,
      lag_value = lag_value,
      decay_rate = decay_rate
    )
    
    # Store the transformed data
    univ_rv$transformed_data <- result$data
    
    return(result$data)
  })
  
  # Apply KPI normalization to the transformed data
  normalized_kpi_data <- reactive({
    req(transformed_kpi_data(), input$kpi_univ)
    
    df <- transformed_kpi_data()
    kpi_col <- input$kpi_univ
    
    # Get the normalization method
    normalization_type <- univ_rv$kpi_normalization
    
    if (normalization_type == "None") {
      univ_rv$normalized_data <- df
      return(df)
    }
    
    # Get the transformed column name based on the transformation pipeline
    transformed_col <- get_processed_column_name(
      kpi_column = kpi_col,
      transformation = univ_rv$kpi_transformation,
      normalization = "None", # Not yet normalized
      ma_window = if (univ_rv$kpi_transformation == "Moving Average") as.integer(input$ma_window_univ) else 3,
      lag_value = as.integer(input$lag_kpi_univ),
      decay_rate = as.numeric(input$decay_kpi_univ)
    )
    
    # Check if the transformed column exists
    if (!transformed_col %in% names(df)) {
      warning(paste("Transformed KPI column", transformed_col, "not found in data"))
      # Fall back to the original KPI column
      transformed_col <- kpi_col
    }
    
    # Apply normalization to the transformed KPI
    processed_df <- normalize_kpi(df, transformed_col, normalization_type)
    
    univ_rv$normalized_data <- processed_df
    return(processed_df)
  })
  
  # Get the KPI column name to use based on transformation and normalization
  get_kpi_column <- reactive({
    req(input$kpi_univ)
    
    kpi_col <- input$kpi_univ
    transformation_type <- univ_rv$kpi_transformation
    normalization_type <- univ_rv$kpi_normalization
    
    # Get the final column name based on the full pipeline
    final_col <- get_processed_column_name(
      kpi_column = kpi_col,
      transformation = transformation_type,
      normalization = normalization_type,
      ma_window = if (transformation_type == "Moving Average") as.integer(input$ma_window_univ) else 3,
      lag_value = as.integer(input$lag_kpi_univ),
      decay_rate = as.numeric(input$decay_kpi_univ)
    )
    
    # Verify that the column exists in the normalized data
    if (!final_col %in% names(normalized_kpi_data())) {
      warning(paste("Final KPI column", final_col, "not found, using original KPI column"))
      return(kpi_col)
    }
    
    return(final_col)
  })
  
  # 2. Render main charts
  
  # Variable Flighting Chart
  output$variable_flighting_chart <- renderPlotly({
    req(normalized_kpi_data(), input$kpi_univ, input$variable_univ)
    validate(
      need(input$variable_univ != "N/A", "Please select a valid variable for analysis.")
    )
    
    kpi_col <- get_kpi_column()
    
    render_variable_flighting(
      normalized_kpi_data(),
      kpi_col,
      input$variable_univ,
      input$geography_univ
    )
  })
  
  # Transformed Variable Chart
  output$var_transf_chart <- renderPlotly({
    req(normalized_kpi_data(), input$kpi_univ, input$variable_univ, input$transformation_univ)
    validate(
      need(input$variable_univ != "N/A", "Please select a valid variable for transformation.")
    )
    
    kpi_col <- get_kpi_column()
    
    # Try to use transformation cache for better performance
    # Create a unique key for this transformation configuration
    cache_key <- paste(
      kpi_col, input$variable_univ, input$transformation_univ, 
      input$lag_univ, input$decay_univ, input$alpha_univ, input$beta_univ, 
      input$maxval_univ, sep = "_"
    )
    
    withProgress(message = 'Generating transformation...', value = 0, {
      render_transformation_chart(
        df = normalized_kpi_data(),
        kpi_univ = kpi_col,
        variable_univ = input$variable_univ,
        transformation_univ = input$transformation_univ,
        lag_univ = input$lag_univ,
        decay_univ = input$decay_univ,
        alpha_univ = input$alpha_univ,
        beta_univ = input$beta_univ,
        maxval_univ = input$maxval_univ,
        geography_univ = input$geography_univ
      )
    })
  })
  
  # Boxplot
  output$boxplot_univ <- renderPlot({
    req(filtered_geography_data(), input$variable_univ)
    validate(
      need(input$variable_univ != "N/A", "Please select a valid variable for the boxplot.")
    )
    render_boxplot_univariate(
      filtered_geography_data(),
      input$variable_univ,
      input$geography_univ
    )
  })
  
  # Transformation summary info
  output$transformations_summary_univ <- renderPrint({
    req(input$transformation_univ, filtered_geography_data(), input$variable_univ)
    validate(
      need(input$variable_univ != "N/A", "Please select a valid variable for the summary.")
    )
    render_transformations_summary(filtered_geography_data(), input)
  })
  
  # S-Curve plot (only shown for S Origin or S Shaped transformations)
  output$s_curve_univariate_plot <- renderPlotly({
    req(filtered_geography_data(), input$variable_univ)
    validate(
      need(input$variable_univ != "N/A", "Please select a valid variable for the S-Curve."),
      need(
        input$transformation_univ %in% c("S Origin", "S Shaped"),
        "This chart is only shown for 'S Origin' or 'S Shaped'."
      )
    )
    withProgress(message = 'Generating S-Curve...', value = 0, {
      render_s_curve_plots(filtered_geography_data(), input)
    })
  })
  
  # KPI vs. Variable correlation scatter plot
  output$corr_kpi_var_univ <- renderPlotly({
    req(normalized_kpi_data(), input$kpi_univ, input$variable_univ, input$transformation_univ)
    validate(
      need(input$variable_univ != "N/A", "Please select a valid variable to calculate correlation.")
    )
    
    # Get the actual KPI column name (transformed and/or normalized)
    kpi_col <- get_kpi_column()
    
    # Pass the normalization type to the correlation function
    withProgress(message = 'Calculating correlation...', value = 0, {
      render_correlation_plot(
        df = normalized_kpi_data(),
        kpi_univ = kpi_col,
        variable_univ = input$variable_univ,
        transformation_univ = input$transformation_univ,
        lag_univ = input$lag_univ,
        decay_univ = input$decay_univ,
        alpha_univ = input$alpha_univ,
        beta_univ = input$beta_univ,
        maxval_univ = input$maxval_univ,
        normalization_type = "None" # Already normalized in the pipeline
      )
    })
  })
  
  # Update UI when data changes
  observe({
    req(rv$data)
    
    # Update the KPI transformation radio buttons
    updateRadioButtons(session, "kpi_transformation_univ", 
                      choices = c("None", "Logarithmic", "Moving Average"),
                      selected = univ_rv$kpi_transformation)
    
    # Update the KPI normalization radio buttons
    updateRadioButtons(session, "kpi_normalization_univ", 
                      choices = c("None", "Division", "Subtraction"),
                      selected = univ_rv$kpi_normalization)
    
    # Update transformation options based on variable characteristics
    var_choices <- names(rv$data)
    numeric_cols <- var_choices[sapply(rv$data, is.numeric)]
    
    # Update variable choices
    updateSelectInput(session, "variable_univ", 
                     choices = c("Select" = "N/A", numeric_cols),
                     selected = input$variable_univ)
    
    # Update KPI choices
    updateSelectInput(session, "kpi_univ", 
                     choices = c("Select" = "N/A", numeric_cols),
                     selected = input$kpi_univ)
  })
  
  # Re-run transformations when parameters change
  observeEvent(input$kpi_transformation_univ, {
    transformed_kpi_data()
  })
  
  # Re-run normalizations when normalization changes
  observeEvent(input$kpi_normalization_univ, {
    normalized_kpi_data()
  })
  
  # Observer for change in transformation parameters
  observe({
    # React to KPI transformation parameters
    input$ma_window_univ
    input$lag_kpi_univ
    input$decay_kpi_univ
    
    # React to variable transformation parameters
    input$transformation_univ
    input$lag_univ
    input$decay_univ
    input$alpha_univ
    input$beta_univ
    input$maxval_univ
    
    # Validate KPI transformation parameters
    if (!is.null(input$ma_window_univ) && (is.na(input$ma_window_univ) || input$ma_window_univ < 2)) {
      updateSliderInput(session, "ma_window_univ", value = 3)
    }
    
    if (!is.null(input$lag_kpi_univ) && (is.na(input$lag_kpi_univ) || input$lag_kpi_univ < 0)) {
      updateNumericInput(session, "lag_kpi_univ", value = 0)
    }
    
    if (!is.null(input$decay_kpi_univ) && (is.na(input$decay_kpi_univ) || input$decay_kpi_univ < 0 || input$decay_kpi_univ > 1)) {
      updateNumericInput(session, "decay_kpi_univ", value = 1)
    }
    
    # Validate variable transformation parameters
    if (!is.null(input$alpha_univ) && (is.na(input$alpha_univ) || input$alpha_univ <= 0)) {
      updateNumericInput(session, "alpha_univ", value = 1)
    }
    
    if (!is.null(input$beta_univ) && (is.na(input$beta_univ) || input$beta_univ <= 0)) {
      updateNumericInput(session, "beta_univ", value = 1)
    }
    
    if (!is.null(input$maxval_univ) && (is.na(input$maxval_univ) || input$maxval_univ <= 0)) {
      updateNumericInput(session, "maxval_univ", value = 100)
    }
    
    if (!is.null(input$decay_univ) && (is.na(input$decay_univ) || input$decay_univ <= 0)) {
      updateNumericInput(session, "decay_univ", value = 1)
    }
    
    if (!is.null(input$lag_univ) && (is.na(input$lag_univ) || input$lag_univ < 0)) {
      updateNumericInput(session, "lag_univ", value = 0)
    }
  })
  
  # Handle download of transformed data
  output$download_univariate <- downloadHandler(
    filename = function() {
      paste("transformed_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      # Ensure we have all required data
      tryCatch({
        req(normalized_kpi_data(), input$kpi_univ, input$variable_univ)
        
        # Get the transformed data
        df <- normalized_kpi_data()
        kpi_col <- get_kpi_column()
        
        # Extract the original variable data
        var_data <- df[[input$variable_univ]]
        
        # Apply transformation
        transformed_data <- apply_transformation(
          var_data,
          type = input$transformation_univ,
          alpha = input$alpha_univ,
          beta = input$beta_univ, 
          maxval = input$maxval_univ,
          decay = input$decay_univ,
          lag = input$lag_univ
        )
        
        # Create a download dataframe with date, KPI, original variable, and transformed variable
        date_col <- if ("Period" %in% names(df)) "Period" else if ("periodo" %in% names(df)) "periodo" else NULL
        
        if (is.null(date_col)) {
          download_df <- data.frame(
            KPI = df[[kpi_col]],
            Original = var_data,
            Transformed = transformed_data
          )
        } else {
          download_df <- data.frame(
            Date = df[[date_col]],
            KPI = df[[kpi_col]],
            Original = var_data,
            Transformed = transformed_data
          )
        }
        
        # Write to CSV
        write.csv(download_df, file, row.names = FALSE)
      }, error = function(e) {
        # Write error message to a CSV if something goes wrong
        error_df <- data.frame(Error = paste("Error generating download:", e$message))
        write.csv(error_df, file, row.names = FALSE)
      })
    }
  )
  
  # Return the module server for potential use by the main server
  return(list(
    get_kpi_column = get_kpi_column,
    normalized_data = normalized_kpi_data
  ))
}