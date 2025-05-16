# =============================================================================
# Main server logic for the multivariate panel with KPI transformation.
# =============================================================================

# Additional submodules for plots and transformations
source("R/modules/multivariate/Boxplot.R", local = TRUE)
source("R/modules/multivariate/Correlation_KPI.R", local = TRUE)
source("R/modules/multivariate/Correlation_Matrix.R", local = TRUE)
source("R/modules/multivariate/Data_Processing.R", local = TRUE)
source("R/modules/multivariate/Line_Selected_Variables.R", local = TRUE)
source("R/modules/multivariate/S_Curve_EDA.R", local = TRUE)
source("R/modules/multivariate/Summed_Variables.R", local = TRUE)
source("R/modules/multivariate/Transformed_Summed_Variables.R", local = TRUE)
source("R/modules/multivariate/Suggested_Max_Value.R", local = TRUE)

# Load KPI transformation functions
source("R/modules/kpi/KPI_Transformation_Functions.R", local = TRUE)

multivariate_module_server <- function(input, output, session, rv) {
  # Server module for multivariate analysis
  #
  # Args:
  #   input: Shiny input object
  #   output: Shiny output object
  #   session: Shiny session object
  #   rv: Shared reactive values
  
  # Local reactive values for multivariate module
  multi_rv <- reactiveValues(
    kpi_transformation = "None",     # Type of transformation to apply to KPI
    kpi_normalization = "None",       # Type of normalization to apply to KPI
    filtered_data = NULL,             # Data after filtering
    transformed_data = NULL,          # Data after KPI transformation
    normalized_data = NULL,           # Data after KPI normalization
    final_processed_data = NULL       # Final data after all transformations
  )
  
  # Observer for the KPI transformation selection in multivariate
  observeEvent(input$kpi_transformation_multi, {
    multi_rv$kpi_transformation <- input$kpi_transformation_multi
  }, ignoreInit = TRUE)
  
  # Observer for the KPI normalization selection in multivariate
  observeEvent(input$kpi_normalization_multi, {
    multi_rv$kpi_normalization <- input$kpi_normalization_multi
  }, ignoreInit = TRUE)
  
  # Initialize date range inputs when data is loaded
  observe({
    req(rv$filtered_data)
    # Detect date column
    date_col <- if ("Period" %in% names(rv$filtered_data)) "Period" else if ("periodo" %in% names(rv$filtered_data)) "periodo" else NULL
    if (!is.null(date_col)) {
      date_values <- as.Date(rv$filtered_data[[date_col]])
      date_values <- date_values[!is.na(date_values)]
      if (length(date_values) > 0) {
        min_date <- min(date_values, na.rm = TRUE)
        max_date <- max(date_values, na.rm = TRUE)
        updateDateRangeInput(session, "date_range_multi", 
                             start = min_date,
                             end = max_date)
      }
    }
  })
  
  # Reactive dataframe filtered by date and geography
  base_filtered_data <- reactive({
    req(rv$filtered_data, input$geography_multi)
    df <- rv$filtered_data
    date_col <- rv$date_col
    req(date_col)
    
    # Filter by date range if available
    if (!is.null(input$date_range_multi)) {
      df[[date_col]] <- as.Date(df[[date_col]])
      df <- df[df[[date_col]] >= input$date_range_multi[1] & 
                 df[[date_col]] <= input$date_range_multi[2], ]
    }
    
    # Filter by geography
    if (input$geography_multi != "Total" && input$geography_multi != "N/A") {
      geo_col <- if ("Geography" %in% names(df)) "Geography" else if ("Geografia" %in% names(df)) "Geografia" else NULL
      if (!is.null(geo_col)) {
        df <- df %>% filter(.data[[geo_col]] == input$geography_multi)
      }
    } else if (input$geography_multi == "Total") {
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      df[[date_col]] <- as.Date(df[[date_col]])
      df <- df %>%
        group_by(across(all_of(date_col))) %>%
        summarise(across(all_of(numeric_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
    }
    
    multi_rv$filtered_data <- df
    return(df)
  })
  
  # Apply KPI transformation
  transformed_kpi_data <- reactive({
    req(base_filtered_data())
    df <- base_filtered_data()
    
    # If no KPI selected or transformation is None, return base data
    if (is.null(input$kpi_multi) || input$kpi_multi == "None" || input$kpi_multi == "N/A" || 
        multi_rv$kpi_transformation == "None") {
      multi_rv$transformed_data <- df
      return(df)
    }
    
    kpi_col <- input$kpi_multi
    
    # Check if KPI exists in the dataset
    if (!kpi_col %in% names(df)) {
      multi_rv$transformed_data <- df
      return(df)
    }
    
    # Get transformation parameters
    transformation_type <- multi_rv$kpi_transformation
    ma_window <- if (transformation_type == "Moving Average") {
      as.integer(input$ma_window_multi)
    } else {
      3 # Default value
    }
    
    # Get lag and decay parameters
    lag_value <- as.integer(input$lag_kpi_multi)
    decay_rate <- as.numeric(input$decay_kpi_multi)
    
    # Validate parameters
    if (is.na(ma_window) || ma_window < 2) ma_window <- 3
    if (is.na(lag_value) || lag_value < 0) lag_value <- 0
    if (is.na(decay_rate) || decay_rate < 0 || decay_rate > 1) decay_rate <- 1
    
    # Apply transformation using the KPI transformation functions
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
    multi_rv$transformed_data <- result$data
    
    return(result$data)
  })
  
  # Apply KPI normalization
  normalized_kpi_data <- reactive({
    req(transformed_kpi_data())
    df <- transformed_kpi_data()
    
    # If no KPI selected or normalization is None, return transformed data
    if (is.null(input$kpi_multi) || input$kpi_multi == "None" || input$kpi_multi == "N/A" || 
        multi_rv$kpi_normalization == "None") {
      multi_rv$normalized_data <- df
      return(df)
    }
    
    kpi_col <- input$kpi_multi
    
    # Get the transformed column name based on the transformation pipeline
    transformed_col <- get_processed_column_name(
      kpi_column = kpi_col,
      transformation = multi_rv$kpi_transformation,
      normalization = "None", # Not yet normalized
      ma_window = if (multi_rv$kpi_transformation == "Moving Average") as.integer(input$ma_window_multi) else 3,
      lag_value = as.integer(input$lag_kpi_multi),
      decay_rate = as.numeric(input$decay_kpi_multi)
    )
    
    # Check if the transformed column exists
    if (!transformed_col %in% names(df)) {
      warning(paste("Transformed KPI column", transformed_col, "not found in data"))
      # Fall back to the original KPI column
      transformed_col <- kpi_col
    }
    
    # Apply normalization to the transformed KPI
    normalization_type <- multi_rv$kpi_normalization
    processed_df <- normalize_kpi(df, transformed_col, normalization_type)
    
    multi_rv$normalized_data <- processed_df
    return(processed_df)
  })
  
  # Get the appropriate KPI column name based on normalization
  get_kpi_column <- reactive({
    req(input$kpi_multi)
    
    if (input$kpi_multi == "None" || input$kpi_multi == "N/A") {
      return(NULL)
    }
    
    kpi_col <- input$kpi_multi
    transformation_type <- multi_rv$kpi_transformation
    normalization_type <- multi_rv$kpi_normalization
    
    # Get the final column name based on the full pipeline
    final_col <- get_processed_column_name(
      kpi_column = kpi_col,
      transformation = transformation_type,
      normalization = normalization_type,
      ma_window = if (transformation_type == "Moving Average") as.integer(input$ma_window_multi) else 3,
      lag_value = as.integer(input$lag_kpi_multi),
      decay_rate = as.numeric(input$decay_kpi_multi)
    )
    
    # Verify that the column exists in the normalized data
    if (!final_col %in% names(normalized_kpi_data())) {
      warning(paste("Final KPI column", final_col, "not found, using original KPI column"))
      multi_rv$kpi_column <- kpi_col
      return(kpi_col)
    }
    
    multi_rv$kpi_column <- final_col
    return(final_col)
  })
  
  # Single reactive that returns transformed data:
  # If sum_all_vars == TRUE → sum + transform
  # If sum_all_vars == FALSE → return raw data without transformation
  transformed_data_multi <- reactive({
    df <- normalized_kpi_data()
    req(df)
    
    if (!is.null(input$sum_all_vars) && input$sum_all_vars == "true") {
      # Step 1: Sum variables
      df_summed <- process_summed_data(df, input)
      # Step 2: Transform the sum
      df_trans <- process_transformed_data(df_summed, input)
      multi_rv$final_processed_data <- df_trans
      return(df_trans)
    } else {
      # sum_all_vars == "false" → do not transform variables, just return as-is
      multi_rv$final_processed_data <- df
      return(df)
    }
  })
  
  # Output: Summed Variables Chart (Linear Flighting)
  output$sum_variables_chart <- renderPlotly({
    req(input$sum_all_vars == "true")
    render_sum_variables_chart(transformed_data_multi())
  })
  
  # Output: Transformed Summed Variables Chart
  output$sum_variables_transf_chart <- renderPlotly({
    req(input$sum_all_vars == "true")
    render_transformed_variables_chart(transformed_data_multi())
  })
  
  # Output: S-Curve EDA
  output$s_curve_multivariate_plot <- renderPlotly({
    req(input$sum_all_vars == "true")
    render_s_curve_multi(transformed_data_multi(), input)
  })
  
  # Output: Boxplot of Summed Variable
  output$boxplot_multi_sum <- renderPlotly({
    req(input$sum_all_vars == "true")
    render_summed_boxplot(transformed_data_multi())
  })
  
  # Output: Suggested Max Value
  output$transformations_summary_multi <- renderPrint({
    req(input$sum_all_vars == "true")
    render_transformations_summary_multi(base_filtered_data(), input)
  })
  
  # Output: Correlation with KPI (for Summed approach)
  output$corr_with_kpi_multi_sum <- renderPlotly({
    req(input$sum_all_vars == "true", 
        input$kpi_multi != "None", 
        input$kpi_multi != "N/A")
    
    kpi_col <- get_kpi_column()
    
    # Check if KPI column exists in the data
    if (is.null(kpi_col) || !kpi_col %in% names(transformed_data_multi())) {
      return(plot_ly() %>% 
               layout(title = "Please select a valid KPI",
                      xaxis = list(showticklabels = FALSE),
                      yaxis = list(showticklabels = FALSE)))
    }
    
    render_kpi_correlation(transformed_data_multi(), kpi_col)
  })
  
  # If user chooses not to sum:
  # Show individual transformed variables + correlation matrix
  output$boxplot_multi <- renderPlotly({
    req(input$sum_all_vars == "false")
    render_individual_boxplots(normalized_kpi_data(), input)
  })
  
  output$corr_matrix_multi <- renderPlotly({
    req(input$sum_all_vars == "false")
    render_correlation_matrix(normalized_kpi_data(), input)
  })
  
  # Update UI when data changes
  observe({
    req(rv$data)
    # Update KPI transformation radio buttons
    updateRadioButtons(session, "kpi_transformation_multi", 
                      choices = c("None", "Logarithmic", "Moving Average"),
                      selected = multi_rv$kpi_transformation)
    
    # Update KPI normalization radio buttons
    updateRadioButtons(session, "kpi_normalization_multi", 
                      choices = c("None", "Division", "Subtraction"),
                      selected = multi_rv$kpi_normalization)
  })
  
  # Re-run data processing when normalization changes
  observeEvent(input$kpi_normalization_multi, {
    if (!is.null(input$kpi_multi) && input$kpi_multi != "None" && input$kpi_multi != "N/A") {
      normalized_kpi_data()
    }
  })
  
  # Re-run data processing when transformation changes
  observeEvent(input$kpi_transformation_multi, {
    if (!is.null(input$kpi_multi) && input$kpi_multi != "None" && input$kpi_multi != "N/A") {
      transformed_kpi_data()
    }
  })
  
  # Observer for parameter validation
  observe({
    # React to KPI transformation parameters
    input$ma_window_multi
    input$lag_kpi_multi
    input$decay_kpi_multi
    
    # Validate KPI transformation parameters
    if (!is.null(input$ma_window_multi) && (is.na(input$ma_window_multi) || input$ma_window_multi < 2)) {
      updateSliderInput(session, "ma_window_multi", value = 3)
    }
    
    if (!is.null(input$lag_kpi_multi) && (is.na(input$lag_kpi_multi) || input$lag_kpi_multi < 0)) {
      updateNumericInput(session, "lag_kpi_multi", value = 0)
    }

    if (!is.null(input$decay_kpi_multi) && (is.na(input$decay_kpi_multi) || input$decay_kpi_multi < 0 || input$decay_kpi_multi > 1)) {
      updateNumericInput(session, "decay_kpi_multi", value = 1)
    }
  })
}