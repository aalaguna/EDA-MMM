# =============================================================================
# Main server logic for the multivariate panel.
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
    kpi_normalization = "None",
    filtered_data = NULL,
    normalized_data = NULL,
    transformed_data = NULL,
    kpi_column = NULL
  )
  
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
  
  # Apply KPI normalization
  normalized_kpi_data <- reactive({
    req(base_filtered_data())
    df <- base_filtered_data()
    
    # If no KPI selected or normalization is None, return base data
    if (is.null(input$kpi_multi) || input$kpi_multi == "None" || input$kpi_multi == "N/A" || 
        multi_rv$kpi_normalization == "None") {
      multi_rv$normalized_data <- df
      return(df)
    }
    
    kpi_col <- input$kpi_multi
    
    # Check if KPI exists in the dataset
    if (!kpi_col %in% names(df)) {
      multi_rv$normalized_data <- df
      return(df)
    }
    
    # Apply normalization
    normalized_df <- df
    
    if (multi_rv$kpi_normalization == "Division") {
      normalized_df <- normalize_by_division(normalized_df, kpi_col)
    } else if (multi_rv$kpi_normalization == "Subtraction") {
      normalized_df <- normalize_by_subtraction(normalized_df, kpi_col)
    }
    
    multi_rv$normalized_data <- normalized_df
    return(normalized_df)
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
      multi_rv$transformed_data <- df_trans
      return(df_trans)
    } else {
      # sum_all_vars == "false" → do not transform variables, just return as-is
      multi_rv$transformed_data <- df
      return(df)
    }
  })
  
  # Get the appropriate KPI column name based on normalization
  get_kpi_column <- reactive({
    req(input$kpi_multi)
    
    if (input$kpi_multi == "None" || input$kpi_multi == "N/A") {
      return(NULL)
    }
    
    kpi_col <- input$kpi_multi
    normalization_type <- multi_rv$kpi_normalization
    
    if (normalization_type == "Division") {
      normalized_col <- paste0(kpi_col, "_norm_div")
      # Check if normalized column exists in the data
      if (normalized_col %in% names(transformed_data_multi())) {
        multi_rv$kpi_column <- normalized_col
        return(normalized_col)
      }
    } else if (normalization_type == "Subtraction") {
      normalized_col <- paste0(kpi_col, "_norm_sub")
      # Check if normalized column exists in the data
      if (normalized_col %in% names(transformed_data_multi())) {
        multi_rv$kpi_column <- normalized_col
        return(normalized_col)
      }
    }
    
    multi_rv$kpi_column <- kpi_col
    return(kpi_col)
  })
  
  # Output: Summed Variables Chart (Linear Flighting)
  output$sum_variables_chart <- renderPlotly({
    req(input$sum_all_vars == "true")
    render_sum_variables_chart(transformed_data_multi())
  })
  
  # Output: Transformed Summed Variables Chart
  # output$sum_variables_transf_chart <- renderPlotly({
  #   req(input$sum_all_vars == "true")
  #   render_transformed_variables_chart(transformed_data_multi())
  # })
  output$sum_variables_transf_chart <- renderPlotly({
    req(input$sum_all_vars == "true")
    
    render_transformed_sum_chart(
      df = transformed_data_multi(),
      kpi_multi = input$kpi_multi,
      transformation_multi = input$trans_var1,
      lag_multi = input$lag_multi,
      decay_multi = input$decay_multi,
      alpha_multi = input$alpha_multi,
      beta_multi = input$beta_multi,
      maxval_multi = input$maxval_multi,
      geography_multi = input$geography_multi
    )
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
    # Add KPI normalization radio buttons
    updateRadioButtons(session, "kpi_normalization_multi", 
                      choices = c("None", "Division", "Subtraction"),
                      selected = "None")
  })
  
  # Re-run data processing when normalization changes
  observeEvent(input$kpi_normalization_multi, {
    if (!is.null(input$kpi_multi) && input$kpi_multi != "None" && input$kpi_multi != "N/A") {
      normalized_kpi_data()
    }
  })
}