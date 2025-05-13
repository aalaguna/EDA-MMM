# =============================================================================
# Manages the univariate panel logic.
# =============================================================================

# -- AUXILIARY MODULES (load them here) --
source("R/modules/univariate/Boxplot.R", local = TRUE)
source("R/modules/univariate/Data_Filtering.R", local = TRUE)
source("R/modules/univariate/KPI_Variable_Correlation.R", local = TRUE)
source("R/modules/univariate/S_Curve_EDA.R", local = TRUE)
source("R/modules/univariate/Suggested_Max_Value.R", local = TRUE)
source("R/modules/univariate/Transformed_Variable.R", local = TRUE)
source("R/modules/univariate/Variable_Flighting.R", local = TRUE)

univariate_module_server <- function(input, output, session, rv) {
  # Server module for univariate analysis
  #
  # Args:
  #   input: Shiny input object
  #   output: Shiny output object
  #   session: Shiny session object
  #   rv: Shared reactive values
  
  # Local reactive values for this module
  univ_rv <- reactiveValues(
    kpi_normalization = "None",
    filtered_data = NULL,
    normalized_data = NULL
  )
  
  # Observer for the KPI normalization selection in univariate
  observeEvent(input$kpi_normalization_univ, {
    univ_rv$kpi_normalization <- input$kpi_normalization_univ
  }, ignoreInit = TRUE)
  
  # 1. Filter geography using *globally date-filtered data*
  #    (rv$filtered_data), not raw data rv$data:
  filtered_geography_data <- reactive({
    req(rv$filtered_data, input$geography_univ)
    df <- filter_geography_data(rv$filtered_data, input$geography_univ)
    univ_rv$filtered_data <- df
    return(df)
  })
  
  # Apply KPI normalization to the filtered data
  normalized_kpi_data <- reactive({
    req(filtered_geography_data(), input$kpi_univ)
    
    df <- filtered_geography_data()
    kpi_col <- input$kpi_univ
    
    if (!kpi_col %in% names(df)) {
      return(df)
    }
    
    normalization_type <- univ_rv$kpi_normalization
    
    if (normalization_type == "None") {
      univ_rv$normalized_data <- df
      return(df)
    }
    
    # Apply normalization to KPI
    processed_df <- df
    
    if (normalization_type == "Division") {
      processed_df <- normalize_by_division(processed_df, kpi_col)
    } else if (normalization_type == "Subtraction") {
      processed_df <- normalize_by_subtraction(processed_df, kpi_col)
    }
    
    univ_rv$normalized_data <- processed_df
    return(processed_df)
  })
  
  # Get the KPI column name to use based on normalization
  get_kpi_column <- reactive({
    req(input$kpi_univ)
    
    kpi_col <- input$kpi_univ
    normalization_type <- univ_rv$kpi_normalization
    
    if (normalization_type == "Division") {
      norm_col <- paste0(kpi_col, "_norm_div")
      if (norm_col %in% names(normalized_kpi_data())) {
        return(norm_col)
      }
    } else if (normalization_type == "Subtraction") {
      norm_col <- paste0(kpi_col, "_norm_sub")
      if (norm_col %in% names(normalized_kpi_data())) {
        return(norm_col)
      }
    }
    
    return(kpi_col)
  })
  
  # 2. Render main charts
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
  
  
  # output$var_transf_chart <- renderPlotly({
  #   req(filtered_geography_data(), input$kpi_univ, input$variable_univ, input$transformation_univ)
  #   validate(
  #     need(input$variable_univ != "N/A", "Please select a valid variable for transformation."),
  #     need(input$kpi_univ != "N/A", "Please select a valid KPI.")
  #   )
  #   render_transformation_chart(
  #     filtered_geography_data(),
  #     input$kpi_univ,                  
  #     input$variable_univ,
  #     input$transformation_univ,
  #     input$lag_univ,
  #     input$decay_univ,
  #     input$alpha_univ,
  #     input$beta_univ,
  #     input$maxval_univ,
  #     input$geography_univ
  #   )
  # })
  
  output$var_transf_chart <- renderPlotly({
    req(normalized_kpi_data(), input$kpi_univ, input$variable_univ, input$transformation_univ)
    validate(
      need(input$variable_univ != "N/A", "Please select a valid variable for transformation.")
    )
    
    kpi_col <- get_kpi_column()
    
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
  
  output$transformations_summary_univ <- renderPrint({
    req(input$transformation_univ, filtered_geography_data(), input$variable_univ)
    validate(
      need(input$variable_univ != "N/A", "Please select a valid variable for the summary.")
    )
    render_transformations_summary(filtered_geography_data(), input)
  })
  
  output$s_curve_univariate_plot <- renderPlotly({
    req(filtered_geography_data(), input$variable_univ)
    validate(
      need(input$variable_univ != "N/A", "Please select a valid variable for the S-Curve."),
      need(
        input$transformation_univ %in% c("S Origin", "S Shaped"),
        "This chart is only shown for 'S Origin' or 'S Shaped'."
      )
    )
    render_s_curve_plots(filtered_geography_data(), input)
  })
  
  output$corr_kpi_var_univ <- renderPlotly({
    req(normalized_kpi_data(), input$kpi_univ, input$variable_univ, input$transformation_univ)
    validate(
      need(input$variable_univ != "N/A", "Please select a valid variable to calculate correlation.")
    )
    
    # Pass the normalization type to the correlation function
    render_correlation_plot(
      df = normalized_kpi_data(),
      kpi_univ = input$kpi_univ,
      variable_univ = input$variable_univ,
      transformation_univ = input$transformation_univ,
      lag_univ = input$lag_univ,
      decay_univ = input$decay_univ,
      alpha_univ = input$alpha_univ,
      beta_univ = input$beta_univ,
      maxval_univ = input$maxval_univ,
      normalization_type = univ_rv$kpi_normalization
    )
  })
  
  # Update UI when data changes
  observe({
    req(rv$data)
    updateRadioButtons(session, "kpi_normalization_univ", 
                      choices = c("None", "Division", "Subtraction"),
                      selected = "None")
  })
  
  # Re-run correlations when normalization changes
  observeEvent(input$kpi_normalization_univ, {
    req(filtered_geography_data(), input$kpi_univ)
    normalized_kpi_data()
  })
}