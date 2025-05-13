# =============================================================================
# Main server logic for the KPI transformation panel.
# =============================================================================

# Load helper functions and download handlers
source("R/modules/kpi_transformation/KPI_Transformation_Functions.R", local = TRUE)
source("R/modules/kpi_transformation/KPI_Download_Handler.R", local = TRUE)

kpi_transformation_module_server <- function(input, output, session, rv) {
  # Server module for KPI transformation and normalization
  #
  # Args:
  #   input: Shiny input object
  #   output: Shiny output object
  #   session: Shiny session object
  #   rv: Shared reactive values
  
  # Reactive values for KPI transformation module
  kpi_module_rv <- reactiveValues(
    filtered_data = NULL,
    transformed_data = NULL,
    dimension_data = NULL,
    processed_column = NULL
  )
  
  # Setup reactive for date-filtered data
  observe({
    req(rv$data, input$date_range_kpi)
    df <- rv$data
    
    # Make sure date column is detected
    date_col <- rv$date_col
    if (!is.null(date_col) && date_col %in% names(df)) {
      # Ensure column is Date type
      df[[date_col]] <- as.Date(df[[date_col]])
      # Filter by date range
      df <- df[df[[date_col]] >= input$date_range_kpi[1] & 
                 df[[date_col]] <= input$date_range_kpi[2], ]
    }
    
    # Store filtered data in reactive value for sharing across functions
    kpi_module_rv$date_filtered <- df
  })
  
  # Setup reactive for filtered data (geography, product, etc.)
  filtered_data <- reactive({
    req(kpi_module_rv$date_filtered)
    df <- kpi_module_rv$date_filtered
    
    # Apply geography filter if selected
    if (!is.null(input$geography_kpi) && input$geography_kpi != "N/A") {
      geo_col <- if ("Geography" %in% names(df)) "Geography" else if ("Geografia" %in% names(df)) "Geografia" else NULL
      if (!is.null(geo_col) && input$geography_kpi != "Total") {
        df <- df %>% filter(.data[[geo_col]] == input$geography_kpi)
      } else if (input$geography_kpi == "Total") {
        # Handle "Total" by aggregating across all geographies
        date_col <- rv$date_col
        if (!is.null(date_col)) {
          numeric_cols <- names(df)[sapply(df, is.numeric)]
          df <- df %>%
            group_by(across(all_of(date_col))) %>%
            summarise(across(all_of(numeric_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
        }
      }
    }
    
    # Apply product filter if selected
    if (!is.null(input$product_kpi) && input$product_kpi != "N/A") {
      prod_col <- if ("Product" %in% names(df)) "Product" else if ("Producto" %in% names(df)) "Producto" else NULL
      if (!is.null(prod_col)) {
        df <- df %>% filter(.data[[prod_col]] == input$product_kpi)
      }
    }
    
    # Apply campaign filter if selected
    if (!is.null(input$campaign_kpi) && input$campaign_kpi != "N/A") {
      camp_col <- if ("Campaign" %in% names(df)) "Campaign" else if ("Campaña" %in% names(df)) "Campaña" else NULL
      if (!is.null(camp_col)) {
        df <- df %>% filter(.data[[camp_col]] == input$campaign_kpi)
      }
    }
    
    # Apply outlet filter if selected
    if (!is.null(input$outlet_kpi) && input$outlet_kpi != "N/A") {
      outlet_col <- if ("Outlet" %in% names(df)) "Outlet" else NULL
      if (!is.null(outlet_col)) {
        df <- df %>% filter(.data[[outlet_col]] == input$outlet_kpi)
      }
    }
    
    # Apply creative filter if selected
    if (!is.null(input$creative_kpi) && input$creative_kpi != "N/A") {
      creative_col <- if ("Creative" %in% names(df)) "Creative" else if ("Creativo" %in% names(df)) "Creativo" else NULL
      if (!is.null(creative_col)) {
        df <- df %>% filter(.data[[creative_col]] == input$creative_kpi)
      }
    }
    
    kpi_module_rv$filtered_data <- df
    return(df)
  })
  
  # Apply transformations and normalizations
  transformed_data <- reactive({
    req(filtered_data(), input$kpi_selection)
    
    # Get filtered data
    data <- filtered_data()
    kpi_col <- input$kpi_selection
    
    # Check if KPI column exists
    if (!kpi_col %in% names(data)) {
      notifyUser(paste("KPI column", kpi_col, "not found in the data."), "error")
      return(data)
    }
    
    # Apply full transformation and normalization pipeline
    processed_data <- data
    
    # Apply transformation if selected
    if (input$transformation_kpi == "Logarithmic") {
      processed_data <- apply_log_transformation(processed_data, kpi_col)
      transformed_col <- paste0(kpi_col, "_log")
    } else if (input$transformation_kpi == "Moving Average") {
      window_size <- input$ma_window
      processed_data <- apply_moving_average(processed_data, kpi_col, window_size)
      transformed_col <- paste0(kpi_col, "_ma", window_size)
    } else {
      # No transformation
      transformed_col <- kpi_col
    }
    
    # Apply normalization if selected
    if (input$normalization_kpi == "Division by Mean") {
      processed_data <- normalize_by_division(processed_data, kpi_col, transformed_col)
      normalized_col <- paste0(transformed_col, "_norm_div")
    } else if (input$normalization_kpi == "Subtraction of Mean") {
      processed_data <- normalize_by_subtraction(processed_data, kpi_col, transformed_col)
      normalized_col <- paste0(transformed_col, "_norm_sub")
    } else {
      normalized_col <- transformed_col
    }
    
    # Store processed column name
    kpi_module_rv$processed_column <- normalized_col
    kpi_module_rv$transformed_data <- processed_data
    
    return(processed_data)
  })
  
  # Data for dimension analysis
  dimension_data <- reactive({
    req(transformed_data(), input$dimension_select, kpi_module_rv$processed_column)
    
    df <- transformed_data()
    dimension <- input$dimension_select
    processed_col <- kpi_module_rv$processed_column
    
    # Check if dimension column exists
    if (!dimension %in% names(df)) {
      return(NULL)
    }
    
    # Check if processed column exists
    if (!processed_col %in% names(df)) {
      return(NULL)
    }
    
    # Aggregate data by dimension
    result <- df %>%
      group_by(across(all_of(dimension))) %>%
      summarise(
        Total = sum(.data[[processed_col]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # Calculate percentage
      mutate(
        Percentage = Total / sum(Total, na.rm = TRUE) * 100
      ) %>%
      # Sort by Total descending
      arrange(desc(Total))
    
    kpi_module_rv$dimension_data <- result
    return(result)
  })
  
  # Plot 1: KPI Time Series (original data)
  output$kpi_time_series <- renderPlotly({
    req(filtered_data(), input$kpi_selection)
    
    df <- filtered_data()
    kpi_col <- input$kpi_selection
    
    # Check if KPI column exists
    validate(
      need(kpi_col %in% names(df), "KPI column not found in the data.")
    )
    
    # Get date column
    date_col <- if ("Period" %in% names(df)) "Period" else "periodo"
    req(date_col)
    
    # Prepare data for plotting
    df <- df %>%
      select(date = !!sym(date_col), kpi = !!sym(kpi_col)) %>%
      filter(!is.na(kpi)) %>%
      arrange(date)
    
    # Plot time series
    p <- plot_ly(
      df,
      x = ~date,
      y = ~kpi,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'blue'),
      marker = list(color = 'blue'),
      name = "Original KPI",
      hovertemplate = paste(
        "Date: %{x|%Y-%m-%d}<br>",
        "KPI: %{y:.2f}<extra></extra>"
      )
    ) %>%
      layout(
        title = paste("KPI Time Series:", kpi_col),
        xaxis = list(
          title = list(text = "Date", standoff = 25),
          type = "date",
          tickformat = "%Y-%m",
          dtick = "M3",
          tickangle = -45,
          automargin = TRUE
        ),
        yaxis = list(
          title = list(text = "Value", standoff = 20),
          automargin = TRUE
        ),
        margin = list(l = 70, r = 40, t = 60, b = 80),
        showlegend = TRUE
      )
    
    return(p)
  })
  
  # Plot 2: Transformed KPI
  output$transformed_kpi_plot <- renderPlotly({
    req(transformed_data(), input$kpi_selection)
    
    df <- transformed_data()
    kpi_col <- input$kpi_selection
    
    # Skip if no transformation selected
    if (input$transformation_kpi == "None") {
      return(plot_ly() %>% 
               layout(title = "No transformation selected",
                      xaxis = list(showticklabels = FALSE),
                      yaxis = list(showticklabels = FALSE)))
    }
    
    # Determine transformed column name
    transformed_col <- if (input$transformation_kpi == "Logarithmic") {
      paste0(kpi_col, "_log")
    } else if (input$transformation_kpi == "Moving Average") {
      paste0(kpi_col, "_ma", input$ma_window)
    } else {
      kpi_col
    }
    
    # Check if transformed column exists
    validate(
      need(transformed_col %in% names(df), "Transformed column not found.")
    )
    
    # Get date column
    date_col <- if ("Period" %in% names(df)) "Period" else "periodo"
    req(date_col)
    
    # Prepare data for plotting
    plot_data <- df %>%
      select(date = !!sym(date_col), 
             original = !!sym(kpi_col), 
             transformed = !!sym(transformed_col)) %>%
      filter(!is.na(transformed)) %>%
      arrange(date)
    
    # Plot transformed data
    p <- plot_ly(plot_data, x = ~date) %>%
      add_trace(
        y = ~transformed,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = 'red'),
        marker = list(color = 'red'),
        name = input$transformation_kpi,
        hovertemplate = paste(
          "Date: %{x|%Y-%m-%d}<br>",
          "Transformed: %{y:.2f}<extra></extra>"
        )
      ) %>%
      layout(
        title = paste("Transformed KPI:", input$transformation_kpi),
        xaxis = list(
          title = list(text = "Date", standoff = 25),
          type = "date",
          tickformat = "%Y-%m",
          dtick = "M3",
          tickangle = -45,
          automargin = TRUE
        ),
        yaxis = list(
          title = list(text = "Value", standoff = 20),
          automargin = TRUE
        ),
        margin = list(l = 70, r = 40, t = 60, b = 80),
        showlegend = TRUE
      )
    
    return(p)
  })
  
  # Plot 3: Normalized KPI
  output$normalized_kpi_plot <- renderPlotly({
    req(transformed_data(), input$kpi_selection)
    
    df <- transformed_data()
    kpi_col <- input$kpi_selection
    
    # Skip if no normalization selected
    if (input$normalization_kpi == "None") {
      return(plot_ly() %>% 
               layout(title = "No normalization selected",
                      xaxis = list(showticklabels = FALSE),
                      yaxis = list(showticklabels = FALSE)))
    }
    
    # Determine transformed column name (to normalize)
    transformed_col <- if (input$transformation_kpi == "Logarithmic") {
      paste0(kpi_col, "_log")
    } else if (input$transformation_kpi == "Moving Average") {
      paste0(kpi_col, "_ma", input$ma_window)
    } else {
      kpi_col
    }
    
    # Determine normalized column name
    normalized_col <- if (input$normalization_kpi == "Division by Mean") {
      paste0(transformed_col, "_norm_div")
    } else if (input$normalization_kpi == "Subtraction of Mean") {
      paste0(transformed_col, "_norm_sub")
    } else {
      transformed_col
    }
    
    # Check if normalized column exists
    validate(
      need(normalized_col %in% names(df), "Normalized column not found.")
    )
    
    # Get date column
    date_col <- if ("Period" %in% names(df)) "Period" else "periodo"
    req(date_col)
    
    # Prepare data for plotting
    plot_data <- df %>%
      select(date = !!sym(date_col), 
             normalized = !!sym(normalized_col)) %>%
      filter(!is.na(normalized)) %>%
      arrange(date)
    
    # Plot normalized data
    p <- plot_ly(plot_data, x = ~date) %>%
      add_trace(
        y = ~normalized,
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = 'green'),
        marker = list(color = 'green'),
        name = input$normalization_kpi,
        hovertemplate = paste(
          "Date: %{x|%Y-%m-%d}<br>",
          "Normalized: %{y:.2f}<extra></extra>"
        )
      ) %>%
      layout(
        title = paste("Normalized KPI:", input$normalization_kpi),
        xaxis = list(
          title = list(text = "Date", standoff = 25),
          type = "date",
          tickformat = "%Y-%m",
          dtick = "M3",
          tickangle = -45,
          automargin = TRUE
        ),
        yaxis = list(
          title = list(text = "Value", standoff = 20),
          automargin = TRUE
        ),
        margin = list(l = 70, r = 40, t = 60, b = 80),
        showlegend = TRUE
      )
    
    return(p)
  })
  
  # Plot 4: KPI Distribution (histogram)
  output$kpi_distribution <- renderPlotly({
    req(filtered_data(), input$kpi_selection)
    
    df <- filtered_data()
    kpi_col <- input$kpi_selection
    
    # Check if KPI column exists
    validate(
      need(kpi_col %in% names(df), "KPI column not found in the data.")
    )
    
    # Extract KPI values, removing NAs
    kpi_values <- df[[kpi_col]]
    kpi_values <- kpi_values[!is.na(kpi_values)]
    
    # Skip if no valid data
    if (length(kpi_values) == 0) {
      return(plot_ly() %>% 
               layout(title = "No valid data for histogram",
                      xaxis = list(showticklabels = FALSE),
                      yaxis = list(showticklabels = FALSE)))
    }
    
    # Create histogram
    p <- plot_ly(
      x = ~kpi_values,
      type = "histogram",
      histnorm = "probability",
      marker = list(color = "skyblue", line = list(color = "darkblue", width = 1)),
      hovertemplate = paste(
        "Value: %{x:.2f}<br>",
        "Probability: %{y:.4f}<extra></extra>"
      )
    ) %>%
      layout(
        title = paste("KPI Distribution:", kpi_col),
        xaxis = list(
          title = list(text = "Value", standoff = 25),
          automargin = TRUE
        ),
        yaxis = list(
          title = list(text = "Probability", standoff = 20),
          automargin = TRUE
        ),
        margin = list(l = 70, r = 40, t = 60, b = 60),
        bargap = 0.1
      )
    
    return(p)
  })
  
  # Plot 5: KPI by Dimension - Total Values (Bar chart)
  output$kpi_by_dimension_total <- renderPlotly({
    req(dimension_data(), input$dimension_select)
    
    dim_data <- dimension_data()
    dimension <- input$dimension_select
    
    if (is.null(dim_data) || nrow(dim_data) == 0) {
      return(plot_ly() %>% 
               layout(title = "No dimension data available",
                      xaxis = list(showticklabels = FALSE),
                      yaxis = list(showticklabels = FALSE)))
    }
    
    # Ensure the dimension column is present
    if (!dimension %in% names(dim_data)) {
      return(plot_ly() %>% 
               layout(title = paste("Dimension", dimension, "not found in data"),
                      xaxis = list(showticklabels = FALSE),
                      yaxis = list(showticklabels = FALSE)))
    }
    
    # Convert dimension column to factor for ordering
    dim_data[[dimension]] <- factor(dim_data[[dimension]], 
                                   levels = dim_data[[dimension]][order(-dim_data$Total)])
    
    # Create bar chart with direct plotly (more control)
    p <- plot_ly(dim_data, x = ~get(dimension), y = ~Total, type = 'bar',
               marker = list(color = '#3498db'),
               hovertemplate = paste(
                 "%{x}<br>",
                 "Total: %{y:.2f}<extra></extra>"
               )) %>%
      layout(
        title = paste("Total KPI by", dimension),
        xaxis = list(
          title = dimension,
          automargin = TRUE
        ),
        yaxis = list(
          title = "Total KPI Value",
          automargin = TRUE
        ),
        margin = list(l = 80, r = 40, t = 60, b = 120)
      )
    
    return(p)
  })
  
  # Plot 6: KPI by Dimension - Percentage (Bar chart)
  output$kpi_by_dimension_percent <- renderPlotly({
    req(dimension_data(), input$dimension_select)
    
    dim_data <- dimension_data()
    dimension <- input$dimension_select
    
    if (is.null(dim_data) || nrow(dim_data) == 0) {
      return(plot_ly() %>% 
               layout(title = "No dimension data available",
                      xaxis = list(showticklabels = FALSE),
                      yaxis = list(showticklabels = FALSE)))
    }
    
    # Ensure the dimension column is present
    if (!dimension %in% names(dim_data)) {
      return(plot_ly() %>% 
               layout(title = paste("Dimension", dimension, "not found in data"),
                      xaxis = list(showticklabels = FALSE),
                      yaxis = list(showticklabels = FALSE)))
    }
    
    # Convert dimension column to factor for ordering
    dim_data[[dimension]] <- factor(dim_data[[dimension]], 
                                   levels = dim_data[[dimension]][order(-dim_data$Percentage)])
    
    # Create bar chart with direct plotly (more control)
    p <- plot_ly(dim_data, x = ~get(dimension), y = ~Percentage, type = 'bar',
               marker = list(color = '#2ecc71'),
               hovertemplate = paste(
                 "%{x}<br>",
                 "Percentage: %{y:.2f}%<extra></extra>"
               )) %>%
      layout(
        title = paste("Percentage of KPI by", dimension),
        xaxis = list(
          title = dimension,
          automargin = TRUE
        ),
        yaxis = list(
          title = "Percentage (%)",
          automargin = TRUE
        ),
        margin = list(l = 80, r = 40, t = 60, b = 120)
      )
    
    return(p)
  })
  
  # Table: KPI Summary Statistics
  output$kpi_summary_table <- renderDT({
    req(transformed_data(), input$kpi_selection)
    
    df <- transformed_data()
    kpi_col <- input$kpi_selection
    
    # Check if KPI column exists
    validate(
      need(kpi_col %in% names(df), "KPI column not found in the data.")
    )
    
    # Determine column names
    transformed_col <- if (input$transformation_kpi != "None") {
      if (input$transformation_kpi == "Logarithmic") {
        paste0(kpi_col, "_log")
      } else if (input$transformation_kpi == "Moving Average") {
        paste0(kpi_col, "_ma", input$ma_window)
      }
    } else {
      NA
    }
    
    normalized_col <- if (input$normalization_kpi != "None") {
      if (input$normalization_kpi == "Division by Mean") {
        paste0(ifelse(is.na(transformed_col), kpi_col, transformed_col), "_norm_div")
      } else if (input$normalization_kpi == "Subtraction of Mean") {
        paste0(ifelse(is.na(transformed_col), kpi_col, transformed_col), "_norm_sub")
      }
    } else {
      NA
    }
    
    # Create a function to calculate summary statistics
    get_stats <- function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) return(rep(NA, 11))
      
      c(
        Min = min(x, na.rm = TRUE),
        Q1 = quantile(x, 0.25, na.rm = TRUE),
        Median = median(x, na.rm = TRUE),
        Mean = mean(x, na.rm = TRUE),
        Q3 = quantile(x, 0.75, na.rm = TRUE),
        Max = max(x, na.rm = TRUE),
        StdDev = sd(x, na.rm = TRUE),
        Variance = var(x, na.rm = TRUE),
        Skewness = if (require(moments)) moments::skewness(x, na.rm = TRUE) else NA,
        Kurtosis = if (require(moments)) moments::kurtosis(x, na.rm = TRUE) else NA,
        "N (count)" = length(x)
      )
    }
    
    # Prepare summary data frame
    summary_list <- list(
      "Original KPI" = get_stats(df[[kpi_col]])
    )
    
    # Add transformed stats if available
    if (!is.na(transformed_col) && transformed_col %in% names(df)) {
      summary_list[[paste0("Transformed (", input$transformation_kpi, ")")]] <- 
        get_stats(df[[transformed_col]])
    }
    
    # Add normalized stats if available
    if (!is.na(normalized_col) && normalized_col %in% names(df)) {
      summary_list[[paste0("Normalized (", input$normalization_kpi, ")")]] <- 
        get_stats(df[[normalized_col]])
    }
    
    # Convert to data frame
    summary_df <- as.data.frame(do.call(rbind, summary_list))
    summary_df <- cbind(Statistic = rownames(summary_df), summary_df)
    rownames(summary_df) <- NULL
    
    # Format numeric columns
    datatable(
      summary_df,
      options = list(
        pageLength = 11,
        dom = 't',
        ordering = FALSE,
        columnDefs = list(list(className = 'dt-right', targets = 1:11))
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 2:11, digits = 4)
  })
  
  # Setup download handlers
  kpi_transformation_download_handler(input, output, session, rv)
  
  # Update filters
  observe({
    req(rv$data)
    
    # Update date range if date column exists
    date_col <- rv$date_col
    if (!is.null(date_col) && date_col %in% names(rv$data)) {
      date_values <- as.Date(rv$data[[date_col]])
      date_values <- date_values[!is.na(date_values)]
      if (length(date_values) > 0) {
        min_date <- min(date_values, na.rm = TRUE)
        max_date <- max(date_values, na.rm = TRUE)
        updateDateRangeInput(session, "date_range_kpi", 
                            start = min_date,
                            end = max_date)
      }
    }
    
    # Update geography filter
    if ("Geography" %in% names(rv$data) || "Geografia" %in% names(rv$data)) {
      geo_col <- if ("Geography" %in% names(rv$data)) "Geography" else "Geografia"
      geos <- sort(unique(rv$data[[geo_col]]))
      choices <- c("Total", geos)
      updateSelectInput(session, "geography_kpi", choices = choices, selected = "Total")
    }
    
    # Update dimension selector for analysis
    dimension_choices <- c()
    
    # Add geography column if exists
    if ("Geography" %in% names(rv$data)) {
      dimension_choices <- c(dimension_choices, "Geography")
    } else if ("Geografia" %in% names(rv$data)) {
      dimension_choices <- c(dimension_choices, "Geografia")
    }
    
    # Add other dimension columns if exist
    other_dimensions <- c(
      "Product", "Producto", "Campaign", "Campaña", "Outlet", "Creative", "Creativo"
    )
    
    for (dim in other_dimensions) {
      if (dim %in% names(rv$data)) {
        dimension_choices <- c(dimension_choices, dim)
      }
    }
    
    if (length(dimension_choices) > 0) {
      updateSelectInput(session, "dimension_select", 
                      choices = dimension_choices, 
                      selected = dimension_choices[1])
    }
    
    # Extract metric info for product, campaign, etc.
    metric_info <- extract_metric_info(rv$data)
    if (!is.null(metric_info) && nrow(metric_info) > 0) {
      updateSelectInput(session, "product_kpi", choices = c("N/A", sort(unique(metric_info$product[!is.na(metric_info$product)]))), selected = "N/A")
      updateSelectInput(session, "campaign_kpi", choices = c("N/A", sort(unique(metric_info$campaign[!is.na(metric_info$campaign)]))), selected = "N/A")
      updateSelectInput(session, "outlet_kpi", choices = c("N/A", sort(unique(metric_info$outlet[!is.na(metric_info$outlet)]))), selected = "N/A")
      updateSelectInput(session, "creative_kpi", choices = c("N/A", sort(unique(metric_info$creative[!is.na(metric_info$creative)]))), selected = "N/A")
    } else {
      updateSelectInput(session, "product_kpi", choices = c("N/A"), selected = "N/A")
      updateSelectInput(session, "campaign_kpi", choices = c("N/A"), selected = "N/A")
      updateSelectInput(session, "outlet_kpi", choices = c("N/A"), selected = "N/A")
      updateSelectInput(session, "creative_kpi", choices = c("N/A"), selected = "N/A")
    }
    
    # Update KPI selection with numeric columns
    numeric_cols <- names(rv$data)[sapply(rv$data, is.numeric)]
    if (length(numeric_cols) > 0) {
      updateSelectInput(session, "kpi_selection", choices = numeric_cols, selected = numeric_cols[1])
    }
  })
  
  # Force re-evaluation of dimension data when selection changes
  observeEvent(input$dimension_select, {
    req(input$dimension_select, transformed_data())
    dimension_data()
  })
  
  # Force re-evaluation when KPI selection changes
  observeEvent(input$kpi_selection, {
    req(input$kpi_selection, filtered_data())
    transformed_data()
  })
  
  # Force re-evaluation when transformation changes
  observeEvent(input$transformation_kpi, {
    req(input$kpi_selection, filtered_data())
    transformed_data()
  })
  
  # Force re-evaluation when normalization changes
  observeEvent(input$normalization_kpi, {
    req(input$kpi_selection, filtered_data())
    transformed_data()
  })
  
  # Force re-evaluation when ma_window changes
  observeEvent(input$ma_window, {
    req(input$kpi_selection, filtered_data(), input$transformation_kpi == "Moving Average")
    transformed_data()
  })
}