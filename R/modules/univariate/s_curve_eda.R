# =============================================================================
# S-Curve EDA (Exploratory Data Analysis) for Univariate Analysis
# =============================================================================

source("R/modules/common/s_curve_charts.R")  # Ensure this file exists
source("R/modules/common/s_curve_helpers.R") # Ensure this file exists
library(dplyr)
library(shiny)
library(plotly)
library(shinycssloaders)

# -------------------------------------------------------------------------
# render_s_curve_plots: Renders the Univariate S-Curve and Flighting charts
# -------------------------------------------------------------------------
render_s_curve_plots <- function(df, input) {
  # Validate inputs
  req(df, input$variable_univ)
  validate(
    need(input$variable_univ != "N/A", "Please select a valid variable for the S-Curve."),
    need(input$transformation_univ %in% c("S Origin", "S Shaped"), 
         "This chart is only displayed for 'S Origin' or 'S Shaped' transformations.")
  )
  
  # Check if variable exists in data
  if (!input$variable_univ %in% names(df)) {
    return(plot_ly() %>% 
             layout(title = paste("Variable", input$variable_univ, "not found in data."),
                    xaxis = list(showticklabels = FALSE),
                    yaxis = list(showticklabels = FALSE)))
  }
 
  # Use data already filtered by global filter, with error handling
  df_scurve <- tryCatch({
    # Identify date column
    date_col <- if ("Period" %in% names(df)) {
      "Period" 
    } else if ("periodo" %in% names(df)) {
      "periodo"
    } else {
      NULL
    }
    
    if (is.null(date_col)) {
      stop("Date column not found in data.")
    }
    
    # Prepare data for S-curve analysis
    df %>%
      mutate(Period = as.Date(!!sym(date_col))) %>%
      select(Period, value = !!sym(input$variable_univ)) %>%
      filter(!is.na(Period), !is.na(value))  # Remove rows with NA
  }, error = function(e) {
    warning(paste("Error preparing data for S-curve analysis:", e$message))
    return(NULL)
  })
  
  # Check if we have valid data after preparation
  if (is.null(df_scurve) || nrow(df_scurve) == 0) {
    return(plot_ly() %>% 
             layout(title = "No valid data available for S-Curve analysis.",
                    xaxis = list(showticklabels = FALSE),
                    yaxis = list(showticklabels = FALSE)))
  }
 
  # Apply lag and decay to data with error handling
  lag_value <- as.integer(input$lag_univ)
  decay_value <- as.numeric(input$decay_univ)
  
  # Validate lag value
  if (is.na(lag_value) || lag_value < 0) {
    lag_value <- 0
    warning("Invalid lag value. Using 0 instead.")
  }
  
  # Validate decay value
  if (is.na(decay_value) || decay_value <= 0) {
    decay_value <- 1
    warning("Invalid decay value. Using 1 instead.")
  }
  
  # Apply lag
  if (lag_value > 0) {
    if (lag_value >= nrow(df_scurve)) {
      df_scurve$value <- rep(NA, nrow(df_scurve))
      warning("Lag value is too large for the data size. All values will be NA.")
    } else {
      df_scurve$value <- c(rep(NA, lag_value), head(df_scurve$value, -lag_value))
    }
  }
  
  # Apply decay
  df_scurve$value <- as.numeric(df_scurve$value) * decay_value
  
  # Remove rows with NA values after transformation
  df_scurve <- df_scurve[!is.na(df_scurve$value), ]
  
  # Check if we still have data after transformations
  if (nrow(df_scurve) == 0) {
    return(plot_ly() %>% 
             layout(title = "No valid data available after applying lag and decay.",
                    xaxis = list(showticklabels = FALSE),
                    yaxis = list(showticklabels = FALSE)))
  }
 
  # Data for average calculation (green line)
  avg_data <- NULL
  avg_period <- 52  # Default value
  
  # Check if we have a date range selected for average calculation
  if (!is.null(input$avg_date_range_univ) && 
      length(input$avg_date_range_univ) == 2 && 
      !is.na(input$avg_date_range_univ[1]) && 
      !is.na(input$avg_date_range_univ[2])) {
      
    # Filter data for average calculation, excluding zeros and negative values
    tryCatch({
      avg_data <- df_scurve %>%
        filter(Period >= input$avg_date_range_univ[1] & 
               Period <= input$avg_date_range_univ[2] &
               value > 0)  # Exclude zeros and negative values
      
      # Only update avg_period if there's data in the range
      if (nrow(avg_data) > 0) {
        avg_period <- nrow(avg_data)
      } else {
        # If no data in range, clear avg_data to use the average of the whole series
        avg_data <- NULL
        warning("No positive values in the selected date range for average calculation.")
      }
    }, error = function(e) {
      warning(paste("Error filtering data for average calculation:", e$message))
      avg_data <- NULL
    })
  }
  
  # Verify we have data for average calculation
  if (is.null(avg_data)) {
    # Use all positive values if no specific data
    avg_period <- nrow(df_scurve %>% filter(value > 0))
  }
 
  # Get parameter values for S-curve with validation
  var_name <- input$variable_univ
  
  alpha_value <- as.numeric(input$alpha_univ)
  if (is.na(alpha_value) || alpha_value <= 0 || alpha_value >= 1) {
    alpha_value <- 0.85  # Default value
    warning("Invalid alpha value. Using default value of 0.85.")
  }
  
  beta_value <- as.numeric(input$beta_univ)
  if (is.na(beta_value) || beta_value <= 0) {
    beta_value <- 1  # Default value
    warning("Invalid beta value. Using default value of 1.")
  }
  
  maxval_value <- as.numeric(input$maxval_univ)
  if (is.na(maxval_value) || maxval_value <= 0) {
    maxval_value <- 100  # Default value
    warning("Invalid maxval value. Using default value of 100.")
  }
 
  # First generate Flighting Chart to get its configurations
  flighting_plot <- tryCatch({
    create_flighting_chart(
      data_chart = df_scurve,
      alpha = alpha_value,
      beta = beta_value,
      max_val_pct = maxval_value,
      decay = decay_value,
      lag = lag_value,
      var_name = var_name,
      calculated_key_points = NULL,  # No key points yet
      avg_period = avg_period,
      avg_data = avg_data  # Pass filtered data for average calculation
    )
  }, error = function(e) {
    warning(paste("Error creating flighting chart:", e$message))
    return(plot_ly() %>% layout(title = paste("Error in Flighting Chart:", e$message)))
  })
  
  # Then generate S-Curve using Flighting Chart configurations
  s_curve_plot <- tryCatch({
    create_s_curve_chart(
      data_chart = df_scurve,
      alpha = alpha_value,
      beta = beta_value,
      max_val_pct = maxval_value,
      decay = decay_value,
      lag = lag_value,
      var_name = var_name,
      avg_period = avg_period,
      avg_data = avg_data,  # Pass filtered data for average calculation
      flighting_chart = flighting_plot  # Pass flighting chart to coordinate axes
    )
  }, error = function(e) {
    warning(paste("Error creating S-curve chart:", e$message))
    return(plot_ly() %>% layout(title = paste("Error in S-Curve Chart:", e$message)))
  })
  
  # Get key points from S-Curve
  key_points <- attr(s_curve_plot, "key_points")
  
  # Recreate Flighting Chart with S-Curve key points
  flighting_plot <- tryCatch({
    create_flighting_chart(
      data_chart = df_scurve,
      alpha = alpha_value,
      beta = beta_value,
      max_val_pct = maxval_value,
      decay = decay_value,
      lag = lag_value,
      var_name = var_name,
      calculated_key_points = key_points,  # Use key points from S-Curve
      avg_period = avg_period,
      avg_data = avg_data  # Pass filtered data for average calculation
    )
  }, error = function(e) {
    warning(paste("Error recreating flighting chart with key points:", e$message))
    return(plot_ly() %>% layout(title = paste("Error in Flighting Chart:", e$message)))
  })
 
  # Create combined subplot
  tryCatch({
    subplot(flighting_plot, s_curve_plot, nrows = 1, widths = c(0.7, 0.3), titleX = TRUE, titleY = TRUE, margin = 0.05) %>%
      layout(
        title = list(
          text = paste("S-Curve Analysis:", var_name),
          font = list(size = 16, family = "Arial", color = "#333")
        ),
        margin = list(l = 60, r = 60, b = 60, t = 50, pad = 10),
        autosize = TRUE,
        plot_bgcolor = '#FFFFFF',
        paper_bgcolor = '#FFFFFF',
        font = list(family = "Arial, sans-serif"),
        hoverlabel = list(bgcolor = "#FFF", font = list(size = 12, family = "Arial, sans-serif"), bordercolor = "#DDD"),
        annotations = list(
          list(
            x = 0.5,
            y = 1.05,
            xref = "paper",
            yref = "paper",
            text = paste0("Alpha: ", round(alpha_value, 3), ", Beta: ", round(beta_value, 3), 
                          ", MaxVal: ", round(maxval_value, 1), "%, Decay: ", round(decay_value, 2), 
                          if(lag_value > 0) paste0(", Lag: ", lag_value) else ""),
            showarrow = FALSE,
            font = list(size = 12, color = "#666")
          )
        )
      ) %>%
      config(displayModeBar = TRUE, displaylogo = FALSE, 
             modeBarButtonsToRemove = list('sendDataToCloud', 'hoverCompareCartesian', 
                                          'hoverClosestCartesian', 'select2d', 'lasso2d'), 
             responsive = TRUE)
  }, error = function(e) {
    warning(paste("Error creating combined subplot:", e$message))
    return(plot_ly() %>% layout(title = "Error creating S-curve visualization. Please check your parameters."))
  })
}

# -------------------------------------------------------------------------
# ui_s_curve_eda_univariate: UI for the S-Curve EDA module (Univariate)
# -------------------------------------------------------------------------
ui_s_curve_eda_univariate <- function(id) {
  ns <- NS(id)
 
  fluidPage(
    fluidRow(
      column(3,
             wellPanel(
               style = "background-color: #f9f9f9; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1);",
               h4("Parameters", style = "font-weight: bold; color: #333; border-bottom: 1px solid #ddd; padding-bottom: 8px;"),
               selectInput(ns("variable_univ"), "Variable:", choices = c("Select" = "N/A"), width = "100%"),
               selectInput(ns("transformation_univ"), "Transformation:",
                           choices = c("Linear", "S Origin", "S Shaped", "Index Exp", "Log", "Exp", "Power", "Moving Avg"),
                           selected = "S Origin", width = "100%"),
               sliderInput(ns("alpha_univ"), "Alpha:", min = 0.01, max = 0.99, value = 0.85, step = 0.01, width = "100%"),
               sliderInput(ns("beta_univ"), "Beta:", min = 0.01, max = 10, value = 1, step = 0.01, width = "100%"),
               sliderInput(ns("maxval_univ"), "Max Value %:", min = 50, max = 200, value = 100, step = 5, width = "100%"),
               sliderInput(ns("decay_univ"), "Decay:", min = 0.01, max = 5, value = 1, step = 0.01, width = "100%"),
               numericInput(ns("lag_univ"), "Lag (periods):", min = 0, max = 100, value = 0, step = 1, width = "100%"),
               
               # Selector for average period
               conditionalPanel(
                 condition = paste0("input['", ns("transformation_univ"), "'] == 'S Origin' || input['", ns("transformation_univ"), "'] == 'S Shaped'"),
                 div(style = "margin-top: 15px;",
                     h4("Average Period Selection", style = "font-weight: bold; color: #333; border-bottom: 1px solid #ddd; padding-bottom: 8px;"),
                     dateRangeInput(ns("avg_date_range_univ"), "Select Date Range for Average:",
                                    start = Sys.Date() - 365,
                                    end = Sys.Date(),
                                    separator = " to "),
                     helpText("This range ONLY affects the average calculation (green line)"),
                     # Specific button to update average
                     actionButton(ns("update_avg_univ"), "Update Average", 
                                 class = "btn-primary btn-block", 
                                 style = "margin-top: 10px;")
                 )
               )
             )
      ),
      column(9,
             div(
               class = "chart-box",
               style = "background-color: white; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); padding: 15px;",
               plotlyOutput(ns("s_curve_plot"), height = "600px") %>% withSpinner(color = "#0275d8", type = 5)
             )
      )
    )
  )
}

# -------------------------------------------------------------------------
# server_s_curve_eda_univariate: Server for the S-Curve EDA module (Univariate)
# -------------------------------------------------------------------------
server_s_curve_eda_univariate <- function(id, df, var_choices) {
  moduleServer(id, function(input, output, session) {
    # Update variable choices when they change
    observe({
      req(var_choices())
      updateSelectInput(session, "variable_univ", choices = c("Select" = "N/A", var_choices()))
    })
   
    # Initialize date values when data changes
    observe({
      req(df())
      # Detect date column
      date_col <- if ("Period" %in% names(df())) "Period" else if ("periodo" %in% names(df())) "periodo" else NULL
      if (!is.null(date_col)) {
        date_values <- as.Date(df()[[date_col]])
        date_values <- date_values[!is.na(date_values)]
        if (length(date_values) > 0) {
          min_date <- min(date_values, na.rm = TRUE)
          max_date <- max(date_values, na.rm = TRUE)
         
          # Update date range input for average
          updateDateRangeInput(session, "avg_date_range_univ",
                               start = min_date,
                               end = max_date)
        }
      }
    })
   
    # Filter data based on date range if exists
    filtered_data <- reactive({
      req(df())
      data <- df()
     
      # Ensure data contains valid date column 
      tryCatch({
        # Convert date column if exists
        if ("Period" %in% names(data)) {
          data$Period <- as.Date(data$Period)
        } else if ("periodo" %in% names(data)) {
          data$Period <- as.Date(data$periodo)
        } else {
          warning("No date column found in data.")
        }
      }, error = function(e) {
        warning(paste("Error converting date column:", e$message))
      })
     
      return(data)
    })
    
    # Reactive value to force chart update
    update_trigger <- reactiveVal(0)
    
    # Specific observer for update button
    observeEvent(input$update_avg_univ, {
      update_trigger(update_trigger() + 1)
    }, ignoreInit = TRUE)
    
    # Observer for changes in parameters that affect the chart
    observe({
      # List of inputs that should trigger an update
      input$variable_univ
      input$transformation_univ
      input$alpha_univ
      input$beta_univ
      input$maxval_univ
      input$decay_univ
      input$lag_univ
      update_trigger() # Include update trigger
      
      # Update chart only if we have data and a selected variable
      if (!is.null(filtered_data()) && input$variable_univ != "N/A") {
        # Render chart
        output$s_curve_plot <- renderPlotly({
          withProgress(message = 'Generating S-Curve...', value = 0, {
            render_s_curve_plots(filtered_data(), input)
          })
        })
      }
    })
    
    # Render initial chart
    output$s_curve_plot <- renderPlotly({
      req(filtered_data(), input$variable_univ != "N/A")
      withProgress(message = 'Generating S-Curve...', value = 0, {
        render_s_curve_plots(filtered_data(), input)
      })
    })
  })
}