# S_Curve_EDA_Univariate.R


source("R/modules/common/s_curve_charts.R")
source("R/modules/common/s_curve_helpers.R")
library(dplyr)
library(shiny)
library(plotly)
library(shinycssloaders)

# -------------------------------------------------------------------------
# render_s_curve_plots: Renders the Univariate S-Curve and Flighting charts
# -------------------------------------------------------------------------
render_s_curve_plots <- function(df, input) {
  req(df, input$variable_univ)
  validate(
    need(input$variable_univ != "N/A", "Please select a valid variable for the S-Curve."),
    need(input$transformation_univ %in% c("S Origin", "S Shaped"), "This chart is only displayed for 'S Origin' or 'S Shaped'.")
  )
 
  # Use data already filtered by global filter
  df_scurve <- df %>%
    mutate(Period = if ("Period" %in% names(.)) as.Date(Period) else as.Date(periodo)) %>%
    select(Period, value = !!sym(input$variable_univ))
 
  # Apply lag and decay to data
  if (input$lag_univ > 0) {
    if (input$lag_univ >= nrow(df_scurve)) df_scurve$value <- rep(NA, nrow(df_scurve))
    else df_scurve$value <- c(rep(NA, input$lag_univ), head(df_scurve$value, -input$lag_univ))
  }
 
  df_scurve$value <- as.numeric(df_scurve$value) * input$decay_univ
 
  if (nrow(df_scurve) == 0) return(plot_ly() %>% layout(title = "No data available for S-Curve."))
 
  # Data for average calculation (green line)
  avg_data <- NULL
  avg_period <- 52  # Default value
  
  # Check if we have a date range selected for average calculation
  if (!is.null(input$avg_date_range_univ) && 
      length(input$avg_date_range_univ) == 2 && 
      !is.na(input$avg_date_range_univ[1]) && 
      !is.na(input$avg_date_range_univ[2])) {
      
    # Filter data for average calculation, excluding zeros
    avg_data <- df_scurve %>%
      filter(Period >= input$avg_date_range_univ[1] & 
             Period <= input$avg_date_range_univ[2] &
             value > 0)  # Exclude zeros
    
    # Only update avg_period if there's data in the range
    if (nrow(avg_data) > 0) {
      avg_period <- nrow(avg_data)
    } else {
      # If no data in range, clear avg_data to use the average of the whole series
      avg_data <- NULL
    }
  }
  
  # Verify we have data for average calculation
  if (is.null(avg_data)) {
    # Use all non-zero values if no specific data
    avg_period <- nrow(df_scurve %>% filter(value > 0))
  }
 
  # Create charts with appropriate variable values
  var_name    <- input$variable_univ
  alpha       <- input$alpha_univ
  beta        <- input$beta_univ
  max_val_pct <- input$maxval_univ
  decay       <- input$decay_univ
  lag         <- input$lag_univ
 
  # First generate Flighting Chart to get its configurations
  flighting_plot <- tryCatch({
    create_flighting_chart(
      data_chart = df_scurve,
      alpha = alpha,
      beta = beta,
      max_val_pct = max_val_pct,
      decay = decay,
      lag = lag,
      var_name = var_name,
      calculated_key_points = NULL,  # No key points yet
      avg_period = avg_period,
      avg_data = avg_data  # Pass filtered data for average calculation
    )
  }, error = function(e) {
    return(plot_ly() %>% layout(title = paste("Error in Flighting Chart:", e$message)))
  })
  
  # Then generate S-Curve using Flighting Chart configurations
  s_curve_plot <- tryCatch({
    create_s_curve_chart(
      data_chart = df_scurve,
      alpha = alpha,
      beta = beta,
      max_val_pct = max_val_pct,
      decay = decay,
      lag = lag,
      var_name = var_name,
      avg_period = avg_period,
      avg_data = avg_data,  # Pass filtered data for average calculation
      flighting_chart = flighting_plot  # Pass flighting chart to coordinate axes
    )
  }, error = function(e) {
    return(plot_ly() %>% layout(title = paste("Error in S-Curve Chart:", e$message)))
  })
  
  # Get key points from S-Curve
  key_points <- attr(s_curve_plot, "key_points")
  
  # Recreate Flighting Chart with S-Curve key points
  flighting_plot <- tryCatch({
    create_flighting_chart(
      data_chart = df_scurve,
      alpha = alpha,
      beta = beta,
      max_val_pct = max_val_pct,
      decay = decay,
      lag = lag,
      var_name = var_name,
      calculated_key_points = key_points,  # Use key points from S-Curve
      avg_period = avg_period,
      avg_data = avg_data  # Pass filtered data for average calculation
    )
  }, error = function(e) {
    return(plot_ly() %>% layout(title = paste("Error in Flighting Chart:", e$message)))
  })
 
  # Create combined subplot
  subplot(flighting_plot, s_curve_plot, nrows = 1, widths = c(0.7, 0.3), titleX = TRUE, titleY = TRUE, margin = 0.05) %>%
    layout(
      title = "",
      margin = list(l = 60, r = 60, b = 60, t = 10, pad = 10),
      autosize = TRUE,
      plot_bgcolor = '#FFFFFF',
      paper_bgcolor = '#FFFFFF',
      font = list(family = "Arial, sans-serif"),
      hoverlabel = list(bgcolor = "#FFF", font = list(size = 12, family = "Arial, sans-serif"), bordercolor = "#DDD")
    ) %>%
    config(displayModeBar = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = list('sendDataToCloud', 'hoverCompareCartesian', 'hoverClosestCartesian', 'select2d', 'lasso2d'), responsive = TRUE)
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
     
      # Convert date column if exists
      if ("Period" %in% names(data)) {
        data$Period <- as.Date(data$Period)
      } else if ("periodo" %in% names(data)) {
        data$Period <- as.Date(data$periodo)
      }
     
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
          render_s_curve_plots(filtered_data(), input)
        })
      }
    })
    
    # Render initial chart
    output$s_curve_plot <- renderPlotly({
      req(filtered_data(), input$variable_univ != "N/A")
      render_s_curve_plots(filtered_data(), input)
    })
  })
}