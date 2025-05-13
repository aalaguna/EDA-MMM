# S_Curve_EDA_Multivariate.R

# -------------------------------------------------------------------------#
# Module for Multivariate Render (S-Curve EDA)
# -------------------------------------------------------------------------

source("R/modules/common/s_curve_charts.R")
source("R/modules/common/s_curve_helpers.R")
source("R/modules/multivariate/Suggested_Max_Value.R")  # Import Suggested Max Value file for multivariate
library(dplyr)
library(shiny)
library(plotly)
library(shinycssloaders)

# -------------------------------------------------------------------------
# render_s_curve_multi: Renders the Multivariate S-Curve and Flighting charts
# -------------------------------------------------------------------------
render_s_curve_multi <- function(df, input) {
  req(df)
 
  validate(
    need(input$trans_var1 %in% c("S Origin", "S Shaped"),
         "This chart (Multivariate) is only displayed for 'S Origin' or 'S Shaped'.")
  )
 
  if (!"sum_vars" %in% names(df)) {
    return(plot_ly() %>% layout(title = "Column 'sum_vars' not found in df."))
  }
 
  # Use data already filtered with global date filter
  s_data <- df %>%
    select(Period, sum_vars) %>%
    rename(value = sum_vars) %>%
    mutate(Period = as.Date(Period))
 
  # Data for average calculation (green line)
  avg_data <- NULL
  avg_period <- 52  # Default value
  
  # Check if we have a date range selected for average calculation
  if (!is.null(input$avg_period_date_multi) && 
      length(input$avg_period_date_multi) == 2 && 
      !is.na(input$avg_period_date_multi[1]) && 
      !is.na(input$avg_period_date_multi[2])) {
      
    # Filter data for average calculation, excluding zeros
    avg_data <- s_data %>%
      filter(Period >= input$avg_period_date_multi[1] & 
             Period <= input$avg_period_date_multi[2] &
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
    avg_period <- nrow(s_data %>% filter(value > 0))
  }
 
  alpha       <- input$alpha_multi
  beta        <- input$beta_multi
  max_val_pct <- input$maxval_multi
  decay       <- input$decay_multi
  lag         <- input$lag_multi
 
  # First generate Flighting Chart to get its configurations
  flighting_plot <- tryCatch({
    create_flighting_chart(
      data_chart = s_data,
      alpha = alpha,
      beta = beta,
      max_val_pct = max_val_pct,
      decay = decay,
      lag = lag,
      var_name = "Multivariate Sum",
      calculated_key_points = NULL,  # No key points yet
      avg_period = avg_period,
      avg_data = avg_data  # Pass filtered data for average calculation
    )
  }, error = function(e) {
    return(plot_ly() %>% layout(title = paste("Error in Multivariate Flighting:", e$message)))
  })
  
  # Then generate S-Curve using Flighting Chart configurations
  s_curve_plot <- tryCatch({
    create_s_curve_chart(
      data_chart = s_data,
      alpha = alpha,
      beta = beta,
      max_val_pct = max_val_pct,
      decay = decay,
      lag = lag,
      var_name = "Multivariate Sum",
      avg_period = avg_period,
      avg_data = avg_data,  # Pass filtered data for average calculation
      flighting_chart = flighting_plot  # Pass flighting chart to coordinate axes
    )
  }, error = function(e) {
    return(plot_ly() %>% layout(title = paste("Error in Multivariate S-Curve:", e$message)))
  })
  
  # Get key points from S-Curve
  key_points <- attr(s_curve_plot, "key_points")
  
  # Recreate Flighting Chart with S-Curve key points
  flighting_plot <- tryCatch({
    create_flighting_chart(
      data_chart = s_data,
      alpha = alpha,
      beta = beta,
      max_val_pct = max_val_pct,
      decay = decay,
      lag = lag,
      var_name = "Multivariate Sum",
      calculated_key_points = key_points,  # Use key points from S-Curve
      avg_period = avg_period,
      avg_data = avg_data  # Pass filtered data for average calculation
    )
  }, error = function(e) {
    return(plot_ly() %>% layout(title = paste("Error in Multivariate Flighting:", e$message)))
  })
 
  subplot(flighting_plot, s_curve_plot, nrows = 1, widths = c(0.7, 0.3), titleX = TRUE, titleY = TRUE, margin = 0.05) %>%
    layout(
      title = "",  # Remove title
      margin = list(l = 60, r = 60, b = 60, t = 10, pad = 10),  # Reduce top margin
      autosize = TRUE,
      plot_bgcolor = '#FFFFFF',
      paper_bgcolor = '#FFFFFF',
      font = list(family = "Arial, sans-serif"),
      hoverlabel = list(bgcolor = "#FFF", font = list(size = 12, family = "Arial, sans-serif"), bordercolor = "#DDD")
    ) %>%
    config(displayModeBar = TRUE, displaylogo = FALSE, modeBarButtonsToRemove = list('sendDataToCloud', 'hoverCompareCartesian', 'hoverClosestCartesian', 'select2d', 'lasso2d'), responsive = TRUE)
}

# -------------------------------------------------------------------------
# prep_multi_data: Prepares multivariate data for analysis
# -------------------------------------------------------------------------
prep_multi_data <- function(df, input) {
  req(df)
 
  # Filter by global date range if available
  if (!is.null(input$date_filter_multi)) {
    date_col <- if ("Period" %in% names(df)) "Period" else if ("periodo" %in% names(df)) "periodo" else NULL
    if (!is.null(date_col)) {
      df[[date_col]] <- as.Date(df[[date_col]])
      df <- df[df[[date_col]] >= input$date_filter_multi[1] &
              df[[date_col]] <= input$date_filter_multi[2], ]
    }
  }
 
  all_vars <- c(input$var1, input$var2, input$var3, input$var4, input$var5, input$var6)
  vars <- all_vars[!is.na(all_vars) & all_vars != "N/A"]
 
  if (length(vars) == 0) return(NULL)
 
  transforms <- c(input$trans_var1, input$trans_var2, input$trans_var3, input$trans_var4, input$trans_var5, input$trans_var6)[!is.na(all_vars) & all_vars != "N/A"]
  weights <- c(input$weight_var1, input$weight_var2, input$weight_var3, input$weight_var4, input$weight_var5, input$weight_var6)[!is.na(all_vars) & all_vars != "N/A"]
 
  result_df <- df %>% mutate(Period = if ("Period" %in% names(.)) as.Date(Period) else as.Date(periodo))
 
  for (i in seq_along(vars)) {
    var_name <- vars[i]
    trans_type <- transforms[i]
    weight <- weights[i]
    var_values <- result_df[[var_name]]
    transformed_values <- apply_transformation(var_values, type = trans_type, alpha = input$alpha_multi, beta = input$beta_multi, maxval = input$maxval_multi, decay = input$decay_multi, lag = input$lag_multi)
    result_df[[paste0(var_name, "_transformed")]] <- transformed_values * weight
  }
 
  transformed_cols <- paste0(vars, "_transformed")
  result_df$sum_vars <- rowSums(result_df[transformed_cols], na.rm = TRUE)
 
  return(result_df)
}

# -------------------------------------------------------------------------
# ui_s_curve_eda_multivariate: UI for the S-Curve EDA module (Multivariate)
# -------------------------------------------------------------------------
ui_s_curve_eda_multivariate <- function(id) {
  ns <- NS(id)
 
  fluidPage(
    fluidRow(
      column(3,
             wellPanel(
               style = "background-color: #f9f9f9; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1);",
               h4("Global Parameters", style = "font-weight: bold; color: #333; border-bottom: 1px solid #ddd; padding-bottom: 8px;"),
               sliderInput(ns("alpha_multi"), "Alpha:", min = 0.01, max = 0.99, value = 0.85, step = 0.01, width = "100%"),
               sliderInput(ns("beta_multi"), "Beta:", min = 0.01, max = 10, value = 1, step = 0.01, width = "100%"),
               sliderInput(ns("maxval_multi"), "Max Value %:", min = 50, max = 200, value = 100, step = 5, width = "100%"),
               sliderInput(ns("decay_multi"), "Decay:", min = 0.01, max = 5, value = 1, step = 0.01, width = "100%"),
               numericInput(ns("lag_multi"), "Lag (periods):", min = 0, max = 100, value = 0, step = 1, width = "100%"),
               
               # Selector for average period (ONLY affects green line calculation)
               conditionalPanel(
                 condition = paste0("input['", ns("trans_var1"), "'] == 'S Origin' || input['", ns("trans_var1"), "'] == 'S Shaped'"),
                 div(style = "margin-top: 15px;",
                     h4("Average Period Selection", style = "font-weight: bold; color: #333; border-bottom: 1px solid #ddd; padding-bottom: 8px;"),
                     dateRangeInput(ns("avg_period_date_multi"), "Select Date Range for Average:",
                                    start = Sys.Date() - 365,
                                    end = Sys.Date(),
                                    separator = " to "),
                     helpText("This range ONLY affects the average calculation (green line)"),
                     # Specific button to update average
                     actionButton(ns("update_avg_multi"), "Update Average", 
                                 class = "btn-primary btn-block", 
                                 style = "margin-top: 10px;")
                 )
               ),
               
               # Download button
               div(style = "margin-top: 15px; text-align: center;",
                   downloadButton(ns("download_multivariate"), "Download Transformed Data",
                                 class = "btn-primary btn-block custom-download-btn",
                                 icon = icon("download"))
               ),
               
               hr(),
               h4("Variables", style = "font-weight: bold; color: #333; border-bottom: 1px solid #ddd; padding-bottom: 8px;")
             ),
             wellPanel(
               style = "background-color: #f3f8ff; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-top: 10px;",
               h4("Variable 1", style = "color: #0066cc;"),
               selectInput(ns("var1"), "Select:", choices = c("Select" = "N/A"), width = "100%"),
               selectInput(ns("trans_var1"), "Transformation:", choices = c("Linear", "S Origin", "S Shaped", "Index Exp", "Log", "Exp", "Power", "Moving Avg"), selected = "S Origin", width = "100%"),
               sliderInput(ns("weight_var1"), "Weight:", min = 0, max = 10, value = 1, step = 0.1, width = "100%")
             ),
             lapply(2:6, function(i) {
               wellPanel(
                 style = paste0("background-color: #f", i, "f", i+2, "ff; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); margin-top: 10px;"),
                 h4(paste("Variable", i), style = paste0("color: #00", i, "6cc;")),
                 selectInput(ns(paste0("var", i)), "Select:", choices = c("Select" = "N/A", "None" = NA), width = "100%"),
                 selectInput(ns(paste0("trans_var", i)), "Transformation:", choices = c("Linear", "S Origin", "S Shaped", "Index Exp", "Log", "Exp", "Power", "Moving Avg"), selected = "S Origin", width = "100%"),
                 sliderInput(ns(paste0("weight_var", i)), "Weight:", min = 0, max = 10, value = 1, step = 0.1, width = "100%")
               )
             })
      ),
      column(9,
             # Add panel for Suggested Max Value
             wellPanel(
               style = "background-color: white; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); padding: 15px; margin-bottom: 15px;",
               h4("Suggested Max Value", style = "font-weight: bold; color: #333; margin-top: 0;"),
               verbatimTextOutput(ns("transformations_summary_multi"))
             ),
             div(
               class = "chart-box",
               style = "background-color: white; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); padding: 15px; height: 700px;",
               plotlyOutput(ns("s_curve_multi_plot"), height = "100%") %>% withSpinner(color = "#0275d8", type = 5)
             )
      )
    )
  )
}

# -------------------------------------------------------------------------
# server_s_curve_eda_multivariate: Server for the S-Curve EDA module (Multivariate)
# -------------------------------------------------------------------------
server_s_curve_eda_multivariate <- function(id, df, var_choices) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(var_choices())
      var_opts <- c("Select" = "N/A", var_choices())
      var_opts_with_none <- c("Select" = "N/A", "None" = NA, var_choices())
      updateSelectInput(session, "var1", choices = var_opts)
      lapply(2:6, function(i) updateSelectInput(session, paste0("var", i), choices = var_opts_with_none))
    })
   
    # Initialize date values for both selectors
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
          # Update both date selectors
          updateDateRangeInput(session, "date_filter_multi",
                              start = min_date,
                              end = max_date)
          updateDateRangeInput(session, "avg_period_date_multi",
                              start = min_date,
                              end = max_date)
        }
      }
    })
   
    multi_data <- reactive({
      req(df(), input$var1 != "N/A", input$trans_var1)
      prep_multi_data(df(), input)
    })
    
    # Reactive value to force chart update
    update_trigger <- reactiveVal(0)
    
    # Specific observer for update button
    observeEvent(input$update_avg_multi, {
      update_trigger(update_trigger() + 1)
    }, ignoreInit = TRUE)
    
    # Observer for changes in parameters that affect the chart
    observe({
      # List of inputs that should trigger an update
      input$alpha_multi
      input$beta_multi
      input$maxval_multi
      input$decay_multi
      input$lag_multi
      input$var1
      input$trans_var1
      update_trigger() # Include update trigger
      
      # Update chart only if we have data
      if (!is.null(multi_data())) {
        # Render chart
        output$s_curve_multi_plot <- renderPlotly({
          render_s_curve_multi(multi_data(), input)
        })
      }
    })
   
    # Render initial chart
    output$s_curve_multi_plot <- renderPlotly({
      req(multi_data())
      render_s_curve_multi(multi_data(), input)
    })
   
    # Render Suggested Max Value for multivariate transformations
    output$transformations_summary_multi <- renderPrint({
      req(df(), input$var1 != "N/A")
      render_transformations_summary_multi(df(), input)
    })
   
    # Link download button to correct handler based on mode
    observe({
      if (input$sum_all_vars == "true") {
        # Download in sum mode
        output$download_multivariate <- downloadHandler(
          filename = function() {
            paste0("multivariate_summed_", Sys.Date(), ".csv")
          },
          content = function(file) {
            # Implementation in multivariate_download_handler
          }
        )
      } else {
        # Download in individual mode
        output$download_multivariate <- downloadHandler(
          filename = function() {
            paste0("multivariate_individual_", Sys.Date(), ".csv")
          },
          content = function(file) {
            # Implementation in multivariate_download_handler
          }
        )
      }
    })
  })
}