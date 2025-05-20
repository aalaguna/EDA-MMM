# =============================================================================
# Univariate tab UI definition with KPI Transformation module
# =============================================================================

#' UI for the univariate analysis tab
#'
#' @return A tagList with the UI elements for the univariate analysis tab
#' @export
ui_univariate <- function() {
  shiny::tagList(
    shiny::fluidPage(
      # Global filter section
      shiny::div(class = "section-card", 
          shiny::h4("Global Filters", class = "section-title"),
          shiny::fluidRow(
            shiny::column(2, shiny::selectInput("geography_univ", "Geography", choices = NULL)),
            shiny::column(2, shiny::selectInput("product_univ", "Product", choices = NULL)),
            shiny::column(2, shiny::selectInput("campaign_univ", "Campaign", choices = NULL)),
            shiny::column(2, shiny::selectInput("outlet_univ", "Outlet", choices = NULL)),
            shiny::column(2, shiny::selectInput("creative_univ", "Creative", choices = NULL))            
          )
      ),
      
      # Main content layout
      shiny::fluidRow(
        # Left panel with controls
        shiny::column(width = 3,
          # KPI selection and transformation
          shiny::div(class = "section-card",
            shiny::h4("KPI Selection", class = "section-title"),
            shiny::selectInput("kpi_univ", "KPI", choices = NULL),
            
            # Add KPI Transformation controls
            shiny::h4("KPI Transformation", class = "section-title"),
            shiny::radioButtons("kpi_transformation_univ", NULL,
                        choices = c("None", "Logarithmic", "Moving Average"),
                        selected = "None"),
            
            # Conditional panel for Moving Average window size
            shiny::conditionalPanel(
              condition = "input.kpi_transformation_univ == 'Moving Average'",
              shiny::sliderInput("ma_window_univ", "Window Size (periods)", 
                         min = 2, max = 30, value = 3, step = 1)
            ),
            
            # KPI Normalization (existing)
            shiny::h4("KPI Normalization", class = "section-title"),
            shiny::radioButtons("kpi_normalization_univ", NULL,
                        choices = c("None", "Division", "Subtraction"),
                        selected = "None"),
            
            shiny::tags$div(
              style = "font-size: 0.85rem; color: #666; margin-bottom: 15px;",
              shiny::tags$p("Division: KPI/mean(KPI)"),
              shiny::tags$p("Subtraction: KPI-mean(KPI)")
            ),
            
            # Variable selection
            shiny::h4("Variable Selection", class = "section-title"),
            shiny::selectInput("variable_univ", "Variable", choices = NULL),
            
            # Transformation selection
            shiny::h4("Transformation", class = "section-title"),
            shiny::radioButtons("transformation_univ", NULL,
                         choices = c("Linear", "S Origin", "S Shaped", "Index Exp", "Log", "Exp", "Power", "Moving Avg"),
                         selected = "Linear"),
            
            # Average period selection for S-curves
            shiny::h4("Average Period Selection", class = "section-title"),
            shiny::dateRangeInput("avg_date_range_univ", "Select Date Range for Average:",
                           start = Sys.Date() - 365,
                           end = Sys.Date(),
                           separator = " to "),
            
            # Download button
            shiny::div(style = "margin-top: 15px; text-align: center;",
                shiny::downloadButton("download_univariate", "Download Transformed Data", 
                                class = "btn-primary btn-block custom-download-btn",
                                icon = shiny::icon("download"))
            )
          )
        ),
        
        # Right panel with charts and results
        shiny::column(width = 9,
          # Transformation parameters section
          shiny::div(class = "section-card",
            shiny::h4("Transformation Settings", class = "section-title"),
            shiny::div(class = "transform-params",
              # Add Lag parameter for KPI Transformation pipeline
              shiny::div(class = "transform-param-item", 
                   shiny::numericInput("lag_kpi_univ", "KPI Lag", value = 0, min = 0, step = 1)),
              
              # Add Decay parameter for KPI Transformation pipeline
              shiny::div(class = "transform-param-item", 
                   shiny::numericInput("decay_kpi_univ", "KPI Decay", value = 1, min = 0, max = 1, step = 0.1)),
              
              # Existing parameters for variable transformation
              shiny::div(class = "transform-param-item", 
                   shiny::numericInput("decay_univ", "Decay", value = 1, min = 0.01, step = 0.1)),
              
              shiny::div(class = "transform-param-item", 
                   shiny::numericInput("lag_univ", "Lag", value = 0, min = 0, step = 1)),
              
              shiny::div(class = "transform-param-item", 
                   shiny::numericInput("maxval_univ", "% MaxVal", value = 100, min = 1, step = 1)),
              
              shiny::div(class = "transform-param-item", 
                   shiny::numericInput("alpha_univ", "Alpha", value = 0.85, min = 0.01, max = 0.99, step = 0.01)),
              
              shiny::div(class = "transform-param-item", 
                   shiny::numericInput("beta_univ", "Beta", value = 1, min = 0.01, step = 0.1))
            )
          ),
          
          # Main charts section - top row
          shiny::fluidRow(
            shiny::column(6, 
              shiny::div(class = "chart-box", 
                shiny::h4("Variable Flighting", class = "chart-title"), 
                plotly::plotlyOutput("variable_flighting_chart", height = "300px") %>% 
                  shinycssloaders::withSpinner(color = "#0275d8", type = 5)
              )
            ),
            
            shiny::column(6, 
              shiny::div(class = "chart-box", 
                shiny::h4("Transformed Variable", class = "chart-title"), 
                plotly::plotlyOutput("var_transf_chart", height = "300px") %>% 
                  shinycssloaders::withSpinner(color = "#0275d8", type = 5)
              )
            )
          ),
          
          # S-Curve section - only shown for S-curve transformations
          shiny::div(class = "chart-box",
            shiny::conditionalPanel(
              condition = "input.transformation_univ == 'S Origin' || input.transformation_univ == 'S Shaped'",
              shiny::h4("S-Curve EDA", class = "chart-title"),
              plotly::plotlyOutput("s_curve_univariate_plot", height = "400px") %>% 
                shinycssloaders::withSpinner(color = "#0275d8", type = 5)
            )
          ),
          
          # Bottom row of charts
          shiny::fluidRow(
            shiny::column(4, 
              shiny::div(class = "chart-box", 
                shiny::h4("Boxplot", class = "chart-title"), 
                shiny::plotOutput("boxplot_univ", height = "300px") %>% 
                  shinycssloaders::withSpinner(color = "#0275d8", type = 5)
              )
            ),
            
            shiny::column(4, 
              shiny::div(class = "chart-box", 
                shiny::h4("Suggested Parameters", class = "chart-title"), 
                shiny::verbatimTextOutput("transformations_summary_univ", placeholder = TRUE) %>% 
                  shinycssloaders::withSpinner(color = "#0275d8", type = 5)
              )
            ),
            
            shiny::column(4, 
              shiny::div(class = "chart-box", 
                shiny::h4("KPI vs. Variable Correlation", class = "chart-title"), 
                plotly::plotlyOutput("corr_kpi_var_univ", height = "300px") %>% 
                  shinycssloaders::withSpinner(color = "#0275d8", type = 5)
              )
            )
          )
        )
      )
    )
  )
}