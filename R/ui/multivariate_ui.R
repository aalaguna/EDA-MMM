# =============================================================================
# Multivariate tab UI definition with KPI Transformation module
# =============================================================================

ui_multivariate <- function() {
  tagList(
    fluidPage(
      div(class = "section-card", h4("Global Filters", class = "section-title"),
          fluidRow(
            column(4, dateRangeInput("date_range_multi", "Date Range:",
                                     start = Sys.Date() - 30,
                                     end = Sys.Date(),
                                     format = "yyyy-mm-dd",
                                     language = "en")
            ),
            column(8,
                   fluidRow(
                     column(3, selectInput("geography_multi", "Geography", choices = NULL)),
                     column(3, selectInput("product_multi", "Product", choices = NULL)),
                     column(2, selectInput("campaign_multi", "Campaign", choices = NULL)),
                     column(2, selectInput("outlet_multi", "Outlet", choices = NULL)),
                     column(2, selectInput("creative_multi", "Creative", choices = NULL))            
                   )
            )
          )
      ),
      fluidRow(
        column(width = 3,
               div(class = "section-card",
                   h4("Operation Mode", class = "section-title"),
                   div(class = "pretty-radio", 
                       prettyRadioButtons("sum_all_vars", "Add all variables",
                                          choices = c("Yes" = "true", "No" = "false"), 
                                          selected = "false", inline = TRUE, status = "primary")
                   ),
                   
                   h4("Variable Selection", class = "section-title"),
                   div(class = "variable-selection",
                       selectInput("kpi_multi", "KPI", choices = c("None"), selected = "None"),
                       
                       # Add KPI Transformation controls
                       h4("KPI Transformation", class = "section-title"),
                       radioButtons("kpi_transformation_multi", NULL,
                                    choices = c("None", "Logarithmic", "Moving Average"),
                                    selected = "None"),
                       
                       # Conditional panel for Moving Average window size
                       conditionalPanel(
                         condition = "input.kpi_transformation_multi == 'Moving Average'",
                         sliderInput("ma_window_multi", "Window Size (periods)", 
                                     min = 2, max = 30, value = 3, step = 1)
                       ),
                       
                       # # Lag and Decay parameters for KPI
                       # div(class = "transform-params",
                       #     div(class = "transform-param-item", 
                       #         numericInput("lag_kpi_multi", "KPI Lag", value = 0, min = 0, step = 1)),
                       #    
                       #     div(class = "transform-param-item", 
                       #         numericInput("decay_kpi_multi", "KPI Decay", value = 1, min = 0, max = 1, step = 0.1))
                       # ),
                       
                       # KPI Normalization (existing)
                       h4("KPI Normalization", class = "section-title"),
                       radioButtons("kpi_normalization_multi", "Normalization:",
                                    choices = c("None", "Division", "Subtraction"),
                                    selected = "None"),
                       
                       tags$div(
                         style = "font-size: 0.85rem; color: #666; margin-bottom: 15px;",
                         tags$p("Division: KPI/mean(KPI)"),
                         tags$p("Subtraction: KPI-mean(KPI)")
                       ),
                       
                       # Variable selection (existing)
                       selectInput("var1_multi", "Variable 1", choices = c("None"), selected = "None"),
                       selectInput("var2_multi", "Variable 2", choices = c("None"), selected = "None"),
                       selectInput("var3_multi", "Variable 3", choices = c("None"), selected = "None"),
                       selectInput("var4_multi", "Variable 4", choices = c("None" = "None"), selected = "None")
                   ),
                   conditionalPanel(
                     condition = "input.sum_all_vars == 'true'",
                     div(style = "margin-top: 15px;",
                         h4("Average Period Selection", class = "section-title"),
                         dateRangeInput("avg_period_date_multi", "Select Date Range for Average:",
                                        start = Sys.Date() - 365,
                                        end = Sys.Date(),
                                        separator = " to ")
                     ),
                     div(style = "margin-top: 15px; text-align: center;",
                         downloadButton("download_multivariate_sum", "Download Summed Data", 
                                        class = "btn-primary btn-block custom-download-btn",
                                        icon = icon("download"))
                     )
                   ),
                   conditionalPanel(
                     condition = "input.sum_all_vars == 'false'",
                     div(style = "margin-top: 15px; text-align: center;",
                         downloadButton("download_multivariate_individual", "Download Individual Data", 
                                        class = "btn-primary btn-block custom-download-btn",
                                        icon = icon("download"))
                     )
                   )
               ),
               conditionalPanel(condition = "input.sum_all_vars == 'true'",
                                div(class = "section-card",
                                    h4("Variable Transformation", class = "section-title"),
                                    radioButtons(
                                      inputId = "trans_var1", 
                                      label = "Transformation Type",
                                      choices = c("Linear", "S Origin", "S Shaped", "Index Exp", "Log", "Exp", "Power", "Moving Avg"),
                                      selected = "S Origin"
                                    )
                                )
               )
        ),
        column(width = 9,
               conditionalPanel(condition = "input.sum_all_vars == 'true'",
                                div(class = "section-card",
                                    h4("Transformation Settings", class = "section-title"),
                                    div(class = "transform-params",
                                        div(class = "transform-param-item", numericInput("decay_multi", "Decay", value = 1, min = 0, step = 0.1)),
                                        div(class = "transform-param-item", numericInput("lag_multi", "Lag", value = 0, min = 0)),
                                        div(class = "transform-param-item", numericInput("maxval_multi", "% MaxVal", value = 100, min = 0, step = 1)),
                                        div(class = "transform-param-item", numericInput("alpha_multi", "Alpha", value = 0.85, min = 0, step = 0.01)),
                                        div(class = "transform-param-item", numericInput("beta_multi", "Beta", value = 1, min = 0, step = 0.1))
                                    )
                                ),
                                fluidRow(
                                  column(6, div(class = "chart-box", h4("Summed Variables (Linear Flighting)", class = "chart-title"), plotlyOutput("sum_variables_chart", height = "320px"))),
                                  column(6, div(class = "chart-box", h4("Transformed Summed Variables vs KPI", class = "chart-title"), plotlyOutput("sum_variables_transf_chart", height = "320px")))
                                ),
                                conditionalPanel(
                                  condition = "input.trans_var1 == 'S Origin' || input.trans_var1 == 'S Shaped'",
                                  div(class = "chart-box", h4("S-Curve EDA (Multivariate)", class = "chart-title"), plotlyOutput("s_curve_multivariate_plot", height = "400px"))
                                ),
                                fluidRow(
                                  column(4, div(class = "chart-box", h4("Boxplot (Summed Variable)", class = "chart-title"), plotlyOutput("boxplot_multi_sum", height = "300px"))),
                                  column(4, div(class = "chart-box", h4("Suggested Max Value", class = "chart-title"), verbatimTextOutput("transformations_summary_multi", placeholder = TRUE))),
                                  column(4, div(class = "chart-box", h4("Correlation with KPI", class = "chart-title"), plotlyOutput("corr_with_kpi_multi_sum", height = "300px")))
                                )
               ),
               conditionalPanel(condition = "input.sum_all_vars == 'false'",
                                fluidRow(
                                  column(width = 12,
                                         div(class = "chart-box", 
                                             h4("Line Selected Variables", class = "chart-title"), 
                                             plotlyOutput("boxplot_multi", height = "350px"))
                                  )
                                ),
                                fluidRow(
                                  column(width = 12,
                                         div(class = "chart-box", 
                                             h4("Correlation Matrix", class = "chart-title"), 
                                             plotlyOutput("corr_matrix_multi", height = "350px"))
                                  )
                                )
               )
        )
      )
    )
  )
}