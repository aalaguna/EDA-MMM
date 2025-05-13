# =============================================================================
# KPI Transformation and Normalization tab
# =============================================================================

ui_kpi_transformation <- function() {
  tagList(
    fluidPage(
      div(class = "section-card", h4("Global Filters", class = "section-title"),
          fluidRow(
            column(4, dateRangeInput("date_range_kpi", "Date Range:",
                                    start = Sys.Date() - 30,
                                    end = Sys.Date(),
                                    format = "yyyy-mm-dd",
                                    language = "en")
            ),
            column(8,
                   fluidRow(
                     column(3, selectInput("geography_kpi", "Geography", choices = NULL)),
                     column(3, selectInput("product_kpi", "Product", choices = NULL)),
                     column(2, selectInput("campaign_kpi", "Campaign", choices = NULL)),
                     column(2, selectInput("outlet_kpi", "Outlet", choices = NULL)),
                     column(2, selectInput("creative_kpi", "Creative", choices = NULL))            
                   )
            )
          )
      ),
      fluidRow(
        column(width = 3,
               div(class = "section-card",
                   h4("KPI Selection", class = "section-title"),
                   selectInput("kpi_selection", "KPI Variable", choices = NULL),
                   
                   h4("Transformation", class = "section-title"),
                   radioButtons("transformation_kpi", NULL, 
                                choices = c("None", "Logarithmic", "Moving Average"),
                                selected = "None"),
                   
                   # Show extra controls for moving average only when it's selected
                   conditionalPanel(
                     condition = "input.transformation_kpi == 'Moving Average'",
                     div(style = "margin-top: 10px;",
                         sliderInput("ma_window", "Window Size (periods)", 
                                     min = 2, max = 30, value = 3, step = 1)
                     )
                   ),
                   
                   h4("Normalization", class = "section-title"),
                   radioButtons("normalization_kpi", NULL, 
                                choices = c("None", "Division by Mean", "Subtraction of Mean"),
                                selected = "None"),
                   
                   h4("Dimension Analysis", class = "section-title"),
                   selectInput("dimension_select", "Select Dimension", 
                               choices = NULL, 
                               selected = NULL),
                   
                   # Download button for transformed KPI
                   div(style = "margin-top: 15px; text-align: center;",
                       downloadButton("download_transformed_kpi", "Download Transformed KPI", 
                                     class = "btn-primary btn-block custom-download-btn",
                                     icon = icon("download"))
                   ),
                   
                   # Download button for normalized KPI
                   div(style = "margin-top: 15px; text-align: center;",
                       downloadButton("download_normalized_kpi", "Download Normalized KPI", 
                                     class = "btn-primary btn-block custom-download-btn",
                                     icon = icon("download"))
                   )
               )
        ),
        column(width = 9,
               fluidRow(
                 column(12, 
                        div(class = "chart-box",
                            h4("KPI Time Series", class = "chart-title"),
                            plotlyOutput("kpi_time_series", height = "350px") %>% 
                              withSpinner(color = "#0275d8", type = 5)
                        )
                 )
               ),
               fluidRow(
                 column(6, 
                        div(class = "chart-box",
                            h4("Transformed KPI", class = "chart-title"),
                            plotlyOutput("transformed_kpi_plot", height = "300px") %>% 
                              withSpinner(color = "#0275d8", type = 5)
                        )
                 ),
                 column(6, 
                        div(class = "chart-box",
                            h4("Normalized KPI", class = "chart-title"),
                            plotlyOutput("normalized_kpi_plot", height = "300px") %>% 
                              withSpinner(color = "#0275d8", type = 5)
                        )
                 )
               ),
               fluidRow(
                 column(6,
                        div(class = "chart-box",
                            h4("KPI Distribution", class = "chart-title"),
                            plotlyOutput("kpi_distribution", height = "300px") %>% 
                              withSpinner(color = "#0275d8", type = 5)
                        )
                 ),
                 column(6,
                        div(class = "chart-box",
                            h4("KPI Summary Statistics", class = "chart-title"),
                            div(style = "height: 300px; overflow-y: auto;",
                                DTOutput("kpi_summary_table") %>% 
                                  withSpinner(color = "#0275d8", type = 5)
                            )
                        )
                 )
               ),
               # New section for dimension analysis
               fluidRow(
                 column(6, 
                        div(class = "chart-box",
                            h4("KPI by Dimension - Total Values", class = "chart-title"),
                            plotlyOutput("kpi_by_dimension_total", height = "350px") %>% 
                              withSpinner(color = "#0275d8", type = 5)
                        )
                 ),
                 column(6, 
                        div(class = "chart-box",
                            h4("KPI by Dimension - Percentage", class = "chart-title"),
                            plotlyOutput("kpi_by_dimension_percent", height = "350px") %>% 
                              withSpinner(color = "#0275d8", type = 5)
                        )
                 )
               )
        )
      )
    )
  )
}