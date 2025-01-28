library(shiny)
library(shinythemes)
library(plotly)
library(shinyjs)
library(shinyWidgets)

# Define UI
ui <- fluidPage(
  # Load required dependencies
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;500;700&display=swap", rel = "stylesheet"),
    # Add meta tags for responsive design
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    # Add custom CSS for improved styling
    tags$style(HTML("
      .tooltip { max-width: 300px; text-align: left; }
      .custom-sidebar { max-height: calc(100vh - 100px); overflow-y: auto; }
      .info-box { border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); padding: 15px; margin-bottom: 20px; }
      .chart-box { background: white; border-radius: 8px; padding: 15px; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      .section-title { border-bottom: 2px solid #f0f0f0; padding-bottom: 10px; margin-bottom: 20px; }
      .custom-download-btn { background-color: #007bff; color: white; border: none; border-radius: 4px; }
      .custom-download-btn:hover { background-color: #0056b3; }
    "))
  ),
  
  theme = shinytheme("flatly"),
  
  # Header
  div(class = "app-header bg-primary text-white py-4",
      titlePanel(
        div(class = "title-container text-center",
            h1("EDA", class = "main-title display-4"),
            p("Exploratory Data Analysis Tool", class = "subtitle lead")
        )
      )
  ),
  
  # En lugar de un sidebarLayout fijo, usamos un layout condicional:
  # - Si estamos en la pestaña 'Información', mostramos Data Management en la parte superior.
  # - En las demás pestañas, ocultamos el panel y usamos el espacio completo.
  
  navbarPage(
    id = "main-tabs",
    title = NULL,
    theme = shinytheme("flatly"),
    
    # Pestaña INFORMACIÓN (única pestaña con Data Management visible)
    tabPanel(
      "Information",
      fluidPage(
        # Data Management en la parte superior
        fluidRow(
          column(
            width = 12,
            wellPanel(
              h4("Data Management", class = "section-title"),
              div(
                title = "Upload your analytical file (CSV or RData format)",
                fileInput(
                  "file",
                  "Upload Analytical",
                  accept = c(".csv", ".RData"),
                  buttonLabel = "Browse...",
                  placeholder = "No file selected"
                )
              ),
              tags$small("Supported formats: CSV, RData", class = "text-muted"),
              
              # Selector de Fecha
              dateRangeInput(
                "date_range_filter",
                "Select Date Range:",
                start = Sys.Date() - 30,
                end = Sys.Date(),
                format = "yyyy-mm-dd"
              ),
              
              # Variables principales (KPI, media, spend, base)
              div(
                h4("Variable Configuration", class = "section-title"),
                div(
                  title = "Select the main KPI for analysis",
                  selectInput("kpi", "Select KPI", choices = NULL)
                ),
                div(
                  title = "Select one or more media variables",
                  selectInput("media_vars", "Select Media Variables",
                              choices = NULL,
                              multiple = TRUE)
                ),
                div(
                  title = "Select one or more spend variables",
                  selectInput("spend_vars", "Select Spend Variables",
                              choices = NULL,
                              multiple = TRUE)
                ),
                div(
                  title = "Select one or more base variables",
                  selectInput("base_vars", "Select Base Variables",
                              choices = NULL,
                              multiple = TRUE)
                )
              ),
              
              # Botón de descarga
              div(
                downloadButton(
                  "download_analytical",
                  "Download Analytical",
                  class = "custom-download-btn btn-block"
                )
              )
            )
          )
        ),
        
        
        # Configuration and file information --------------------------------------
        
        fluidRow(
          column(
            width = 12,
            wellPanel(
              h4("File Information", class = "section-title"),
              
              # Temporal Dimension
              div(
                strong("Temporal Dimension:"), # Texto en negrilla
                uiOutput("temporal_dimension_ui") # Aquí se renderiza el texto dinámico
              ),
              
              # Cross Sectional Dimension
              div(
                strong("Cross Sectional Dimension:"), # Texto en negrilla
                uiOutput("cross_sectional_dimension_ui") # Aquí se renderiza el texto dinámico
              ),
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            wellPanel(
              h4("Summary Table", class = "section-title"),
              tableOutput("consolidated_table"), # Output de tabla consolidada
              div(
                class = "text-right mt-3",
                downloadButton(
                  "download_consolidated",
                  "Download Summary Table",
                  class = "custom-download-btn"
                )
              )
            )
          )
        )
      )
    ),
    
    
    
    # UNIVARIATE TAB ----------------------------------------------------------
    
    tabPanel("Univariate",
             fluidPage(
               # Panel superior para Geography, Product, Campaign, Outlet, Creative
               fluidPage(
                 fluidRow(
                   column(12,
                          wellPanel(
                            h4("Global Filters", class = "section-title"),
                            fluidRow(
                              column(2,
                                     selectInput("geography_univ", "Geography", choices = NULL)
                              ),
                              column(2,
                                     selectInput("product_univ", "Product", choices = c("Product A", "Product B", "Product C"))
                              ),
                              column(2,
                                     selectInput("campaign_univ", "Campaign", choices = c("Campaign 1", "Campaign 2", "Campaign 3"))
                              ),
                              column(2,
                                     selectInput("outlet_univ", "Outlet", choices = c("Outlet X", "Outlet Y", "Outlet Z"))
                              ),
                              column(2,
                                     selectInput("creative_univ", "Creative", choices = c("Creative 1", "Creative 2", "Creative 3"))
                              )
                            )
                          )
                   )
                 )
               ),
               
               # Contenido principal
               fluidRow(
                 # Variable Selection a la izquierda
                 column(3,
                        wellPanel(
                          h4("Variable Selection", class = "section-title"),
                          selectInput("kpi_univ", "KPI", choices = NULL),
                          selectInput("variable_univ", "Variable", choices = NULL),
                          
                          h4("Transformation", class = "section-title mt-4"),
                          radioButtons("transformation_univ", NULL,
                                       choices = c("Linear", "S Origin",
                                                   "S Shaped", "Index Exp",
                                                   "Log", "Exp", "Power",
                                                   "Moving Avg"),
                                       selected = "Linear")
                        )
                 ),
                 
                 # Transformación Settings a la derecha
                 column(9,
                        fluidRow(
                          column(12,
                                 wellPanel(
                                   h4("Transformation Settings", class = "section-title"),
                                   fluidRow(
                                     column(2,
                                            numericInput("decay_univ", "Decay", value = 1, min = 0, step = 0.1)
                                     ),
                                     column(2,
                                            numericInput("lag_univ", "Lag", value = 0, min = 0)
                                     ),
                                     conditionalPanel(
                                       condition = "input.transformation_univ == 'S Origin' || input.transformation_univ == 'S Shaped'",
                                       column(2,
                                              numericInput("alpha_univ", "Alpha", value = 0.85, min = 0, step = 0.01)
                                       ),
                                       column(2,
                                              numericInput("beta_univ", "Beta", value = 1, min = 0, step = 0.1)
                                       ),
                                       column(2,
                                              numericInput("maxval_univ", "% MaxVal", value = 100, min = 0, step = 1)
                                       )
                                     )
                                   )
                                 )
                          )
                        ),
                        
                        # Dinámica para mostrar S-Curve o las gráficas normales
                        fluidRow(
                          conditionalPanel(
                            condition = "input.transformation_univ == 'S Origin' || input.transformation_univ == 'S Shaped'",
                            column(12,
                                   div(class = "chart-box mt-3",
                                       h4("S-Curve EDA", class = "chart-title"),
                                       plotlyOutput("s_curve_univariate_plot", height = "300px")
                                   )
                            )
                          ),
                          conditionalPanel(
                            condition = "!(input.transformation_univ == 'S Origin' || input.transformation_univ == 'S Shaped')",
                            column(6,
                                   div(class = "chart-box",
                                       h4("Variable Flighting", class = "chart-title"),
                                       plotlyOutput("variable_flighting_chart", height = "300px")
                                   )
                            ),
                            column(6,
                                   div(class = "chart-box",
                                       h4("Transformed Variable", class = "chart-title"),
                                       plotlyOutput("var_transf_chart", height = "300px")
                                   )
                            )
                          )
                        )
                 )
               ),
               
               # Boxplot y transformaciones aplicadas alineadas y con el mismo tamaño que Variable Flighting
               fluidRow(
                 column(6,
                        div(class = "chart-box",
                            h4("Boxplot", class = "chart-title"),
                            plotOutput("boxplot_univ", height = "300px")  # Tamaño uniforme
                        )
                 ),
                 column(6,
                        div(class = "chart-box",
                            h4("Suggested Max Value", class = "chart-title"),
                            verbatimTextOutput("transformations_summary_univ", placeholder = TRUE)
                        )
                 )
               )
             )
    ),
    
    
    # MULTIVARIATE TAB --------------------------------------------------------
    
    tabPanel("Multivariate",
             fluidPage(
               # Global Filters
               fluidRow(
                 column(12,
                        wellPanel(
                          h4("Global Filters", class = "section-title"),
                          fluidRow(
                            column(2, selectInput("geography_multi", "Geography", choices = NULL)),
                            column(2, selectInput("product_multi", "Product", choices = c("Product A", "Product B", "Product C"))),
                            column(2, selectInput("campaign_multi", "Campaign", choices = c("Campaign 1", "Campaign 2", "Campaign 3"))),
                            column(2, selectInput("outlet_multi", "Outlet", choices = c("Outlet X", "Outlet Y", "Outlet Z"))),
                            column(2, selectInput("creative_multi", "Creative", choices = c("Creative 1", "Creative 2", "Creative 3")))
                          )
                        )
                 )
               ),
               
               # Main Content
               fluidRow(
                 # Variable Selection
                 column(3,
                        wellPanel(
                          h4("Variable Selection", class = "section-title"),
                          prettyRadioButtons("sum_all_vars", "Sum all variables",
                                             choices = c("Yes" = "true", "No" = "false"),
                                             inline = TRUE, status = "primary"),
                          selectInput("kpi_multi", "KPI", choices = NULL),
                          selectInput("var1_multi", "Variable 1", choices = NULL),
                          selectInput("var2_multi", "Variable 2", choices = NULL),
                          selectInput("var3_multi", "Variable 3", choices = NULL),
                          selectInput("var4_multi", "Variable 4", choices = c("None" = "None")),
                          
                          h4("Transformation", class = "section-title mt-4"),
                          radioButtons("transformation_multi", NULL,
                                       choices = c("Linear", "S Origin", "S Shaped", "Index Exp",
                                                   "Log", "Exp", "Power", "Moving Avg"),
                                       selected = "Linear")
                        )
                 ),
                 
                 # Transformation Settings and Charts
                 column(9,
                        fluidRow(
                          column(12,
                                 wellPanel(
                                   h4("Transformation Settings", class = "section-title"),
                                   fluidRow(
                                     column(2, numericInput("decay_multi", "Decay", value = 1, min = 0, step = 0.1)),
                                     column(2, numericInput("lag_multi", "Lag", value = 0, min = 0)),
                                     conditionalPanel(
                                       condition = "input.transformation_multi == 'S Origin' || input.transformation_multi == 'S Shaped'",
                                       column(2, numericInput("alpha_multi", "Alpha", value = 0.85, min = 0, step = 0.01)),
                                       column(2, numericInput("beta_multi", "Beta", value = 1, min = 0, step = 0.1)),
                                       column(2, numericInput("maxval_multi", "% MaxVal", value = 100, min = 0, step = 1))
                                     )
                                   )
                                 )
                          )
                        ),
                        
                        # Dynamic Chart Display
                        fluidRow(
                          conditionalPanel(
                            condition = "input.transformation_multi == 'S Origin' || input.transformation_multi == 'S Shaped'",
                            column(12,
                                   div(class = "chart-box mt-3",
                                       h4("S-Curve EDA", class = "chart-title"),
                                       plotlyOutput("s_curve_multivariate_plot", height = "300px")
                                   )
                            )
                          ),
                          conditionalPanel(
                            condition = "!(input.transformation_multi == 'S Origin' || input.transformation_multi == 'S Shaped')",
                            column(6,
                                   div(class = "chart-box",
                                       h4("Variable Flighting", class = "chart-title"),
                                       plotlyOutput("variable_flighting_chart_multi", height = "300px")
                                   )
                            ),
                            column(6,
                                   div(class = "chart-box",
                                       h4("Transformed Variable", class = "chart-title"),
                                       plotlyOutput("var_transf_chart_multi", height = "300px")
                                   )
                            )
                          )
                        )
                 )
               ),
               
               # Boxplot and Summary
               fluidRow(
                 column(6,
                        div(class = "chart-box",
                            h4("Boxplot", class = "chart-title"),
                            plotOutput("boxplot_multi", height = "300px")
                        )
                 ),
                 column(6,
                        div(class = "chart-box",
                            h4("Suggested Max Value", class = "chart-title"),
                            verbatimTextOutput("transformations_summary_multi", placeholder = TRUE)
                        )
                 )
               )
             )
    )
  )
)
