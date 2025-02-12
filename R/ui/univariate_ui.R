# R/ui/univariate/univariate_ui.R
# =============================================================================
# Univariate Panel UI.
# Returns the UI for univariate analysis, with filter controls,
# variable selection, transformation options, and graphs.
# =============================================================================

ui_univariate <- function() {
  tagList(
    fluidPage(
      # Global Filters
      div(
        class = "section-card",
        h4("Global Filters", class = "section-title"),
        fluidRow(
          column(2, selectInput("geography_univ", "Geography", choices = NULL)),
          column(2, selectInput("product_univ", "Product", choices = NULL)),
          column(2, selectInput("campaign_univ", "Campaign", choices = NULL)),
          column(2, selectInput("outlet_univ", "Outlet", choices = NULL)),
          column(2, selectInput("creative_univ", "Creative", choices = NULL))
        )
      ),
      
      # Main Content
      fluidRow(
        # Left Panel
        column(
          width = 3,
          div(
            class = "section-card",
            h4("Variable Selection", class = "section-title"),
            selectInput("kpi_univ", "KPI", choices = NULL),
            selectInput("variable_univ", "Variable", choices = NULL),
            h4("Transformation", class = "section-title"),
            radioButtons("transformation_univ", NULL,
                         choices = c("Linear", "S Origin", "S Shaped", "Index Exp",
                                     "Log", "Exp", "Power", "Moving Avg"),
                         selected = "Linear")
          )
        ),
        # Right Panel
        column(
          width = 9,
          div(
            class = "section-card",
            h4("Transformation Settings", class = "section-title"),
            div(
              class = "transform-params",
              div(class = "transform-param-item",
                  numericInput("decay_univ", "Decay", value = 1, min = 0, step = 0.1)
              ),
              div(class = "transform-param-item",
                  numericInput("lag_univ", "Lag", value = 0, min = 0)
              ),
              div(class = "transform-param-item",
                  numericInput("maxval_univ", "% MaxVal", value = 100, min = 0, step = 1)
              ),
              div(class = "transform-param-item",
                  numericInput("alpha_univ", "Alpha", value = 0.85, min = 0, step = 0.01)
              ),
              div(class = "transform-param-item",
                  numericInput("beta_univ", "Beta", value = 1, min = 0, step = 0.1)
              )
            )
          ),
          # S-Curve EDA Section
          conditionalPanel(
            condition = "input.transformation_univ == 'S Origin' || input.transformation_univ == 'S Shaped'",
            div(
              class = "chart-box",
              h4("S-Curve EDA", class = "chart-title"),
              plotlyOutput("s_curve_univariate_plot", height = "350px")
            )
          ),
          # Variable Flighting and Transformed Variable (for other transformation types)
          conditionalPanel(
            condition = "!(input.transformation_univ == 'S Origin' || input.transformation_univ == 'S Shaped')",
            fluidRow(
              column(
                6,
                div(
                  class = "chart-box",
                  h4("Variable Flighting", class = "chart-title"),
                  plotlyOutput("variable_flighting_chart", height = "300px")
                )
              ),
              column(
                6,
                div(
                  class = "chart-box",
                  h4("Transformed Variable", class = "chart-title"),
                  plotlyOutput("var_transf_chart", height = "300px")
                )
              )
            )
          ),
          # Bottom Charts: Boxplot (puede ser un violin plot), Summary, and Correlation
          fluidRow(
            column(
              4,
              div(
                class = "chart-box",
                # Violin or Boxplot
                h4("Boxplot", class = "chart-title"),
                plotOutput("boxplot_univ", height = "300px")
              )
            ),
            column(
              4,
              div(
                class = "chart-box",
                h4("Suggested Max Value", class = "chart-title"),
                verbatimTextOutput("transformations_summary_univ", placeholder = TRUE)
              )
            ),
            column(
              4,
              div(
                class = "chart-box",
                h4("KPI vs Variable Correlation", class = "chart-title"),
                plotOutput("corr_kpi_var_univ", height = "300px")
              )
            )
          )
        )
      )
    )
  )
}
