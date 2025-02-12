# R/ui/multivariate/multivariate_ui.R
# =============================================================================
# Multivariate Panel UI.
# Returns the UI for multivariate analysis, with filter controls,
# variable selection, and visualization of graphs for summed or individual analysis.
# =============================================================================

ui_multivariate <- function() {
  tagList(
    fluidPage(
      # Global Filters - Horizontal Layout
      div(
        class = "section-card",
        h4("Global Filters", class = "section-title"),
        fluidRow(
          column(2, selectInput("geography_multi", "Geography", choices = NULL)),
          column(2, selectInput("product_multi", "Product", choices = NULL)),
          column(2, selectInput("campaign_multi", "Campaign", choices = NULL)),
          column(2, selectInput("outlet_multi", "Outlet", choices = NULL)),
          column(2, selectInput("creative_multi", "Creative", choices = NULL))
        )
      ),
      # Main Content
      fluidRow(
        # Left Panel - Variable Selection
        column(
          width = 3,
          div(
            class = "section-card",
            h4("Variable Selection", class = "section-title"),
            div(
              class = "pretty-radio",
              prettyRadioButtons("sum_all_vars", "Sum all variables",
                                 choices = c("Yes" = "true", "No" = "false"),
                                 # Defecto no
                                 selected = "false",
                                 inline = TRUE, status = "primary")
            ),
            div(
              class = "variable-selection",
              selectInput("kpi_multi", "KPI", choices = NULL),
              selectInput("var1_multi", "Variable 1", choices = NULL),
              selectInput("var2_multi", "Variable 2", choices = c("None" = "None")),
              selectInput("var3_multi", "Variable 3", choices = c("None" = "None")),
              selectInput("var4_multi", "Variable 4", choices = c("None" = "None"))
            ),
            # Transformation Options
            conditionalPanel(
              condition = "input.sum_all_vars == 'true'",
              h4("Variable Transformation", class = "section-title"),
              selectInput("trans_var1", "Transform Summed Data",
                          choices = c("Linear", "S Origin", "S Shaped", "Index Exp",
                                      "Log", "Exp", "Power", "Moving Avg"),
                          selected = "Linear"),
              selectInput("trans_var2", "Transform Variable 2",
                          choices = c("Linear", "S Origin", "S Shaped", "Index Exp",
                                      "Log", "Exp", "Power", "Moving Avg"),
                          selected = "Linear"),
              selectInput("trans_var3", "Transform Variable 3",
                          choices = c("Linear", "S Origin", "S Shaped", "Index Exp",
                                      "Log", "Exp", "Power", "Moving Avg"),
                          selected = "Linear"),
              selectInput("trans_var4", "Transform Variable 4",
                          choices = c("Linear", "S Origin", "S Shaped", "Index Exp",
                                      "Log", "Exp", "Power", "Moving Avg"),
                          selected = "Linear")
            )
          )
        ),
        # Right Panel - Charts and Settings
        column(
          width = 9,
          conditionalPanel(
            condition = "input.sum_all_vars == 'true'",
            div(
              class = "section-card",
              h4("Transformation Settings", class = "section-title"),
              div(
                class = "transform-params",
                div(class = "transform-param-item",
                    numericInput("decay_multi", "Decay", value = 1, min = 0, step = 0.1)
                ),
                div(class = "transform-param-item",
                    numericInput("lag_multi", "Lag", value = 0, min = 0)
                ),
                div(class = "transform-param-item",
                    numericInput("maxval_multi", "% MaxVal", value = 100, min = 0, step = 1)
                ),
                div(class = "transform-param-item",
                    numericInput("alpha_multi", "Alpha", value = 0.85, min = 0, step = 0.01)
                ),
                div(class = "transform-param-item",
                    numericInput("beta_multi", "Beta", value = 1, min = 0, step = 0.1)
                )
              )
            ),
            fluidRow(
              column(
                6,
                div(
                  class = "chart-box",
                  h4("Summed Variables (Linear Flighting)", class = "chart-title"),
                  plotlyOutput("sum_variables_chart", height = "320px")
                )
              ),
              column(
                6,
                div(
                  class = "chart-box",
                  h4("Transformed Summed Variables", class = "chart-title"),
                  plotlyOutput("sum_variables_transf_chart", height = "320px")
                )
              )
            ),
            conditionalPanel(
              condition = "input.trans_var1 == 'S Origin' || input.trans_var1 == 'S Shaped'",
              div(
                class = "chart-box",
                h4("S-Curve EDA Multivariate", class = "chart-title"),
                plotlyOutput("s_curve_multivariate_plot", height = "400px")
              )
            ),
            fluidRow(
              column(
                6,
                div(
                  class = "chart-box",
                  h4("Boxplot (Summed Variable)", class = "chart-title"),
                  plotlyOutput("boxplot_multi_sum", height = "300px")
                )
              ),
              column(
                6,
                div(
                  class = "chart-box",
                  h4("Correlation with KPI", class = "chart-title"),
                  plotlyOutput("corr_with_kpi_multi_sum", height = "300px")
                )
              )
            )
          ),
          conditionalPanel(
            condition = "input.sum_all_vars == 'false'",
            fluidRow(
              column(
                6,
                div(
                  class = "chart-box",
                  h4("Boxplot of Selected Variables", class = "chart-title"),
                  plotlyOutput("boxplot_multi", height = "300px")
                )
              ),
              column(
                6,
                div(
                  class = "chart-box",
                  h4("Correlation Matrix", class = "chart-title"),
                  plotlyOutput("corr_matrix_multi", height = "300px")
                )
              )
            )
          )
        )
      )
    )
  )
}
