# Information_UI.R

ui_information <- function() {
  tagList(
    fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      # Section: Data Management
      div(class = "section-card",
          h4("Data Management:", class = "section-title"),
          div(class = "upload-zone",
              fileInput("file", NULL,
                        accept = c(".csv", ".RData"),
                        buttonLabel = "Select File",
                        placeholder = "Drag and Drop files here"
              ),
              tags$small("Supported format: CSV, RData", 
                         style = "display: block; margin-top: 0.5rem; color: #6c757d;")
          ),
          div(style = "margin-top: 1rem;",
              dateRangeInput("date_range_filter", "Date Range:",
                             start  = Sys.Date() - 30,
                             end    = Sys.Date(),
                             format = "yyyy-mm-dd",
                             language = "en"
              )
          )
      ),
      
      # Section: Variable Configuration
      div(class = "section-card",
          h4("Variable Configuration", class = "section-title"),
          div(class = "variable-config-container",
              div(class = "variable-group",
                  div(class = "variable-group-title", "KPI"),
                  selectInput("kpi", NULL,
                              choices = NULL,
                              selectize = TRUE,
                              width = "100%")
              ),
              conditionalPanel(
                condition = "input.kpi",
                div(
                  div(class = "variable-group",
                      div(class = "variable-group-title", "Media Variables"),
                      selectInput("media_vars", NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  selectize = TRUE,
                                  width = "100%")
                  ),
                  div(class = "variable-group",
                      div(class = "variable-group-title", "Spend Variables"),
                      selectInput("spend_vars", NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  selectize = TRUE,
                                  width = "100%")
                  ),
                  div(class = "variable-group",
                      div(class = "variable-group-title", "Base Variables"),
                      selectInput("base_vars", NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  selectize = TRUE,
                                  width = "100%")
                  )
                )
              )
          ),
          div(style = "margin-top: 1.5rem; text-align: right;",
              downloadButton("download_analytical", "Download Analytical File",
                             class = "custom-download-btn")
          )
      ),
      
      # Section: File Information
      div(class = "section-card",
          h4("File Information", class = "section-title"),
          div(class = "info-grid",
              div(class = "info-item",
                  div(class = "info-label", "Temporal Dimension"),
                  uiOutput("temporal_dimension_ui")
              ),
              div(class = "info-item",
                  div(class = "info-label", "Cross-Sectional Dimensions"),
                  uiOutput("cross_sectional_dimension_ui")
              )
          ),
          verbatimTextOutput("file_details")
      ),
      
      # Section: Summary Table
      div(class = "section-card",
          h4("Summary Table", class = "section-title"),
          div(class = "summary-table",
              withSpinner(
                DTOutput("consolidated_table"),
                type = 6,
                color = "#3498db",
                size = 1
              )
          ),
          div(style = "margin-top: 1rem; text-align: right;",
              downloadButton("download_consolidated", "Download Summary Table",
                             class = "custom-download-btn")
          )
      )
    )
  )
}
