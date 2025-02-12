# =============================================================================
# Interfaz del panel Information.
# Define la UI para la pestaña de Información, mostrando la carga de datos,
# configuración de variables y detalles relevantes del archivo.
# =============================================================================

library(shiny)
library(shinycssloaders)

ui_information <- function() {
  tagList(
    fluidPage(
      # Se carga el CSS externo ubicado en www/styles.css
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),
      
      # Sección: Gestión de Datos
      div(class = "section-card",
          h4("Data Management:", class = "section-title"),
          div(class = "upload-zone",
              fileInput("file", NULL,
                        accept = c(".csv", ".RData"),
                        buttonLabel = "Select File",
                        placeholder = "Drag and Drop files here"
              ),
              tags$small("FSupported format: CSV, RData", 
                         style = "display: block; margin-top: 0.5rem; color: #6c757d;")
          ),
          div(style = "margin-top: 1rem;",
              dateRangeInput("date_range_filter", "Date Range:",
                             start = Sys.Date() - 30,
                             end = Sys.Date(),
                             format = "yyyy-mm-dd",
                             language = "en"
              )
          )
      ),
      
      # Sección: Configuración de Variables
      div(class = "section-card",
          h4("Variable configuration", class = "section-title"),
          div(class = "variable-config-container",
              # KPI Principal
              div(class = "variable-group",
                  div(class = "variable-group-title", "KPI"),
                  selectInput("kpi", NULL,
                              choices = NULL,
                              selectize = TRUE,
                              width = "100%"
                  )
              ),
              
              # Variables Agrupadas
              conditionalPanel(
                condition = "input.kpi",
                div(
                  div(class = "variable-group",
                      div(class = "variable-group-title", "Media Variables"),
                      selectInput("media_vars", NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  selectize = TRUE,
                                  width = "100%"
                      )
                  ),
                  
                  div(class = "variable-group",
                      div(class = "variable-group-title", "Spend Variables"),
                      selectInput("spend_vars", NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  selectize = TRUE,
                                  width = "100%"
                      )
                  ),
                  
                  div(class = "variable-group",
                      div(class = "variable-group-title", "Base Variables"),
                      selectInput("base_vars", NULL,
                                  choices = NULL,
                                  multiple = TRUE,
                                  selectize = TRUE,
                                  width = "100%"
                      )
                  )
                )
              )
          ),
          
          div(style = "margin-top: 1.5rem; text-align: right;",
              downloadButton("download_analytical", "Download Analytical File",
                             class = "custom-btn"
              )
          )
      ),
      
      # Sección: Información del Archivo
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
      
      # Sección: Tabla Resumen
      div(class = "section-card",
          h4("Summary Table", class = "section-title"),
          div(class = "summary-table",
              # Se envuelve la tabla en un spinner para indicar que se está cargando la información
              shinycssloaders::withSpinner(
                tableOutput("consolidated_table"),
                type = 6,
                color = "#3498db",
                size = 1
              )
          ),
          div(style = "margin-top: 1rem; text-align: right;",
              downloadButton("download_consolidated", "Download Summary Table",
                             class = "custom-btn"
              )
          )
      )
    )
  )
}