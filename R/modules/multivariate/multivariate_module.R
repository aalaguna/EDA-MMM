# R/modules/multivariate/multivariate_module_server.R
# =============================================================================
# Módulo: multivariate_module_server
# Orquesta la lógica del panel multivariado
# =============================================================================

source("R/modules/common/transformations.R")
source("R/modules/multivariate/data_processing.R")
source("R/modules/multivariate/sum_variables_charts.R")
source("R/modules/multivariate/s_curve_multi.R")
source("R/modules/multivariate/correlation_plots.R")
source("R/modules/multivariate/boxplots_multi.R")

multivariate_module_server <- function(input, output, session, rv) {
  # Reactive para datos sumados
  summed_data <- reactive({
    process_summed_data(rv$filtered_data_multi(), input)
  })

  # Reactive para datos transformados
  transformed_data <- reactive({
    process_transformed_data(summed_data(), input)
  })

  # Gráfico de Variables Sumadas (Flighting Lineal)
  output$sum_variables_chart <- renderPlotly({
    render_sum_variables_chart(summed_data())
  })

  # Gráfico de Variables Sumadas Transformadas
  output$sum_variables_transf_chart <- renderPlotly({
    render_transformed_variables_chart(transformed_data())
  })

  # Boxplot de Variable Sumada
  output$boxplot_multi_sum <- renderPlotly({
    render_summed_boxplot(summed_data())
  })

  # Correlación con KPI
  output$corr_with_kpi_multi_sum <- renderPlotly({
    render_kpi_correlation(summed_data(), input$kpi_multi)
  })

  # S-Curve EDA Multivariate
  output$s_curve_multivariate_plot <- renderPlotly({
    render_s_curve_multi(summed_data(), input)
  })

  # Boxplots para variables individuales
  output$boxplot_multi <- renderPlotly({
    render_individual_boxplots(rv$filtered_data_multi(), input)
  })

  # Matriz de correlación
  output$corr_matrix_multi <- renderPlotly({
    render_correlation_matrix(rv$filtered_data_multi(), input)
  })
}