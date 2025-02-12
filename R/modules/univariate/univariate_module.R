# R/modules/univariate/univariate_module_server.R
# =============================================================================
# Módulo: univariate_module_server
# Orquesta la lógica del panel univariado, aplicando filtros por Geografía,
# y utilizando los filtros compuestos para determinar la variable a analizar.
# =============================================================================

source("R/modules/common/transformations.R")
source("R/modules/univariate/data_filtering.R")
source("R/modules/univariate/variable_flighting_chart.R")
source("R/modules/univariate/transformation_chart.R")
source("R/modules/univariate/boxplot_univariate.R")
source("R/modules/univariate/correlation_plot.R")
source("R/modules/univariate/s_curve_eda.R")
source("R/modules/univariate/transformations_summary.R")

univariate_module_server <- function(input, output, session, rv) {
  # Filtrar datos según Geografía
  filtered_geography_data <- reactive({
    filter_geography_data(rv$data, input$geography_univ)
  })
  
  # Renderizado de gráficos principales
  output$variable_flighting_chart <- renderPlotly({
    req(filtered_geography_data(), input$kpi_univ, input$variable_univ)
    validate(
      need(input$variable_univ != "N/A", "Seleccione una variable válida para el análisis.")
    )
    render_variable_flighting(filtered_geography_data(), input$kpi_univ, 
                            input$variable_univ, input$geography_univ)
  })
  
  output$var_transf_chart <- renderPlotly({
    req(filtered_geography_data(), input$variable_univ, input$transformation_univ)
    validate(
      need(input$variable_univ != "N/A", "Seleccione una variable válida para la transformación.")
    )
    render_transformation_chart(filtered_geography_data(), input$variable_univ,
                              input$transformation_univ, input$lag_univ,
                              input$decay_univ, input$alpha_univ, 
                              input$beta_univ, input$maxval_univ,
                              input$geography_univ)
  })
  
  output$boxplot_univ <- renderPlot({
    req(filtered_geography_data(), input$variable_univ)
    validate(
      need(input$variable_univ != "N/A", "Seleccione una variable válida para el boxplot.")
    )
    render_boxplot_univariate(filtered_geography_data(), input$variable_univ,
                            input$geography_univ)
  })
  
  output$transformations_summary_univ <- renderPrint({
    req(input$transformation_univ, filtered_geography_data(), input$variable_univ)
    validate(
      need(input$variable_univ != "N/A", "Seleccione una variable válida para el resumen.")
    )
    render_transformations_summary(filtered_geography_data(), input)
  })
  
  # S-Curve EDA Plot con la lógica exacta del original
  output$s_curve_univariate_plot <- renderPlotly({
    req(filtered_geography_data(), input$variable_univ)
    validate(
      need(input$variable_univ != "N/A", "Seleccione una variable válida para la S-Curve."),
      need(input$transformation_univ %in% c("S Origin", "S Shaped"), 
           "Esta gráfica solo se muestra para 'S Origin' o 'S Shaped'.")
    )
    render_s_curve_plots(filtered_geography_data(), input)
  })
  
  output$corr_kpi_var_univ <- renderPlot({
    req(filtered_geography_data(), input$kpi_univ, input$variable_univ)
    validate(
      need(input$variable_univ != "N/A", "Seleccione una variable válida para calcular la correlación.")
    )
    render_correlation_plot(filtered_geography_data(), input$kpi_univ,
                          input$variable_univ)
  })
}