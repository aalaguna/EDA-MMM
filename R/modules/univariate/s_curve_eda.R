# R/modules/univariate/s_curve_eda.R
# =============================================================================
# Módulo: s_curve_eda
# Renderiza los gráficos S-Curve EDA utilizando las funciones de transformations.R
# =============================================================================

render_s_curve_plots <- function(df, input) {
  req(df, input$variable_univ)
  validate(
    need(input$variable_univ != "N/A", "Seleccione una variable válida para la S-Curve."),
    need(input$transformation_univ %in% c("S Origin", "S Shaped"), 
         "Esta gráfica solo se muestra para 'S Origin' o 'S Shaped'.")
  )
  
  var_name <- input$variable_univ
  alpha <- input$alpha_univ
  beta <- input$beta_univ
  max_val_pct <- input$maxval_univ
  decay <- input$decay_univ
  lag <- input$lag_univ
  
  df_scurve <- df %>%
    mutate(Period = if ("Period" %in% names(.)) as.Date(Period) else as.Date(periodo)) %>%
    select(Period, value = !!sym(var_name))
  
  if (lag > 0) {
    if(lag >= nrow(df_scurve)) {
      df_scurve$value <- rep(NA, nrow(df_scurve))
    } else {
      df_scurve$value <- c(rep(NA, lag), head(df_scurve$value, -lag))
    }
  }
  
  df_scurve$value <- as.numeric(df_scurve$value) * decay
  
  if (nrow(df_scurve) == 0) {
    notifyUser("No hay datos disponibles para la S-Curve.", "error")
    return(NULL)
  }
  
  flighting_plot <- tryCatch({
    create_flighting_chart(
      data_chart  = df_scurve,
      alpha       = alpha,
      beta        = beta,
      max_val_pct = max_val_pct,
      decay       = decay,
      lag         = lag,
      var_name    = var_name
    )
  }, error = function(e) {
    notifyUser(paste("Error en Flighting Chart:", e$message), "error")
    return(NULL)
  })
  
  s_curve_plot <- tryCatch({
    create_s_curve_chart(
      data_chart  = df_scurve,
      alpha       = alpha,
      beta        = beta,
      max_val_pct = max_val_pct,
      decay       = decay,
      lag         = lag,
      var_name    = var_name
    )
  }, error = function(e) {
    notifyUser(paste("Error en S-Curve Chart:", e$message), "error")
    return(NULL)
  })
  
  if (is.null(flighting_plot) || is.null(s_curve_plot)) {
    return(NULL)
  }
  
  tryCatch({
    subplot(flighting_plot, s_curve_plot,
            nrows = 1,
            titleX = TRUE, titleY = TRUE) %>%
      layout(title = "S-Curve EDA")
  }, error = function(e) {
    notifyUser(paste("Error al combinar gráficas S-Curve:", e$message), "error")
    return(NULL)
  })
}