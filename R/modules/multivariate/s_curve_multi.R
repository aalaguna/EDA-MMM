# R/modules/multivariate/s_curve_multi.R
# =============================================================================
# Módulo: s_curve_multi
# Renderiza el gráfico S-Curve EDA para variables sumadas
# =============================================================================

render_s_curve_multi <- function(df, input) {
  req(df)
  validate(
    need(input$trans_var1 %in% c("S Origin", "S Shaped"), 
         "Esta gráfica solo se muestra para 'S Origin' o 'S Shaped'.")
  )
  
  # Preparar datos para S-Curve
  s_data <- df %>% 
    select(Period, sum_vars) %>% 
    mutate(Period = if ("Period" %in% names(df)) as.Date(Period) else as.Date(periodo)) %>%
    rename(value = sum_vars)
  
  # Crear S-Curve usando la función del módulo común
  tryCatch({
    create_s_curve_chart(
      data_chart = s_data,
      alpha = input$alpha_multi,
      beta = input$beta_multi,
      max_val_pct = input$maxval_multi,
      decay = input$decay_multi,
      lag = input$lag_multi,
      var_name = "Variables Sumadas"
    )
  }, error = function(e) {
    notifyUser(paste("Error en S-Curve Multivariate:", e$message), "error")
    return(NULL)
  })
}