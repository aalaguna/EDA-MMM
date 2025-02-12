# R/modules/univariate/transformation_chart.R
# =============================================================================
# Módulo: transformation_chart
# Renderiza el gráfico de la variable transformada
# =============================================================================

render_transformation_chart <- function(df, variable_univ, transformation_univ, lag_univ, 
                                     decay_univ, alpha_univ, beta_univ, maxval_univ, 
                                     geography_univ) {
  req(df, variable_univ, transformation_univ)
  validate(
    need(variable_univ != "N/A", "Seleccione una variable válida para la transformación.")
  )
  
  var_name <- variable_univ
  data_vec <- df[[var_name]]
  validate(
    need(length(data_vec) > 0, "No hay datos disponibles para transformar.")
  )
  
  data_trans <- as.numeric(data_vec)
  if (lag_univ > 0) {
    if (lag_univ >= length(data_trans)) {
      data_trans <- rep(NA, length(data_trans))
    } else {
      data_trans <- c(rep(NA, lag_univ), head(data_trans, -lag_univ))
    }
  }
  data_trans <- data_trans * decay_univ
  
  transformed_data <- tryCatch({
    switch(transformation_univ,
           "Linear" = data_trans,
           "S Origin" = s_curve_transform(data_trans, "s-origin", alpha_univ, beta_univ, maxval_univ),
           "S Shaped" = s_curve_transform(data_trans, "s-shaped", alpha_univ, beta_univ, maxval_univ),
           "Index Exp" = 1 - exp(- (alpha_univ/10) * data_trans),
           "Log" = log1p(data_trans),
           "Exp" = exp(data_trans),
           "Power" = data_trans^alpha_univ,
           "Moving Avg" = {
             if (length(data_trans) < 3) rep(NA, length(data_trans)) 
             else zoo::rollmean(data_trans, k = 3, fill = NA, align = "right")
           },
           data_trans
    )
  }, error = function(e) {
    notifyUser(paste("Error en transformación:", e$message), "error")
    data_trans
  })
  
  df_trans <- df
  validate(
    need(nrow(df_trans) > 0, "No hay datos disponibles para transformar.")
  )
  
  if(length(transformed_data) != nrow(df_trans)){
    notifyUser("Error: la longitud de la transformación no coincide con el número de registros.", "error")
    return(NULL)
  }
  
  df_trans <- df_trans %>% mutate(Transformed = transformed_data)
  date_col <- if ("Period" %in% names(df_trans)) "Period" else "periodo"
  req(date_col)
  
  p <- ggplot(df_trans, aes_string(x = date_col, y = "Transformed")) +
    geom_line(color = "red") +
    theme_minimal() +
    labs(
      title = paste("Variable Transformada (Geography:", geography_univ, ")"),
      x = "Tiempo",
      y = "Valor Transformado"
    )
  
  tryCatch({
    ggplotly(p)
  }, error = function(e) {
    notifyUser(paste("Error al renderizar gráfico transformado:", e$message), "error")
    return(NULL)
  })
}