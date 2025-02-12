# R/modules/multivariate/data_processing.R
# =============================================================================
# Módulo: data_processing
# Maneja el procesamiento de datos multivariados
# =============================================================================

process_summed_data <- function(df, input) {
  req(df)
  
  vars_selected <- c(input$var1_multi, input$var2_multi, input$var3_multi)
  if (!is.null(input$var4_multi) && input$var4_multi != "None" && input$var4_multi != "") {
    vars_selected <- c(vars_selected, input$var4_multi)
  }
  vars_selected <- intersect(vars_selected, names(df))
  
  if (length(vars_selected) < 2) {
    return(NULL)
  }
  
  df$sum_vars <- rowSums(df[, vars_selected, drop = FALSE], na.rm = TRUE)
  df
}

process_transformed_data <- function(df, input) {
  req(df)
  
  transformed <- apply_transformation(df$sum_vars,
                                   type = input$trans_var1,
                                   alpha = input$alpha_multi,
                                   beta = input$beta_multi,
                                   maxval = input$maxval_multi,
                                   decay = input$decay_multi,
                                   lag = input$lag_multi)
  df$trans_sum_vars <- transformed
  df
}