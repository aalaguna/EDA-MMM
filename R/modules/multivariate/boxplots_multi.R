# R/modules/multivariate/boxplots_multi.R
# =============================================================================
# Módulo: boxplots_multi
# Renderiza los boxplots para variables individuales y sumadas
# =============================================================================

render_summed_boxplot <- function(df) {
  req(df)
  
  plot_ly(y = ~df$sum_vars, type = "box",
          fillcolor = 'orange',
          line = list(color = 'darkred')) %>%
    layout(title = "Boxplot - Variable Sumada",
           yaxis = list(title = "Valor Sumado"),
           showlegend = FALSE)
}

render_individual_boxplots <- function(df, input) {
  req(df)
  
  vars <- c(input$var1_multi, input$var2_multi, input$var3_multi)
  if (!is.null(input$var4_multi) && input$var4_multi != "None") {
    vars <- c(vars, input$var4_multi)
  }
  vars <- intersect(vars, names(df))
  
  df_long <- df %>% 
    select(all_of(vars)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
  
  plot_ly(df_long, x = ~variable, y = ~value, type = "box",
          fillcolor = 'purple',
          line = list(color = 'darkpurple')) %>%
    layout(title = "Boxplot de Variables Seleccionadas",
           xaxis = list(title = "Variable"),
           yaxis = list(title = "Valor"),
           showlegend = FALSE)
}