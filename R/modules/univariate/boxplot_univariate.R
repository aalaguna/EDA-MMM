# R/modules/univariate/boxplot_univariate.R
# =============================================================================
# Módulo: boxplot_univariate
# Renderiza el boxplot para la variable seleccionada
# =============================================================================

render_boxplot_univariate <- function(df, variable_univ, geography_univ) {
  req(df, variable_univ)
  validate(
    need(variable_univ != "N/A", "Seleccione una variable válida para el boxplot.")
  )

  box_data <- df[[variable_univ]]
  box_data <- box_data[!is.na(box_data)]
  validate(
    need(length(box_data) > 0, "No hay suficientes datos para el boxplot.")
  )

  ggplot(data.frame(val = box_data), aes(x = "", y = val)) +
    geom_boxplot(fill = "skyblue") +
    theme_minimal() +
    labs(
      title = paste("Boxplot (Geography:", geography_univ, ")"),
      x = "",
      y = variable_univ
    )
}

# render_boxplot_univariate <- function(df, variable_univ, geography_univ) {
#   req(df, variable_univ)
#   validate(
#     need(variable_univ != "N/A", "Seleccione una variable válida para el violin plot.")
#   )
# 
#   violin_data <- df[[variable_univ]]
#   violin_data <- violin_data[!is.na(violin_data)]
#   validate(
#     need(length(violin_data) > 0, "No hay suficientes datos para el violin plot.")
#   )
# 
#   ggplot(data.frame(val = violin_data), aes(x = "", y = val)) +
#     geom_violin(fill = "skyblue", alpha = 0.7) +  # Violin plot
#     geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  # Opcional: boxplot superpuesto
#     theme_minimal() +
#     labs(
#       title = paste("Violin Plot (Geography:", geography_univ, ")"),
#       x = "",
#       y = variable_univ
#     )
# }
