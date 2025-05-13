# =============================================================================
# Boxplot for Univariate tab.
# =============================================================================

library(ggplot2)
library(scales)

render_boxplot_univariate <- function(df, variable_univ, geography_univ) {
  req(df, variable_univ)
  
  validate(
    need(variable_univ != "N/A", "Please select a valid variable for the boxplot.")
  )
  
  # Asegurarse de que la variable existe en el dataframe
  if (!variable_univ %in% names(df)) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = paste("Variable not found:", variable_univ)) +
             theme_void())
  }
  
  # Extraer datos y filtrar NAs y ceros
  box_data <- df[[variable_univ]]
  
  # Comprobar si hay algún dato válido
  if (length(box_data) == 0 || all(is.na(box_data)) || all(box_data == 0, na.rm = TRUE)) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No valid data available for boxplot") +
             theme_void())
  }
  
  # Filtrar NAs y ceros explícitamente
  valid_data <- box_data[!is.na(box_data) & box_data != 0]
  
  # Comprobar de nuevo si quedaron datos válidos después del filtrado
  if (length(valid_data) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, label = "No non-zero data available for boxplot") +
             theme_void())
  }
  
  # Crear dataframe con los datos válidos para ggplot
  plot_df <- data.frame(val = valid_data)
  
  # Crear plot con manejo seguro de escalas
  p <- ggplot(plot_df, aes(x = val)) +
    geom_boxplot(
      fill = "#4682B4",
      color = "#2F4F4F",
      alpha = 0.8,
      outlier.shape = 21,
      outlier.fill = "white",
      outlier.color = "#2F4F4F"
    ) +
    coord_flip() +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0, margin = margin(b = 10)),
      axis.title = element_text(size = 11, face = "bold", color = "#303030"),
      axis.text = element_text(size = 10, color = "#505050"),
      panel.grid.major = element_line(color = "#ECECEC"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 10, r = 20, b = 10, l = 40)
    ) +
    # Usar scale_x_continuous de forma segura
    scale_x_continuous(labels = function(x) {
      tryCatch({
        scales::label_number(scale_cut = scales::cut_short_scale())(x)
      }, error = function(e) {
        # En caso de error, volver a una función de etiquetado sencilla
        format(x, scientific = FALSE, big.mark = ",")
      })
    }) +
    labs(
      title = paste("Boxplot (Geography:", geography_univ, ")"),
      x = variable_univ,
      y = NULL
    )
  
  return(p)
}