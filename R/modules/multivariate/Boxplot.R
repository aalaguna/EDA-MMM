# =============================================================================
# Boxplot for Multivariate tab.
# =============================================================================

# Function that generates a boxplot for 'sum_vars' ignoring NA and zeros,
# in horizontal orientation.
render_summed_boxplot <- function(df) {
  # Verify that the dataframe is not NULL
  req(df)
  
  # Check that the 'sum_vars' column exists
  if (!"sum_vars" %in% names(df)) {
    return(plot_ly() %>% layout(title = "Column 'sum_vars' does not exist."))
  }
  
  # Filter NA values and zeros
  non_zero_sum_vars <- df$sum_vars
  non_zero_sum_vars <- non_zero_sum_vars[!is.na(non_zero_sum_vars) & non_zero_sum_vars != 0]
  
  # Validate that valid data exists
  if (length(non_zero_sum_vars) == 0) {
    return(plot_ly() %>% layout(title = "No non-zero data in 'sum_vars'."))
  }
  
  # Create horizontal boxplot with Plotly (clean y-axis)
  plot_ly(
    y = "",  # Empty category to suppress Y-axis label like "orange"
    x = ~non_zero_sum_vars,
    type = "box",
    orientation = "h",
    fillcolor = "orange",
    line = list(color = "darkred")
  ) %>%
    layout(
      title = "Boxplot - Summed Variable",
      xaxis = list(
        title = list(
          text = "sum_vars",
          standoff = 10
        ),
        automargin = TRUE
      ),
      yaxis = list(
        title = NULL,
        showticklabels = FALSE  # Hide tick labels on Y axis
      ),
      margin = list(l = 50, r = 30, t = 60, b = 40),
      showlegend = FALSE
    )
}

# render_summed_boxplot <- function(df) {
#   req(df)
#   
#   if (!"sum_vars" %in% names(df)) {
#     return(NULL)
#   }
#   
#   # Filtrar valores no nulos ni cero
#   non_zero_sum_vars <- df$sum_vars
#   non_zero_sum_vars <- non_zero_sum_vars[!is.na(non_zero_sum_vars) & non_zero_sum_vars != 0]
#   
#   if (length(non_zero_sum_vars) == 0) {
#     return(NULL)
#   }
#   
#   # Crear dataframe para graficar
#   df_plot <- data.frame(sum_vars = non_zero_sum_vars)
#   
#   # Crear gráfico ggplot
#   p <- ggplot(df_plot, aes(x = "", y = sum_vars)) +
#     geom_boxplot(fill = "orange", color = "darkred", width = 0.3) +
#     coord_flip() +
#     scale_y_continuous(
#       labels = scales::label_number(scale_cut = scales::cut_si("B", "M", "K", ""), accuracy = 0.1)
#     ) +
#     labs(
#       title = "Boxplot - Summed Variable",
#       x = NULL,
#       y = "sum_vars"
#     ) +
#     theme_minimal() +
#     theme(
#       axis.text.x = element_text(size = 10),
#       axis.text.y = element_blank(),
#       axis.ticks.y = element_blank(),
#       plot.title = element_text(hjust = 0.5)
#     )
#   
#   ggplotly(p)
# }
