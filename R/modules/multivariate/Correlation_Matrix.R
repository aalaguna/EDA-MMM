# Correlation_Matrix.R


# Function that renders a correlation matrix between selected variables
render_correlation_matrix <- function(df, input) {
  req(df)
  
  # First, determine which variables are selected and their indices
  var_inputs <- list(
    "1" = input$var1_multi,
    "2" = input$var2_multi,
    "3" = input$var3_multi,
    "4" = input$var4_multi
  )
  
  # Filter out non-selected variables
  selected_vars <- var_inputs[var_inputs != "None"]
  var_columns <- intersect(unlist(selected_vars), names(df))
  
  # Include the KPI if selected and valid
  kpi_var <- NULL
  if (!is.null(input$kpi_multi) && input$kpi_multi != "None" && input$kpi_multi %in% names(df)) {
    kpi_var <- input$kpi_multi
    var_columns <- c(var_columns, kpi_var)
  }
  
  if (length(var_columns) < 2) {
    return(plot_ly() %>% layout(title = "Select at least two variables."))
  }
  
  # Calculate the correlation matrix
  corr_matrix <- cor(df[var_columns], use = "complete.obs")
  
  # Create "Variable X" type labels where X is the original index (1-4)
  display_names <- c()
  for (i in 1:length(var_columns)) {
    var_name <- var_columns[i]
    
    # If it's the KPI, use "KPI"
    if (!is.null(kpi_var) && var_name == kpi_var) {
      display_names[i] <- "KPI"
    } else {
      # Find the original index in var_inputs
      for (j in 1:4) {
        if (var_inputs[[as.character(j)]] == var_name) {
          display_names[i] <- paste("Variable", j)
          break
        }
      }
    }
  }
  
  # Identify the position of the KPI in the matrix
  kpi_index <- which(var_columns == kpi_var)
  
  # Create annotations to display correlation values in each cell
  annotations <- list()
  for (i in 1:length(var_columns)) {
    for (j in 1:length(var_columns)) {
      # Text color: black for regular correlations,
      # green for significant positive correlations with the KPI,
      # red for significant negative correlations with the KPI
      text_color <- "black"
      
      # If we're in a cell involving the KPI and not on the diagonal
      if (length(kpi_index) > 0 && (i == kpi_index || j == kpi_index) && i != j) {
        corr_value <- corr_matrix[i, j]
        if (abs(corr_value) >= 0.5) {
          text_color <- ifelse(corr_value > 0, "darkgreen", "darkred")
        }
      }
      
      annotations[[length(annotations) + 1]] <- list(
        x = display_names[j],
        y = display_names[i],
        text = format(round(corr_matrix[i, j], 2), nsmall = 2),
        showarrow = FALSE,
        font = list(color = text_color, size = 12)
      )
    }
  }
  
  # Customize color scale for correlations
  color_scale <- list(
    c(0, "red"),
    c(0.5, "white"),
    c(1, "blue")
  )
  
  plot_ly(
    x = display_names,
    y = display_names,
    z = corr_matrix,
    type = "heatmap",
    colorscale = color_scale,
    zmin = -1, zmax = 1
  ) %>%
    layout(
      title = list(
        text = "Correlation Matrix",
        y = 0.97  # Slightly lower the title
      ),
      xaxis = list(
        title = "", 
        tickangle = 45, 
        tickfont = list(size = 12),
        automargin = TRUE,  # Automatic margin to avoid overlap
        tickmode = "array",
        tickvals = display_names,
        ticktext = display_names
      ),
      yaxis = list(
        title = "", 
        tickfont = list(size = 12),
        automargin = TRUE,  # Automatic margin to avoid overlap
        tickmode = "array",
        tickvals = display_names,
        ticktext = display_names
      ),
      annotations = annotations,
      margin = list(l = 80, r = 40, t = 60, b = 100, pad = 4),  # Increase margins
      autosize = TRUE
    ) %>%
    config(responsive = TRUE)
}