# Line_Selected_Variables.R

# Function that generates a multi-variable line chart for 4 selected variables,
# synchronizing the time axis according to the period defined in the dataframe.
render_individual_boxplots <- function(df, input) {
  # Verify that the dataframe is not NULL
  req(df)
  
  # Determine the date column: "Period" or "periodo"
  date_col <- if ("Period" %in% names(df)) "Period" else "periodo"
  req(date_col)
  
  # Select variables using inputs
  var_inputs <- c(
    if (!is.null(input$var1_multi)) input$var1_multi else "None",
    if (!is.null(input$var2_multi)) input$var2_multi else "None",
    if (!is.null(input$var3_multi)) input$var3_multi else "None",
    if (!is.null(input$var4_multi)) input$var4_multi else "None"
  )
  
  # Filter out "None" values
  var_inputs <- var_inputs[var_inputs != "None"]
  
  # Intersect with available columns
  vars <- intersect(var_inputs, names(df))
  
  if (length(vars) < 1) {
    return(plot_ly() %>% layout(title = "No variables to plot."))
  }
  
  # Create variable to "Variable X" mapping
  var_mapping <- list()
  
  if (!is.null(input$var1_multi) && input$var1_multi != "None" && input$var1_multi %in% names(df)) {
    var_mapping[[input$var1_multi]] <- "Variable 1"
  }
  
  if (!is.null(input$var2_multi) && input$var2_multi != "None" && input$var2_multi %in% names(df)) {
    var_mapping[[input$var2_multi]] <- "Variable 2"
  }
  
  if (!is.null(input$var3_multi) && input$var3_multi != "None" && input$var3_multi %in% names(df)) {
    var_mapping[[input$var3_multi]] <- "Variable 3"
  }
  
  if (!is.null(input$var4_multi) && input$var4_multi != "None" && input$var4_multi %in% names(df)) {
    var_mapping[[input$var4_multi]] <- "Variable 4"
  }
  
  # Transform dataframe to long format for easier visualization
  df_long <- df %>%
    select(all_of(vars), !!sym(date_col)) %>%
    pivot_longer(
      cols = all_of(vars),
      names_to = "variable",
      values_to = "value"
    )
  
  # Filter NA values and zeros
  df_long <- df_long %>% filter(!is.na(value) & value != 0)
  
  if (nrow(df_long) == 0) {
    return(plot_ly() %>% layout(title = "No non-zero values to plot."))
  }
  
  # Create display_name column with "Variable X" label
  df_long <- df_long %>%
    mutate(display_name = sapply(variable, function(v) {
      if (v %in% names(var_mapping)) {
        return(var_mapping[[v]])
      } else {
        return(v)  # Fallback to original name
      }
    }))
  
  # Format dates
  df_long <- df_long %>%
    mutate(
      year = format(as.Date(get(date_col)), "%Y"),
      quarter = lubridate::quarter(as.Date(get(date_col))),
      month_label = case_when(
        quarter == 1 ~ "01",
        quarter == 2 ~ "04",
        quarter == 3 ~ "07",
        quarter == 4 ~ "10"
      ),
      label = paste0(year, "-", month_label)
    )
  
  # Group by quarterly label and variable
  df_grouped <- df_long %>%
    group_by(label, display_name) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  
  # Create synchronized line chart with quarterly time axis
  plot_ly(
    data = df_grouped,
    x = ~label,
    y = ~value,
    color = ~display_name,
    type = "scatter",
    mode = "lines+markers"
  ) %>%
    layout(
      title = "Multi-Variable Line Chart",
      xaxis = list(
        title = list(
          text = "Time",
          standoff = 35  # Greater distance between title and axis
        ), 
        type = "category", 
        tickangle = -70,  # Steeper angle for better readability
        tickfont = list(size = 10),  # Slightly smaller font
        automargin = TRUE,
        dtick = 2  # Show every other tick (reduce crowding)
      ),
      yaxis = list(
        title = list(
          text = "Value",
          standoff = 20  # Greater distance between title and axis
        ),
        automargin = TRUE
      ),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        y = -0.4,  # Position the legend further below
        xanchor = "center",
        x = 0.5
      ),
      margin = list(l = 60, r = 40, t = 50, b = 150),  # Increased bottom margin for date labels
      autosize = TRUE
    ) %>%
    config(responsive = TRUE)
}