# # Transformed_Summed_Variables.R
# 
# 
# # Function that renders a chart for transformed summed variables
# render_transformed_variables_chart <- function(df) {
#   req(df)
#   if (!"trans_sum_vars" %in% names(df)) {
#     return(NULL)
#   }
#   
#   date_col <- if ("Period" %in% names(df)) "Period" else "periodo"
#   req(date_col)
# 
#   df2 <- df %>%
#     mutate(
#       date = as.Date(.data[[date_col]])
#     ) %>%
#     arrange(date)
#   
#   # Crear gráfico con fechas reales y eje limpio
#   plot_ly(
#     df2, 
#     x = ~date, 
#     y = ~trans_sum_vars,
#     type = 'scatter', 
#     mode = 'lines+markers',
#     line = list(color = 'red'),
#     marker = list(color = 'red'),
#     hovertemplate = paste(
#       "Date: %{x|%Y-%m-%d}<br>",
#       "Transformed Sum: %{y:,.2f}<extra></extra>"
#     )
#   ) %>%
#     layout(
#       title = "Transformed Summed Variables",
#       xaxis = list(
#         title = list(text = "Time", standoff = 35),
#         type = "date",
#         tickformat = "%Y-%m",
#         dtick = "M3",
#         tickangle = -70,
#         tickfont = list(size = 10),
#         automargin = TRUE
#       ),
#       yaxis = list(
#         title = list(text = "Transformed Sum", standoff = 20),
#         automargin = TRUE
#       ),
#       margin = list(l = 70, r = 40, t = 60, b = 120),
#       showlegend = FALSE
#     )
#   
# }
render_transformed_sum_chart <- function(df, kpi_multi, transformation_multi,
                                         lag_multi, decay_multi,
                                         alpha_multi, beta_multi, maxval_multi,
                                         geography_multi) {
  
  req(df, kpi_multi, transformation_multi)
  
  validate(
    need("trans_sum_vars" %in% names(df), "Missing transformed summed variable."),
    need(kpi_multi %in% names(df), "Selected KPI not found in data.")
  )
  
  date_col <- if ("Period" %in% names(df)) "Period" else "periodo"
  req(date_col)
  
  # --- KPI Vector ---
  kpi_vec <- as.numeric(df[[kpi_multi]])
  
  # --- Variable to transform ---
  data_vec <- as.numeric(df$trans_sum_vars)
  data_trans <- data_vec
  
  # --- Apply Lag ---
  if (lag_multi > 0) {
    if (lag_multi >= length(data_trans)) {
      data_trans <- rep(NA, length(data_trans))
    } else {
      data_trans <- c(rep(NA, lag_multi), head(data_trans, -lag_multi))
    }
  }
  
  # --- Apply Decay (Carry-over effect) ---
  if (!is.na(decay_multi) && decay_multi < 1 && decay_multi > 0) {
    for (i in 2:length(data_trans)) {
      if (!is.na(data_trans[i - 1])) {
        data_trans[i] <- data_trans[i] + decay_multi * data_trans[i - 1]
      }
    }
  }
  
  # --- Apply Transformation ---
  transformed_data <- tryCatch({
    switch(transformation_multi,
           "Linear" = data_trans,
           "S Origin" = {
             if (max(data_trans, na.rm = TRUE) != 0) {
               beta_scaled <- beta_multi / (10^9)
               scaled_x <- 100 * data_trans / (max(data_trans, na.rm = TRUE) * maxval_multi)
               ((beta_scaled)^((alpha_multi^scaled_x))) - beta_scaled
             } else data_trans
           },
           "S Shaped" = {
             if (max(data_trans, na.rm = TRUE) != 0) {
               beta_scaled <- beta_multi / (10^10)
               scaled_x <- 100 * data_trans / (max(data_trans, na.rm = TRUE) * maxval_multi)
               (beta_scaled)^(alpha_multi^scaled_x)
             } else data_trans
           },
           "Index Exp" = {
             if (max(data_trans, na.rm = TRUE) != 0) {
               1 - exp((-alpha_multi * (100 * data_trans / (max(data_trans, na.rm = TRUE) * maxval_multi))) / 10)
             } else data_trans
           },
           "Log" = log1p(data_trans),
           "Exp" = exp(data_trans),
           "Power" = data_trans^alpha_multi,
           "Moving Avg" = if (length(data_trans) < 3) rep(NA, length(data_trans)) else zoo::rollmean(data_trans, k = 3, fill = NA, align = "right"),
           data_trans
    )
  }, error = function(e) {
    notifyUser(paste("Transformation error:", e$message), "error")
    data_trans
  })
  
  # --- Prepare final data for plotting ---
  df_plot <- df %>%
    mutate(
      date = as.Date(.data[[date_col]]),
      KPI = kpi_vec,
      Variable_Transformed = transformed_data
    ) %>%
    arrange(date)
  
  # --- Plot ---
  plot_ly(df_plot, x = ~date) %>%
    add_trace(
      y = ~Variable_Transformed,
      name = "Transformed Variable",
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "red", width = 2),
      marker = list(color = "red", size = 6),
      yaxis = "y",
      hovertemplate = paste(
        "Date: %{x|%Y-%m-%d}<br>",
        "Transformed: %{y:.2f}<extra></extra>"
      )
    ) %>%
    add_trace(
      y = ~KPI,
      name = "KPI",
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "blue", width = 2),
      marker = list(color = "blue", size = 6),
      yaxis = "y2",
      hovertemplate = paste(
        "Date: %{x|%Y-%m-%d}<br>",
        "KPI: %{y:.2f}<extra></extra>"
      )
    ) %>%
    layout(
      title = list(text = paste("Transformed Sum vs KPI (Geography:", geography_multi, ")")),
      xaxis = list(
        title = list(text = "Time", standoff = 25),
        type = "date",
        tickformat = "%Y-%m",
        dtick = "M3",
        tickangle = -45,
        automargin = TRUE
      ),
      yaxis = list(
        title = list(text = "Transformed Variable", standoff = 20),
        side = "left",
        automargin = TRUE
      ),
      yaxis2 = list(
        title = list(text = "KPI", standoff = 20),
        overlaying = "y",
        side = "right",
        automargin = TRUE
      ),
      legend = list(
        orientation = "v",
        xanchor = "left",
        x = 1.02,
        y = 1
      ),
      margin = list(l = 70, r = 120, t = 60, b = 80),
      hovermode = "x unified"
    )
}
