# # Transformed_Variable.R
# 
# # =============================================================================
# # Renders the chart of the transformed variable over time.
# # =============================================================================
# 
# render_transformation_chart <- function(df, variable_univ, transformation_univ,
#                                         lag_univ, decay_univ,
#                                         alpha_univ, beta_univ, maxval_univ,
#                                         geography_univ) {
# 
# 
#   req(df, variable_univ, transformation_univ)
#   validate(
#     need(variable_univ != "N/A", "Please select a valid variable for transformation.")
#   )
# 
#   var_name <- variable_univ
#   data_vec <- df[[var_name]]
#   validate(
#     need(length(data_vec) > 0, "No data available for transformation.")
#   )
# 
#   data_trans <- as.numeric(data_vec)
#   if (lag_univ > 0) {
#     if (lag_univ >= length(data_trans)) {
#       data_trans <- rep(NA, length(data_trans))
#     } else {
#       data_trans <- c(rep(NA, lag_univ), head(data_trans, -lag_univ))
#     }
#   }
#   data_trans <- data_trans * decay_univ
# 
#   transformed_data <- tryCatch({
#     switch(transformation_univ,
# 
#            "Linear" = data_trans,
# 
#            "S Origin" = {
#              if (max(data_trans, na.rm = TRUE) != 0) {
#                beta_scaled <- beta_univ / 1e9
#                scaled_x <- 100 * data_trans / (max(data_trans, na.rm = TRUE) * maxval_univ)
#                result <- (beta_scaled)^(alpha_univ^scaled_x) - beta_scaled
#                result
#              } else {
#                data_trans
#              }
#            },
# 
#            "S Shaped" = {
#              if (max(data_trans, na.rm = TRUE) != 0) {
#                beta_scaled <- beta_univ / 1e10
#                scaled_x <- 100 * data_trans / (max(data_trans, na.rm = TRUE) * maxval_univ)
#                result <- (beta_scaled)^(alpha_univ^scaled_x)
#                result
#              } else {
#                data_trans
#              }
#            },
# 
#            "Index Exp" = 1 - exp(-(alpha_univ / 10) * data_trans),
#            "Log"      = log1p(data_trans),
#            "Exp"      = exp(data_trans),
#            "Power"    = data_trans^alpha_univ,
# 
#            "Moving Avg" = {
#              if (length(data_trans) < 3) rep(NA, length(data_trans))
#              else zoo::rollmean(data_trans, k = 3, fill = NA, align = "right")
#            },
# 
#            data_trans
#     )
#   }, error = function(e) {
#     notifyUser(paste("Transformation error:", e$message), "error")
#     data_trans
#   })
# 
# 
#   date_col <- if ("Period" %in% names(df)) "Period" else "periodo"
#   req(date_col)
# 
#   df_trans <- df %>%
#     mutate(
#       date = as.Date(.data[[date_col]]),
#       Transformed = transformed_data
#     ) %>%
#     arrange(date)
# 
#   # Use plotly directly with real date and quarterly labels
#   p <- plot_ly(
#     df_trans,
#     x = ~date,
#     y = ~Transformed,
#     type = 'scatter',
#     mode = 'lines+markers',
#     line = list(color = 'red'),
#     hovertemplate = paste(
#       "Date: %{x|%Y-%m-%d}<br>",
#       "Transformed: %{y:,.2f}<extra></extra>"
#     )
#   ) %>%
#     layout(
#       title = paste("Transformed Variable (Geography:", geography_univ, ")"),
#       xaxis = list(
#         title = list(text = "Time", standoff = 25),
#         type = "date",
#         tickformat = "%Y-%m",
#         dtick = "M3",
#         tickangle = -45,
#         automargin = TRUE
#       ),
#       yaxis = list(
#         title = list(text = "Transformed Value", standoff = 20),
#         automargin = TRUE
#       ),
#       margin = list(l = 70, r = 40, t = 60, b = 80),
#       showlegend = FALSE
#     )
# 
#   return(p)
# 
# }


render_transformation_chart <- function(df, kpi_univ, variable_univ, transformation_univ,
                                        lag_univ, decay_univ,
                                        alpha_univ, beta_univ, maxval_univ,
                                        geography_univ) {
  
  req(df, kpi_univ, variable_univ, transformation_univ)
  validate(
    need(variable_univ != "N/A", "Please select a valid variable for transformation."),
    need(kpi_univ != "N/A", "Please select a valid KPI.")
  )
  
  date_col <- if ("Period" %in% names(df)) "Period" else "periodo"
  req(date_col)
  
  # --- KPI Vector ---
  kpi_vec <- as.numeric(df[[kpi_univ]])
  
  # --- Transform Variable ---
  data_vec <- as.numeric(df[[variable_univ]])
  
  data_trans <- data_vec
  
  # Apply Lag
  if (lag_univ > 0) {
    if (lag_univ >= length(data_trans)) {
      data_trans <- rep(NA, length(data_trans))
    } else {
      data_trans <- c(rep(NA, lag_univ), head(data_trans, -lag_univ))
    }
  }
  
  # Apply Decay
  data_trans <- data_trans * decay_univ
  
  # Apply Transformation
  transformed_data <- tryCatch({
    switch(transformation_univ,
           "Linear" = data_trans,
           "S Origin" = {
             if (max(data_trans, na.rm = TRUE) != 0) {
               beta_scaled <- beta_univ / 1e9
               scaled_x <- 100 * data_trans / (max(data_trans, na.rm = TRUE) * maxval_univ)
               (beta_scaled)^(alpha_univ^scaled_x) - beta_scaled
             } else data_trans
           },
           "S Shaped" = {
             if (max(data_trans, na.rm = TRUE) != 0) {
               beta_scaled <- beta_univ / 1e10
               scaled_x <- 100 * data_trans / (max(data_trans, na.rm = TRUE) * maxval_univ)
               (beta_scaled)^(alpha_univ^scaled_x)
             } else data_trans
           },
           "Index Exp" = 1 - exp(-(alpha_univ / 10) * data_trans),
           "Log" = log1p(data_trans),
           "Exp" = exp(data_trans),
           "Power" = data_trans^alpha_univ,
           "Moving Avg" = if (length(data_trans) < 3) rep(NA, length(data_trans)) else zoo::rollmean(data_trans, k = 3, fill = NA, align = "right"),
           data_trans
    )
  }, error = function(e) {
    notifyUser(paste("Transformation error:", e$message), "error")
    data_trans
  })
  
  # --- date ---

  df_plot <- df %>%
    mutate(
      date = as.Date(.data[[date_col]]),
      KPI = kpi_vec,
      Variable_Transformed = transformed_data,
      year = format(as.Date(.data[[date_col]]), "%Y"),
      month = as.integer(format(as.Date(.data[[date_col]]), "%m")),
      quarter = ceiling(month / 3),
      month_label = case_when(
        quarter == 1 ~ "01",
        quarter == 2 ~ "04",
        quarter == 3 ~ "07",
        quarter == 4 ~ "10",
        TRUE ~ "01"
      ),
      label = paste0(year, "-", month_label)
    ) %>%
    arrange(date)
  

  
  # --- Plot ---
  # plot_ly(df_plot, x = ~date) %>%
  #   add_lines(
  #     y = ~KPI,
  #     name = "KPI",
  #     line = list(color = "blue"),
  #     yaxis = "y"
  #   ) %>%
  #   add_lines(
  #     y = ~Variable_Transformed,
  #     name = "Variable (Transformed)",
  #     line = list(color = "red"),
  #     yaxis = "y2"
  #   ) %>%
  #   layout(
  #     title = paste("Transformed Variable vs KPI (Geography:", geography_univ, ")"),
  #     xaxis = list(
  #       title = list(text = "Time", standoff = 25),
  #       type = "date",
  #       tickformat = "%Y-%m",
  #       tickangle = -45
  #     ),
  #     yaxis = list(
  #       title = "KPI",
  #       side = "left"
  #     ),
  #     yaxis2 = list(
  #       title = "Transformed Variable",
  #       overlaying = "y",
  #       side = "right"
  #     ),
  #     legend = list(
  #       orientation = "v",
  #       xanchor = "left",
  #       x = 1.02,
  #       y = 1
  #     ),
  #     hovermode = "x unified"
  #   )
  # --- Plot ---
  # plot_ly(df_plot, x = ~label) %>%
  #   add_lines(
  #     y = ~KPI,
  #     name = "KPI",
  #     line = list(color = "blue"),
  #     yaxis = "y"
  #   ) %>%
  #   add_lines(
  #     y = ~Variable_Transformed,
  #     name = "Variable (Transformed)",
  #     line = list(color = "red"),
  #     yaxis = "y2"
  #   ) %>%
  #   layout(
  #     title = paste("Transformed Variable vs KPI (Geography:", geography_univ, ")"),
  #     xaxis = list(
  #       title = list(text = "Time", standoff = 25),
  #       type = "category",
  #       tickangle = -45
  #     ),
  #     yaxis = list(
  #       title = "KPI",
  #       side = "left"
  #     ),
  #     yaxis2 = list(
  #       title = "Transformed Variable",
  #       overlaying = "y",
  #       side = "right"
  #     ),
  #     legend = list(
  #       orientation = "v",
  #       xanchor = "left",
  #       x = 1.02,
  #       y = 1
  #     ),
  #     hovermode = "x unified"
  #   )
  # --- Plot ---
  plot_ly(df_plot, x = ~date) %>%
    add_trace(
      y = ~KPI,
      name = "KPI",
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "blue", width = 2),
      marker = list(color = "blue", size = 6),
      yaxis = "y",
      hovertemplate = paste(
        "Date: %{x|%Y-%m-%d}<br>",
        "KPI: %{y:.2f}<extra></extra>"
      )
    ) %>%
    add_trace(
      y = ~Variable_Transformed,
      name = "Variable (Transformed)",
      type = "scatter",
      mode = "lines+markers",
      line = list(color = "red", width = 2),
      marker = list(color = "red", size = 6),
      yaxis = "y2",
      hovertemplate = paste(
        "Date: %{x|%Y-%m-%d}<br>",
        "Variable: %{y:.2f}<extra></extra>"
      )
    ) %>%
    layout(
      title = list(text = paste("Transformed Variable vs KPI (Geography:", geography_univ, ")")),
      xaxis = list(
        title = list(text = "Time", standoff = 25),
        type = "date",
        tickformat = "%Y-%m",
        dtick = "M3",
        tickangle = -45,
        automargin = TRUE
      ),
      yaxis = list(
        title = list(text = "KPI", standoff = 20),
        side = "left",
        automargin = TRUE
      ),
      yaxis2 = list(
        title = list(text = "Transformed Variable", standoff = 20),
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


# GGPLOT ------------------------------------------------------------------


# render_transformation_chart <- function(df, variable_univ, transformation_univ,
#                                         lag_univ, decay_univ,
#                                         alpha_univ, beta_univ, maxval_univ,
#                                         geography_univ) {
#   library(ggplot2)
#   library(plotly)
#   
#   req(df, variable_univ, transformation_univ)
#   validate(
#     need(variable_univ != "N/A", "Please select a valid variable for transformation.")
#   )
#   
#   var_name <- variable_univ
#   data_vec <- as.numeric(df[[var_name]])
#   validate(
#     need(length(data_vec) > 0, "No data available for transformation.")
#   )
#   
#   # 1. Transform
#   transformed_data <- tryCatch({
#     switch(transformation_univ,
#            "Linear" = data_vec,
#            "S Origin" = {
#              if (max(data_vec, na.rm = TRUE) != 0) {
#                beta_scaled <- beta_univ / 1e9
#                scaled_x <- 100 * data_vec / (max(data_vec, na.rm = TRUE) * (maxval_univ / 100))
#                (beta_scaled)^(alpha_univ^scaled_x) - beta_scaled
#              } else data_vec
#            },
#            "S Shaped" = {
#              if (max(data_vec, na.rm = TRUE) != 0) {
#                beta_scaled <- beta_univ / 1e10
#                scaled_x <- 100 * data_vec / (max(data_vec, na.rm = TRUE) * (maxval_univ / 100))
#                (beta_scaled)^(alpha_univ^scaled_x)
#              } else data_vec
#            },
#            "Index Exp" = 1 - exp(-(alpha_univ / 10) * data_vec),
#            "Log" = log1p(data_vec),
#            "Exp" = exp(data_vec),
#            "Power" = data_vec^alpha_univ,
#            "Moving Avg" = {
#              if (length(data_vec) < 3) rep(NA, length(data_vec))
#              else zoo::rollmean(data_vec, k = 3, fill = NA, align = "right")
#            },
#            data_vec
#     )
#   }, error = function(e) {
#     notifyUser(paste("Transformation error:", e$message), "error")
#     data_vec
#   })
#   
#   # 2. Lag
#   if (lag_univ > 0) {
#     n <- length(transformed_data)
#     if (lag_univ >= n) {
#       transformed_data <- rep(NA, n)
#     } else {
#       transformed_data <- c(rep(NA, lag_univ), head(transformed_data, n - lag_univ))
#     }
#   }
#   
#   # 3. Decay
#   decay_value <- 1 - decay_univ
#   for (i in 2:length(transformed_data)) {
#     transformed_data[i] <- transformed_data[i - 1] + (transformed_data[i] * decay_value)
#   }
#   
#   # Preparar dataframe para graficar
#   date_col <- if ("Period" %in% names(df)) "Period" else "periodo"
#   df_plot <- df %>%
#     mutate(
#       date = as.Date(.data[[date_col]]),
#       Transformed = transformed_data
#     ) %>%
#     arrange(date)
#   
#   # ggplot 
#   g <- ggplot(df_plot, aes(x = date, y = Transformed)) +
#     geom_line(color = "red") +
#     geom_point(color = "red", size = 0.5) +
#     labs(
#       title = paste("Transformed Variable (Geography:", geography_univ, ")"),
#       x = "Date",
#       y = "Transformed Value"
#     ) +
#     theme_minimal()
#   
#   # ggplotly
#   ggplotly(g)
# }
