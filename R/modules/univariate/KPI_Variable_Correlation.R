# =============================================================================
# Renders the correlation chart between KPI and (optionally) transformed variable
# =============================================================================

render_correlation_plot <- function(df, kpi_univ, variable_univ,
                                    transformation_univ, lag_univ, decay_univ,
                                    alpha_univ, beta_univ, maxval_univ,
                                    normalization_type = "None") {
  # Generates a correlation chart between KPI and (transformed) variable
  #
  # Args:
  #   df: Dataframe with data
  #   kpi_univ: Name of the KPI variable
  #   variable_univ: Name of the variable to transform
  #   transformation_univ, lag_univ, decay_univ, alpha_univ, beta_univ, maxval_univ:
  #     Transformation parameters
  #   normalization_type: Type of normalization to apply to KPI ("None", "Division", "Subtraction")
  #
  # Returns:
  #   plotly chart with correlation

  req(df, kpi_univ, variable_univ, transformation_univ)
  validate(
    need(variable_univ != "N/A", "Please select a valid variable for correlation.")
  )

  # Verificar que las columnas existen
  if (!kpi_univ %in% names(df) || !variable_univ %in% names(df)) {
    return(plotly::plot_ly() %>% 
           plotly::layout(title = "KPI or Variable column not found in data",
                         xaxis = list(showticklabels = FALSE),
                         yaxis = list(showticklabels = FALSE)))
  }

  kpi_data <- df[[kpi_univ]]
  var_data <- df[[variable_univ]]
  
  # Verificación de datos suficientes
  if (sum(!is.na(var_data)) < 5) {
    return(plotly::plot_ly() %>% 
           plotly::layout(title = "Insufficient data for correlation (less than 5 values).",
                         xaxis = list(showticklabels = FALSE),
                         yaxis = list(showticklabels = FALSE)))
  }

  # Apply normalization to KPI if requested
  if (!is.null(normalization_type) && normalization_type != "None") {
    kpi_norm_col <- NULL
    
    if (normalization_type == "Division") {
      kpi_norm_col <- paste0(kpi_univ, "_norm_div")
      # Check if the normalized column exists
      if (kpi_norm_col %in% names(df)) {
        kpi_data <- df[[kpi_norm_col]]
      } else {
        # Apply normalization manually if column doesn't exist
        kpi_mean <- mean(kpi_data, na.rm = TRUE)
        if (!is.na(kpi_mean) && kpi_mean != 0) {
          kpi_data <- kpi_data / kpi_mean
        }
      }
    } else if (normalization_type == "Subtraction") {
      kpi_norm_col <- paste0(kpi_univ, "_norm_sub")
      # Check if the normalized column exists
      if (kpi_norm_col %in% names(df)) {
        kpi_data <- df[[kpi_norm_col]]
      } else {
        # Apply normalization manually if column doesn't exist
        kpi_mean <- mean(kpi_data, na.rm = TRUE)
        if (!is.na(kpi_mean)) {
          kpi_data <- kpi_data - kpi_mean
        }
      }
    }
  }

  # Apply transformation to variable data - with error handling
  data_trans <- tryCatch({
    apply_transformation(var_data,
                       type   = transformation_univ,
                       alpha  = alpha_univ,
                       beta   = beta_univ,
                       maxval = maxval_univ,
                       decay  = decay_univ,
                       lag    = lag_univ)
  }, error = function(e) {
    warning("Error applying transformation: ", e$message)
    return(var_data)
  })

  valid_idx <- complete.cases(kpi_data, data_trans)
  if (sum(valid_idx) < 5) {
    return(plotly::plot_ly() %>% 
           plotly::layout(title = "Not enough complete data points for correlation.",
                         xaxis = list(showticklabels = FALSE),
                         yaxis = list(showticklabels = FALSE)))
  }

  corr_val <- tryCatch({
    if (sd(kpi_data[valid_idx], na.rm = TRUE) == 0 ||
        sd(data_trans[valid_idx], na.rm = TRUE) == 0) {
      NA
    } else {
      cor(kpi_data[valid_idx], data_trans[valid_idx], use = "complete.obs")
    }
  }, error = function(e) {
    warning("Error calculating correlation: ", e$message)
    return(NA)
  })

  # Setup KPI title based on normalization
  kpi_title <- kpi_univ
  if (normalization_type == "Division") {
    kpi_title <- paste0(kpi_univ, " (Normalized by Division)")
  } else if (normalization_type == "Subtraction") {
    kpi_title <- paste0(kpi_univ, " (Normalized by Subtraction)")
  }
  
  # Create a data frame for plotting
  plot_data <- data.frame(
    KPI = kpi_data[valid_idx],
    Variable = data_trans[valid_idx]
  )
  
  # Create the plot using plotly directly with explicit type/mode
  p <- plotly::plot_ly(
    data = plot_data,
    x = ~KPI,
    y = ~Variable,
    type = 'scatter',
    mode = 'markers',
    marker = list(
      color = 'darkblue', 
      size = 10,
      opacity = 0.7,
      line = list(color = 'darkblue', width = 1)
    ),
    hovertemplate = paste(
      "KPI: %{x:.2f}<br>",
      "Variable: %{y:.2f}<extra></extra>"
    )
  ) %>%
    plotly::layout(
      title = list(
        text = paste("Correlation:", ifelse(is.na(corr_val), "NA", round(corr_val, 2))),
        font = list(size = 16)
      ),
      xaxis = list(
        title = list(
          text = kpi_title,
          standoff = 25
        ),
        zeroline = TRUE,
        zerolinecolor = '#969696',
        zerolinewidth = 1,
        gridcolor = '#ddd',
        automargin = TRUE
      ),
      yaxis = list(
        title = list(
          text = paste(variable_univ, "(Transformed)"),
          standoff = 25
        ),
        zeroline = TRUE,
        zerolinecolor = '#969696',
        zerolinewidth = 1,
        gridcolor = '#ddd',
        automargin = TRUE
      ),
      margin = list(l = 80, r = 40, t = 60, b = 60),
      plot_bgcolor = "#f8f9fa",
      paper_bgcolor = "#ffffff",
      showlegend = FALSE
    )
  
  # Add regression line if we have enough points and valid correlation
  if (sum(valid_idx) >= 5 && !is.na(corr_val)) {
    # Try to fit a linear model with error handling
    fit <- tryCatch({
      lm(Variable ~ KPI, data = plot_data)
    }, error = function(e) {
      warning("Error fitting regression line: ", e$message)
      return(NULL)
    })
    
    # Only add regression line if fit was successful
    if (!is.null(fit)) {
      # Generate predicted values
      x_range <- seq(min(plot_data$KPI), max(plot_data$KPI), length.out = 100)
      predicted <- tryCatch({
        predict(fit, newdata = data.frame(KPI = x_range))
      }, error = function(e) {
        warning("Error predicting values: ", e$message)
        return(NULL)
      })
      
      # Add regression line if prediction was successful
      if (!is.null(predicted)) {
        p <- p %>% plotly::add_trace(
          x = x_range,
          y = predicted,
          type = 'scatter',
          mode = 'lines',
          line = list(
            color = 'red',
            width = 2,
            dash = 'solid'
          ),
          hoverinfo = 'none',
          showlegend = FALSE
        )
      }
    }
  }
  
  return(p)
}