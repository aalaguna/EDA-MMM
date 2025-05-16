# =============================================================================
# Renders the correlation chart between KPI and (optionally) transformed variable
# =============================================================================

render_correlation_plot <- function(df, kpi_univ, variable_univ,
                                    transformation_univ, lag_univ, decay_univ,
                                    alpha_univ, beta_univ, maxval_univ,
                                    normalization_type = "None") {
 
  # Basic input validation
  req(df, kpi_univ, variable_univ, transformation_univ)
  validate(
    need(variable_univ != "N/A", "Please select a valid variable for correlation.")
  )

  # Verify columns exist
  if (!kpi_univ %in% names(df)) {
    return(plotly::plot_ly() %>% 
           plotly::layout(title = "KPI column not found in data",
                         xaxis = list(showticklabels = FALSE),
                         yaxis = list(showticklabels = FALSE)))
  }
  
  if (!variable_univ %in% names(df)) {
    return(plotly::plot_ly() %>% 
           plotly::layout(title = "Variable column not found in data",
                         xaxis = list(showticklabels = FALSE),
                         yaxis = list(showticklabels = FALSE)))
  }

  # Extract KPI data with validation
  kpi_data <- tryCatch({
    as.numeric(df[[kpi_univ]])
  }, error = function(e) {
    warning(paste("Error converting KPI to numeric:", e$message))
    return(rep(NA_real_, nrow(df)))
  })
  
  # Extract variable data with validation
  var_data <- tryCatch({
    as.numeric(df[[variable_univ]])
  }, error = function(e) {
    warning(paste("Error converting variable to numeric:", e$message))
    return(rep(NA_real_, nrow(df)))
  })
  
  # Verify sufficient data
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
        kpi_data <- as.numeric(df[[kpi_norm_col]])
      } else {
        # Apply normalization manually if column doesn't exist
        kpi_valid <- kpi_data[is.finite(kpi_data) & !is.na(kpi_data)]
        if (length(kpi_valid) > 0) {
          kpi_mean <- mean(kpi_valid, na.rm = TRUE)
          if (!is.na(kpi_mean) && kpi_mean != 0) {
            kpi_data <- kpi_data / kpi_mean
          } else {
            warning("Cannot normalize KPI by division: Mean is zero or NA.")
          }
        } else {
          warning("Cannot normalize KPI by division: No valid data.")
        }
      }
    } else if (normalization_type == "Subtraction") {
      kpi_norm_col <- paste0(kpi_univ, "_norm_sub")
      # Check if the normalized column exists
      if (kpi_norm_col %in% names(df)) {
        kpi_data <- as.numeric(df[[kpi_norm_col]])
      } else {
        # Apply normalization manually if column doesn't exist
        kpi_valid <- kpi_data[is.finite(kpi_data) & !is.na(kpi_data)]
        if (length(kpi_valid) > 0) {
          kpi_mean <- mean(kpi_valid, na.rm = TRUE)
          if (!is.na(kpi_mean)) {
            kpi_data <- kpi_data - kpi_mean
          } else {
            warning("Cannot normalize KPI by subtraction: Mean is NA.")
          }
        } else {
          warning("Cannot normalize KPI by subtraction: No valid data.")
        }
      }
    }
  }

  # Validate and clean transformation parameters
  lag_value <- as.integer(lag_univ)
  if (is.na(lag_value) || lag_value < 0) {
    warning("Invalid lag value. Using 0 instead.")
    lag_value <- 0
  }
  
  decay_value <- as.numeric(decay_univ)
  if (is.na(decay_value) || decay_value <= 0) {
    warning("Invalid decay value. Using 1 instead.")
    decay_value <- 1
  }
  
  alpha_value <- as.numeric(alpha_univ)
  if (is.na(alpha_value) || alpha_value <= 0) {
    warning("Invalid alpha value. Using 1 instead.")
    alpha_value <- 1
  }
  
  beta_value <- as.numeric(beta_univ)
  if (is.na(beta_value) || beta_value <= 0) {
    warning("Invalid beta value. Using 1 instead.")
    beta_value <- 1
  }
  
  maxval_value <- as.numeric(maxval_univ)
  if (is.na(maxval_value) || maxval_value <= 0) {
    warning("Invalid maxval value. Using 100 instead.")
    maxval_value <- 100
  }

  # Apply transformation to variable data using the helper function
  # Assume this function is defined in Transformed_Variable.R
  if (exists("apply_transformation")) {
    data_trans <- tryCatch({
      apply_transformation(var_data,
                         type   = transformation_univ,
                         alpha  = alpha_value,
                         beta   = beta_value,
                         maxval = maxval_value,
                         decay  = decay_value,
                         lag    = lag_value)
    }, error = function(e) {
      warning("Error applying transformation: ", e$message)
      return(var_data)
    })
  } else {
    # If helper function not available, implement basic transformation logic here
    data_trans <- tryCatch({
      # Apply lag
      if (lag_value > 0) {
        if (lag_value >= length(var_data)) {
          var_data <- rep(NA, length(var_data))
        } else {
          var_data <- c(rep(NA, lag_value), head(var_data, -lag_value))
        }
      }
      
      # Apply decay
      var_data <- var_data * decay_value
      
      # Apply transformation based on type
      switch(transformation_univ,
            "Linear" = var_data,
            "S Origin" = {
              # Get max value safely
              max_value <- max(var_data, na.rm = TRUE)
              if (!is.finite(max_value) || max_value == 0) {
                return(rep(NA_real_, length(var_data)))
              }
              
              # Scale x values between 0-100 as percentage of max
              scaled_x <- var_data / (max_value * maxval_value / 100)
              
              # Initialize result vector
              result <- rep(NA_real_, length(var_data))
              
              # Apply transformation only to valid values
              valid_idx <- which(is.finite(scaled_x))
              if (length(valid_idx) > 0) {
                result[valid_idx] <- (beta_value)^(alpha_value^scaled_x[valid_idx]) - beta_value
              }
              result
            },
            "S Shaped" = {
              # Get max value safely
              max_value <- max(var_data, na.rm = TRUE)
              if (!is.finite(max_value) || max_value == 0) {
                return(rep(NA_real_, length(var_data)))
              }
              
              # Scale x values between 0-100 as percentage of max
              scaled_x <- var_data / (max_value * maxval_value / 100)
              
              # Initialize result vector
              result <- rep(NA_real_, length(var_data))
              
              # Apply transformation only to valid values
              valid_idx <- which(is.finite(scaled_x))
              if (length(valid_idx) > 0) {
                result[valid_idx] <- (beta_value)^(alpha_value^scaled_x[valid_idx])
              }
              result
            },
            "Index Exp" = {
              # Prevent overflow by limiting values
              safe_data <- pmin(var_data, 100)  # Cap at reasonable maximum
              1 - exp(-(alpha_value / 10) * safe_data)
            },
            "Log" = {
              # Use log1p to handle zeros, convert negatives to 0
              log1p(pmax(var_data, 0))
            },
            "Exp" = {
              # Prevent overflow by limiting values
              safe_data <- pmin(var_data, 20)  # Cap at reasonable maximum
              exp(safe_data)
            },
            "Power" = {
              # Handle negative values for non-integer powers
              safe_data <- var_data
              if (alpha_value %% 1 != 0) {  # Non-integer power
                safe_data[safe_data < 0] <- NA
              }
              safe_data^alpha_value
            },
            "Moving Avg" = {
              if (requireNamespace("zoo", quietly = TRUE)) {
                zoo::rollmean(var_data, k = 3, fill = NA, align = "right")
              } else {
                warning("Package 'zoo' is required for moving average calculation")
                var_data
              }
            },
            # Default: return original
            var_data
      )
    }, error = function(e) {
      warning("Error in transformation: ", e$message)
      return(var_data)
    })
  }

  # Keep only rows with valid data in both KPI and transformed variable
  valid_idx <- which(is.finite(kpi_data) & !is.na(kpi_data) & 
                    is.finite(data_trans) & !is.na(data_trans))
  
  if (length(valid_idx) < 5) {
    return(plotly::plot_ly() %>% 
           plotly::layout(title = "Not enough complete data points for correlation (after transformation).",
                         xaxis = list(showticklabels = FALSE),
                         yaxis = list(showticklabels = FALSE)))
  }

  # Calculate correlation with error handling
  corr_val <- tryCatch({
    if (stats::sd(kpi_data[valid_idx], na.rm = TRUE) == 0 ||
        stats::sd(data_trans[valid_idx], na.rm = TRUE) == 0) {
      NA
    } else {
      stats::cor(kpi_data[valid_idx], data_trans[valid_idx], use = "complete.obs")
    }
  }, error = function(e) {
    warning("Error calculating correlation: ", e$message)
    return(NA)
  })
  
  # Calculate statistical significance (p-value)
  p_value <- tryCatch({
    if (is.na(corr_val)) {
      NA
    } else {
      # Calculate p-value from correlation test
      cor_test <- stats::cor.test(kpi_data[valid_idx], data_trans[valid_idx])
      cor_test$p.value
    }
  }, error = function(e) {
    warning("Error calculating p-value: ", e$message)
    return(NA)
  })

  # Setup KPI title based on normalization
  kpi_title <- kpi_univ
  if (normalization_type == "Division") {
    kpi_title <- paste0(kpi_univ, " (Normalized by Division)")
  } else if (normalization_type == "Subtraction") {
    kpi_title <- paste0(kpi_univ, " (Normalized by Subtraction)")
  }
  
  # Create transformation details for subtitle
  transformation_details <- paste0(
    transformation_univ,
    ifelse(lag_value > 0, paste0(", Lag: ", lag_value), ""),
    ifelse(decay_value != 1, paste0(", Decay: ", decay_value), "")
  )
  
  # Create a data frame for plotting with only valid data points
  plot_data <- data.frame(
    KPI = kpi_data[valid_idx],
    Variable = data_trans[valid_idx]
  )
  
  # Add R-squared value
  r_squared <- if (!is.na(corr_val)) corr_val^2 else NA
  
  # Format significance with stars
  sig_stars <- ""
  if (!is.na(p_value)) {
    if (p_value < 0.001) sig_stars <- "***"
    else if (p_value < 0.01) sig_stars <- "**"
    else if (p_value < 0.05) sig_stars <- "*"
    else if (p_value < 0.1) sig_stars <- "."
  }
  
  # Title with correlation stats
  title_text <- if (!is.na(corr_val)) {
    paste0("Correlation: ", round(corr_val, 3), sig_stars, "  (R² = ", round(r_squared, 3), ")")
  } else {
    "Correlation: NA"
  }
  
  # Create the plot using plotly directly with explicit type/mode
  p <- plotly::plot_ly(
    data = plot_data,
    x = ~KPI,
    y = ~Variable,
    type = 'scatter',
    mode = 'markers',
    marker = list(
      color = '#1f77b4', 
      size = 10,
      opacity = 0.7,
      line = list(color = '#17a2b8', width = 1)
    ),
    hovertemplate = paste(
      "KPI: %{x:.2f}<br>",
      "Variable: %{y:.2f}<extra></extra>"
    )
  ) %>%
    plotly::layout(
      title = list(
        text = title_text,
        font = list(size = 18, family = "Arial", color = "#333")
      ),
      annotations = list(
        list(
          x = 0.5,
          y = 1.05,
          xref = "paper",
          yref = "paper",
          text = paste0("Transformation: ", transformation_details),
          showarrow = FALSE,
          font = list(size = 14, color = "#666")
        )
      ),
      xaxis = list(
        title = list(
          text = kpi_title,
          font = list(size = 14),
          standoff = 25
        ),
        zeroline = TRUE,
        zerolinecolor = '#969696',
        zerolinewidth = 1,
        gridcolor = '#e1e5ed',
        automargin = TRUE
      ),
      yaxis = list(
        title = list(
          text = paste0(variable_univ, " (Transformed)"),
          font = list(size = 14),
          standoff = 25
        ),
        zeroline = TRUE,
        zerolinecolor = '#969696',
        zerolinewidth = 1,
        gridcolor = '#e1e5ed',
        automargin = TRUE
      ),
      margin = list(l = 80, r = 40, t = 80, b = 60),
      plot_bgcolor = "#f8f9fa",
      paper_bgcolor = "#ffffff",
      showlegend = FALSE
    )
  
  # Add regression line if we have enough points and valid correlation
  if (length(valid_idx) >= 5 && !is.na(corr_val)) {
    # Try to fit a linear model with error handling
    fit <- tryCatch({
      stats::lm(Variable ~ KPI, data = plot_data)
    }, error = function(e) {
      warning("Error fitting regression line: ", e$message)
      return(NULL)
    })
    
    # Only add regression line if fit was successful
    if (!is.null(fit)) {
      # Generate predicted values
      x_range <- seq(min(plot_data$KPI), max(plot_data$KPI), length.out = 100)
      predicted <- tryCatch({
        stats::predict(fit, newdata = data.frame(KPI = x_range))
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
            color = '#d62728',
            width = 2,
            dash = 'solid'
          ),
          name = "Regression Line",
          hoverinfo = 'none'
        )
        
        # Extract coefficients for equation
        intercept <- stats::coef(fit)[1]
        slope <- stats::coef(fit)[2]
        equation <- paste0("y = ", round(slope, 3), "x + ", round(intercept, 3))
        
        # Add equation to plot
        p <- p %>% plotly::layout(
          annotations = c(
            plotly::layout(p)$annotations,
            list(
              x = 0.05,
              y = 0.95,
              xref = "paper",
              yref = "paper",
              text = equation,
              showarrow = FALSE,
              font = list(size = 12, color = "#d62728"),
              bgcolor = "rgba(255, 255, 255, 0.7)",
              bordercolor = "#d62728",
              borderwidth = 1,
              borderpad = 4
            )
          )
        )
      }
    }
  }
  
  # Add descriptive text for the correlation strength
  corr_desc <- if (!is.na(corr_val)) {
    abs_corr <- abs(corr_val)
    if (abs_corr >= 0.8) {
      "Very Strong Correlation"
    } else if (abs_corr >= 0.6) {
      "Strong Correlation"
    } else if (abs_corr >= 0.4) {
      "Moderate Correlation"
    } else if (abs_corr >= 0.2) {
      "Weak Correlation"
    } else {
      "Very Weak Correlation"
    }
  } else {
    "Unable to Calculate Correlation"
  }
  
  # Add significance explanation if p-value is available
  sig_text <- if (!is.na(p_value)) {
    if (p_value < 0.05) {
      paste0("Statistically Significant (p=", format(p_value, digits = 3, scientific = TRUE), ")")
    } else {
      paste0("Not Statistically Significant (p=", format(p_value, digits = 3, scientific = TRUE), ")")
    }
  } else {
    NULL
  }
  
  # Add annotations for correlation strength and significance
  annotations_list <- c(
    plotly::layout(p)$annotations,
    list(
      x = 0.95,
      y = 0.1,
      xref = "paper",
      yref = "paper",
      text = corr_desc,
      showarrow = FALSE,
      font = list(size = 12, color = "#333"),
      bgcolor = "rgba(255, 255, 255, 0.7)",
      bordercolor = "#333",
      borderwidth = 1,
      borderpad = 4,
      align = "center"
    )
  )
  
  # Add significance annotation if available
  if (!is.null(sig_text)) {
    annotations_list <- c(
      annotations_list,
      list(
        x = 0.95,
        y = 0.05,
        xref = "paper",
        yref = "paper",
        text = sig_text,
        showarrow = FALSE,
        font = list(size = 10, color = ifelse(p_value < 0.05, "#006400", "#8B0000")),
        bgcolor = "rgba(255, 255, 255, 0.7)",
        bordercolor = "#333",
        borderwidth = 1,
        borderpad = 4,
        align = "center"
      )
    )
  }
  
  # Add all annotations to plot
  p <- p %>% plotly::layout(annotations = annotations_list)
  
  return(p)
}