# S_Curve_Charts.R


# Module for creating Flighting and S-Curve Charts using Plotly

source("R/modules/common/s_curve_helpers.R")
library(plotly)
library(dplyr)

# -------------------------------------------------------------------------
# create_flighting_chart():
#   - Shows time series with 6 horizontal lines
#   - Includes 5 colored zones
# -------------------------------------------------------------------------
create_flighting_chart <- function(data_chart, alpha, beta, max_val_pct, decay = 1, lag = 0, var_name = "Variable",
                               calculated_key_points = NULL, avg_period = 52, avg_data = NULL) {
 
  if (!is.data.frame(data_chart) || !all(c("Period", "value") %in% names(data_chart))) {
    return(plot_ly() %>% layout(title = "Invalid data for Flighting Chart."))
  }
  if (nrow(data_chart) < 1) {
    return(plot_ly() %>% layout(title = "No rows for Flighting Chart."))
  }
 
  if (!is.numeric(data_chart$value)) {
    return(plot_ly() %>% layout(title = "The 'value' column is not numeric."))
  }
 
  # Keep original data but filter out zeros for calculations
  var_activity <- data_chart
  
  # Filter out zero values for calculations but keep them for display
  var_activity_no_zeros <- var_activity %>%
    filter(value > 0)
 
  # For percentiles ignore NA but include non-zero values
  val_no_na <- var_activity_no_zeros$value[!is.na(var_activity_no_zeros$value)]
  if (length(val_no_na) == 0) {
    return(plot_ly() %>% layout(title = "All values are NA in Flighting Chart."))
  }
 
  # Real maximum of the activity
  max_activity <- max(val_no_na, na.rm = TRUE)
  if (!is.finite(max_activity) || max_activity == 0) max_activity <- 1
 
  # Calculate "Max_value" based on max_val_pct
  Max_value <- max_activity * (max_val_pct / 100)
  
  # Calculate average from avg_data, ignoring zeros
  if (!is.null(avg_data) && nrow(avg_data) > 0) {
    avg_data_no_zeros <- avg_data %>%
      filter(value > 0)
    
    if (nrow(avg_data_no_zeros) > 0) {
      real_avg_period <- mean(avg_data_no_zeros$value, na.rm = TRUE)
      if (is.na(real_avg_period)) real_avg_period <- 0
      avg_period <- nrow(avg_data_no_zeros)  # Update avg_period to number of rows in filtered avg_data
    } else {
      # If no non-zero data in range, use default calculation
      real_avg_period <- mean(var_activity_no_zeros$value, na.rm = TRUE)
      if (is.na(real_avg_period)) real_avg_period <- 0
    }
  } else {
    # If no avg_data provided, calculate from non-zero values in var_activity
    real_avg_period <- mean(var_activity_no_zeros$value, na.rm = TRUE)
    if (is.na(real_avg_period)) real_avg_period <- 0
  }
 
  # If key points were provided from S-Curve, use those
  if (!is.null(calculated_key_points)) {
    avg_key_point <- paste0(avg_period, "_Period_Avg")
    # Make sure the key point exists
    if (avg_key_point %in% calculated_key_points$Key_point) {
      reference_lines <- data.frame(
        Key_point = c(avg_key_point, "Breakthrough", "Optimal begins",
                    "Saturation begins", "Full saturation", "Max efficiency"),
        Value = c(
          calculated_key_points$Value[calculated_key_points$Key_point == avg_key_point],
          calculated_key_points$Value[calculated_key_points$Key_point == "Breakthrough"],
          calculated_key_points$Value[calculated_key_points$Key_point == "Optimal begins"],
          calculated_key_points$Value[calculated_key_points$Key_point == "Saturation begins"],
          calculated_key_points$Value[calculated_key_points$Key_point == "Full saturation"],
          calculated_key_points$Value[calculated_key_points$Key_point == "Max efficiency"]
        ),
        Color = c("#4DAF4A", "#377EB8", "#000000", "#FF69B4", "#E41A1C", "#FFD700")
      )
    } else {
      # If it doesn't exist, calculate manually
      X_ratio <- min(max(real_avg_period / max_activity, 0), 1)
      exact_X_avg <- X_ratio * Max_value
      
      if (exact_X_avg < 0.001) exact_X_avg <- 0.001
      if (exact_X_avg > Max_value * 0.999) exact_X_avg <- Max_value * 0.999
      
      reference_lines <- data.frame(
        Key_point = c(avg_key_point, "Breakthrough", "Optimal begins",
                     "Saturation begins", "Full saturation", "Max efficiency"),
        Value = c(
          exact_X_avg,
          calculated_key_points$Value[calculated_key_points$Key_point == "Breakthrough"],
          calculated_key_points$Value[calculated_key_points$Key_point == "Optimal begins"],
          calculated_key_points$Value[calculated_key_points$Key_point == "Saturation begins"],
          calculated_key_points$Value[calculated_key_points$Key_point == "Full saturation"],
          calculated_key_points$Value[calculated_key_points$Key_point == "Max efficiency"]
        ),
        Color = c("#4DAF4A", "#377EB8", "#000000", "#FF69B4", "#E41A1C", "#FFD700")
      )
    }
  } else {
    # Generate key points from S-Curve
    i_max <- 1000
    index_seq <- seq(0, i_max, by = 1)
   
    # Mapping: indexing = (index_seq * Max_value) / 100 => domain in X
    data_s_curves <- data.frame(index = index_seq) %>%
      mutate(
        indexing     = (index * Max_value) / 100,
        s_curve_idx  = s_curve_indexing(indexing, alpha, beta),
        first_deriv  = first_derivative(s_curve_idx, "s-origin", alpha, beta),
        second_deriv = second_derivative(s_curve_idx, "s-origin", alpha, beta),
        third_deriv  = third_derivative(s_curve_idx, "s-origin", alpha, beta),
        ROI_max_eff  = ifelse(indexing > 0, s_curve_idx / indexing, NA)
      )
   
    # Identify 5 key points using derivatives/ROI:
    max_1st_deriv <- which.max(data_s_curves$first_deriv)
    max_2nd_deriv <- which.max(data_s_curves$second_deriv)
    min_2nd_deriv <- which.min(data_s_curves$second_deriv)
    max_ROI_eff   <- which.max(data_s_curves$ROI_max_eff)
   
    # Full saturation ~ 98%
    full_saturation_idx <- which.min(abs(data_s_curves$s_curve_idx - 0.98))
    if (length(full_saturation_idx) == 0) full_saturation_idx <- nrow(data_s_curves)
   
    # Map indexing values (X) to the real data scale
    breakthrough_val <- data_s_curves$indexing[max_2nd_deriv]
    optimal_begins_val <- data_s_curves$indexing[max_1st_deriv]
    saturation_begins_val <- data_s_curves$indexing[min_2nd_deriv]
    full_saturation_val <- data_s_curves$indexing[full_saturation_idx]
    max_efficiency_val <- data_s_curves$indexing[max_ROI_eff]
   
    # Calculate X for the average
    X_ratio <- min(max(real_avg_period / max_activity, 0), 1)
    exact_X_avg <- X_ratio * Max_value
    
    if (exact_X_avg < 0.001) exact_X_avg <- 0.001
    if (exact_X_avg > Max_value * 0.999) exact_X_avg <- Max_value * 0.999
   
    # Create dataframe with the 6 horizontal lines
    reference_lines <- data.frame(
      Key_point = c(paste0(avg_period, "_Period_Avg"), "Breakthrough", "Optimal begins",
                  "Saturation begins", "Full saturation", "Max efficiency"),
      Value = c(
        exact_X_avg, # Value for average
        breakthrough_val,
        optimal_begins_val,
        saturation_begins_val,
        full_saturation_val,
        max_efficiency_val
      ),
      Color = c("#4DAF4A", "#377EB8", "#000000", "#FF69B4", "#E41A1C", "#FFD700")
    )
  }
 
  # Min and max period for rectangles
  x_min <- min(var_activity$Period, na.rm = TRUE)
  x_max <- max(var_activity$Period, na.rm = TRUE)
 
  # Scale factor for Y axis
  scale_info <- calculate_scale_info(max_activity)
 
  # Calculate adequate vertical range for the chart
  y_max <- max(max(val_no_na, na.rm = TRUE) * 1.1,
               max(reference_lines$Value, na.rm = TRUE) * 1.05)
  
  # Round up for uniform scale
  y_max <- ceiling(y_max * 1.05)
 
  # Define zones to color
  zones <- list(
    startup = list(
      name = "Startup",
      ymin = 0,
      ymax = reference_lines$Value[reference_lines$Key_point == "Breakthrough"],
      color = "#CCCCCC", # Gray
      alpha = 0.55
    ),
    growth = list(
      name = "Growth",
      ymin = reference_lines$Value[reference_lines$Key_point == "Breakthrough"],
      ymax = reference_lines$Value[reference_lines$Key_point == "Optimal begins"],
      color = "#FFFFFF", # Transparent
      alpha = 0.01
    ),
    optimal = list(
      name = "Optimal Range",
      ymin = reference_lines$Value[reference_lines$Key_point == "Optimal begins"],
      ymax = reference_lines$Value[reference_lines$Key_point == "Saturation begins"],
      color = "#9370DB", # Purple
      alpha = 0.60
    ),
    diminishing = list(
      name = "Diminishing",
      ymin = reference_lines$Value[reference_lines$Key_point == "Saturation begins"],
      ymax = reference_lines$Value[reference_lines$Key_point == "Full saturation"],
      color = "#FFFFFF", # Transparent
      alpha = 0.01
    ),
    saturated = list(
      name = "Saturated",
      ymin = reference_lines$Value[reference_lines$Key_point == "Full saturation"],
      ymax = max(y_max * 1.1, max_activity * 1.1),
      color = "#CCCCCC", # Gray
      alpha = 0.55
    )
  )
 
  # VARIABLES FOR SCALABILITY (easily modifiable)
  red_point_size <- 6      # Larger size for red points
  red_line_width <- 2      # Greater thickness for the line connecting red points
  ref_line_thickness <- 2  # Thickness for horizontal green and yellow lines
 
  # Initialize Plotly chart
  p <- plot_ly(height = 450)
 
  # Add colored zones (without hover to not show zone names)
  for (zone_name in names(zones)) {
    zone <- zones[[zone_name]]
   
    p <- p %>% add_trace(
      x = c(x_min, x_max, x_max, x_min, x_min),
      y = c(zone$ymin, zone$ymin, zone$ymax, zone$ymax, zone$ymin),
      type = "scatter",
      mode = "none",
      fill = "toself",
      fillcolor = adjustcolor(zone$color, alpha.f = zone$alpha),
      hoverinfo = "none",  # Don't show hover for zones
      showlegend = FALSE
    )
  }
 
  # Move Saturated zone area right after zones so it doesn't interfere with points
  full_saturation_val <- reference_lines$Value[reference_lines$Key_point == "Full saturation"]
 
  p <- p %>% add_trace(
    x = c(x_min, x_max, x_max, x_min, x_min),
    y = c(full_saturation_val, full_saturation_val, y_max*1.1, y_max*1.1, full_saturation_val),
    type = "scatter",
    mode = "none",
    fill = "toself",
    fillcolor = adjustcolor("#CCCCCC", alpha.f = 0.55),  # Same color as startup
    hoverinfo = "none",  # Don't show hover for zones
    showlegend = FALSE
  )
 
  # Add time series line (black connected line, with greater thickness)
  p <- p %>% add_trace(
    data = var_activity,
    x = ~Period,
    y = ~value,
    type = "scatter",
    mode = "lines",
    line = list(color = "#1A1A1A", width = red_line_width),
    hoverinfo = "none",
    showlegend = FALSE
  )
 
  # Add horizontal reference lines with thickness adjustment for green and yellow
  for (i in 1:nrow(reference_lines)) {
    line <- reference_lines[i, ]
   
    # Change specific colors to dark gray
    lineColor <- line$Color
    if (lineColor %in% c("#377EB8", "#FF69B4", "#E41A1C")) {
      lineColor <- "#444444"  # Dark gray for lines that were blue, pink or red
    }
   
    # Determine if line should be dash or solid and adjust thickness for green and yellow
    if (line$Key_point %in% c(paste0(avg_period, "_Period_Avg"), "Max efficiency")) {
      lineDash <- "dash"
      line_width <- ref_line_thickness
    } else {
      lineDash <- "solid"
      line_width <- 1.0
    }
   
    # Improve hover text with HTML format for better visualization
    hover_text <- paste0("<b>", line$Key_point, "</b><br>",
                         "Value: ", format(round(line$Value, 2), big.mark = ","))
   
    p <- p %>% add_trace(
      x = c(x_min, x_max),
      y = c(line$Value, line$Value),
      type = "scatter",
      mode = "lines",
      line = list(color = lineColor, width = line_width, dash = lineDash),
      hoverinfo = "text",
      text = hover_text,
      hoveron = "points+lines",  # Hover on the entire line, not just extremes
      showlegend = FALSE
    )
  }
 
  # Finally, add red points (only for non-zero values)
  # Filter out very low values for the markers
  var_activity_for_markers <- var_activity %>%
    filter(value > max_activity * 0.01)  # Only show points for values > 1% of max
  
  p <- p %>% add_trace(
    data = var_activity_for_markers,
    x = ~Period,
    y = ~value,
    type = "scatter",
    mode = "markers",
    marker = list(color = "#E41A1C", size = red_point_size),
    hoverinfo = "text",
    text = ~paste(
      "<b>Date:</b>", format(Period, "%Y-%m-%d"),
      "<br><b>Value:</b>", format(round(value, 2), big.mark = ",")
    ),
    showlegend = FALSE
  )
  
  # Calculate exact ticks for Y axis
  # Make sure it matches the X axis of the S curve
  tick_count <- 6  # Number of ticks
  tick_values <- round(seq(0, y_max, length.out = tick_count))
  
  # Custom function to format axis labels without duplicates
  format_axis_label <- function(value) {
    if (value >= 1000) {
      suffix <- "k"
      formatted_value <- value / 1000
      return(paste0(formatted_value, suffix))
    } else {
      return(as.character(value))
    }
  }
  
  # Generate unique axis labels
  axis_labels <- sapply(tick_values, format_axis_label)
 
  # Configure chart layout with specific ticks
  p <- p %>% layout(
    xaxis = list(
      title = list(
        text = "Period",
        font = list(size = 12, family = "Arial, sans-serif", color = "#000000"),
        standoff = 15  # Increase distance between title and axis
      ),
      type = "date",
      range = c(x_min, x_max),
      tickformat = "%Y-%m",
      dtick = "M3",
      tickangle = -45,
      gridcolor = "#EEEEEE",
      automargin = TRUE  # Allow auto margin to avoid cropping
    ),
    yaxis = list(
      title = list(
        text = "Activity",
        font = list(size = 12, family = "Arial, sans-serif", color = "#000000"),
        standoff = 15  # Increase distance between title and axis
      ),
      range = c(0, y_max),
      tickmode = "array",  # Use specific ticks
      tickvals = tick_values,  # Tick values
      ticktext = axis_labels,  # Custom formatted labels
      gridcolor = "#EEEEEE",
      automargin = TRUE  # Allow auto margin to avoid cropping
    ),
    hovermode = "closest",
    hoverdistance = 50,  # Increase hover detection distance
    hoverlabel = list(
      bgcolor = "white",
      font = list(size = 12, color = "black", family = "Arial, sans-serif"),
      bordercolor = "darkgray",
      namelength = -1  # No limit for name length
    ),
    showlegend = FALSE,
    margin = list(l = 70, r = 50, b = 80, t = 20, pad = 5),  # Increase margins
    paper_bgcolor = "#FFFFFF",
    plot_bgcolor = "#FFFFFF"
  ) %>%
  config(
    displayModeBar = FALSE,
    responsive = TRUE
  )
 
  # Save configuration values to coordinate with S-Curve
  attr(p, "key_points") <- reference_lines
  attr(p, "y_max") <- y_max
  attr(p, "tick_values") <- tick_values
  attr(p, "axis_labels") <- axis_labels
  return(p)
}


# -------------------------------------------------------------------------
# create_s_curve_chart():
#   - Plots the S curve in the range 0..1000 (i_max).
#   - Includes 6 key points: Breakthrough, Optimal begins, Saturation begins,
#     Full saturation, Max efficiency and 52w_Avg.
# -------------------------------------------------------------------------
create_s_curve_chart <- function(data_chart, alpha, beta, max_val_pct, decay = 1, lag = 0, var_name = "Variable", 
                                avg_period = 52, avg_data = NULL, flighting_chart = NULL) {
 
  if (!is.data.frame(data_chart) || !all(c("Period", "value") %in% names(data_chart))) {
    return(plot_ly() %>% layout(title = "Invalid data for S-Curve Chart."))
  }
  
  # Filter out zeros for calculations
  data_chart_no_zeros <- data_chart %>%
    filter(value > 0)
 
  val_no_na <- data_chart_no_zeros$value[!is.na(data_chart_no_zeros$value)]
  if (length(val_no_na) < 1) {
    return(plot_ly() %>% layout(title = "No valid data for S-Curve."))
  }
 
  # Real maximum of the activity
  max_activity <- max(val_no_na, na.rm = TRUE)
  if (!is.finite(max_activity) || max_activity == 0) max_activity <- 1
 
  # Determine "Max_value" based on max_val_pct
  Max_value <- max_activity * (max_val_pct / 100)
  
  # If we have a flighting_chart, get its configuration values
  y_max <- NULL
  tick_values <- NULL
  axis_labels <- NULL
  if (!is.null(flighting_chart)) {
    y_max <- attr(flighting_chart, "y_max")
    tick_values <- attr(flighting_chart, "tick_values")
    axis_labels <- attr(flighting_chart, "axis_labels")
  }
  
  # If we don't have values from flighting_chart, calculate our own values
  if (is.null(y_max)) {
    y_max <- ceiling(max_activity * 1.1)
  }
  
  if (is.null(tick_values)) {
    tick_count <- 6  # Number of ticks
    tick_values <- round(seq(0, y_max, length.out = tick_count))
  }
  
  if (is.null(axis_labels)) {
    axis_labels <- as.character(tick_values)
  }
 
  # Generate points 0..i_max for 'index'
  i_max <- 1000
  index_seq <- seq(0, i_max, by = 1)
 
  # Mapping: indexing = (index_seq * Max_value) / 100 => domain in X
  data_s_curves <- data.frame(index = index_seq) %>%
    mutate(
      indexing     = (index * Max_value) / 100,
      s_curve_idx  = s_curve_indexing(indexing, alpha, beta),
      first_deriv  = first_derivative(s_curve_idx, "s-origin", alpha, beta),
      second_deriv = second_derivative(s_curve_idx, "s-origin", alpha, beta),
      third_deriv  = third_derivative(s_curve_idx, "s-origin", alpha, beta),
      ROI_max_eff  = ifelse(indexing > 0, s_curve_idx / indexing, NA)
    )
 
  # Identify key points using derivatives/ROI:
  max_1st_deriv <- which.max(data_s_curves$first_deriv)
  max_2nd_deriv <- which.max(data_s_curves$second_deriv)
  min_2nd_deriv <- which.min(data_s_curves$second_deriv)
  max_ROI_eff <- which.max(data_s_curves$ROI_max_eff)
 
  # Full saturation ~ 98%
  full_saturation_idx <- which.min(abs(data_s_curves$s_curve_idx - 0.98))
  if (length(full_saturation_idx) == 0) full_saturation_idx <- nrow(data_s_curves)
 
  # Create data.frame with key points
  key_points_df <- data.frame(
    Key_point = c(
      "Breakthrough", "Optimal begins", "Saturation begins",
      "Full saturation", "Max efficiency"
    ),
    Index_X = c(
      data_s_curves$indexing[max_2nd_deriv],
      data_s_curves$indexing[max_1st_deriv],
      data_s_curves$indexing[min_2nd_deriv],
      data_s_curves$indexing[full_saturation_idx],
      data_s_curves$indexing[max_ROI_eff]
    ),
    S_Curve_Value = c(
      data_s_curves$s_curve_idx[max_2nd_deriv],
      data_s_curves$s_curve_idx[max_1st_deriv],
      data_s_curves$s_curve_idx[min_2nd_deriv],
      data_s_curves$s_curve_idx[full_saturation_idx],
      data_s_curves$s_curve_idx[max_ROI_eff]
    ),
    Value = c(
      data_s_curves$indexing[max_2nd_deriv],
      data_s_curves$indexing[max_1st_deriv],
      data_s_curves$indexing[min_2nd_deriv],
      data_s_curves$indexing[full_saturation_idx],
      data_s_curves$indexing[max_ROI_eff]
    ),
    Color = c("#000000", "#000000", "#000000", "#000000", "#FFD700")
  )
 
  # Calculate average using filtered data if available
  if (!is.null(avg_data) && nrow(avg_data) > 0) {
    # Filter out zeros
    avg_data_no_zeros <- avg_data %>%
      filter(value > 0)
    
    if (nrow(avg_data_no_zeros) > 0) {
      real_avg_period <- mean(avg_data_no_zeros$value, na.rm = TRUE)
      if (is.na(real_avg_period)) real_avg_period <- 0
      avg_period <- nrow(avg_data_no_zeros)  # Update avg_period to number of rows in filtered avg_data
    } else {
      # If no non-zero data in range, use default calculation
      real_avg_period <- mean(data_chart_no_zeros$value, na.rm = TRUE)
      if (is.na(real_avg_period)) real_avg_period <- 0
    }
  } else {
    # If no avg_data provided, calculate from non-zero values
    real_avg_period <- mean(data_chart_no_zeros$value, na.rm = TRUE)
    if (is.na(real_avg_period)) real_avg_period <- 0
  }
 
  X_ratio <- min(max(real_avg_period / max_activity, 0), 1)
  X_avg <- X_ratio * Max_value
 
  if (X_avg < 0.001) X_avg <- 0.001
  if (X_avg > Max_value * 0.999) X_avg <- Max_value * 0.999
 
  Y_avg <- find_y_for_x_on_curve(X_avg, alpha, beta)
 
  closest_point_idx <- which.min(abs(data_s_curves$indexing - X_avg))
 
  if (length(closest_point_idx) > 0 && closest_point_idx <= nrow(data_s_curves)) {
    exact_X_avg <- data_s_curves$indexing[closest_point_idx]
    exact_Y_avg <- data_s_curves$s_curve_idx[closest_point_idx]
  } else {
    exact_X_avg <- X_avg
    exact_Y_avg <- Y_avg
  }
 
  # Add average point
  key_points_df <- rbind(
    key_points_df,
    data.frame(
      Key_point     = paste0(avg_period, "_Period_Avg"),
      Index_X       = exact_X_avg,
      S_Curve_Value = exact_Y_avg,
      Value         = exact_X_avg,
      Color         = "#4DAF4A" # Green
    )
  )
 
  # Scale factor
  scale_info <- calculate_scale_info(max_activity)
 
  # Adjust X limits to coordinate with flighting chart
  x_max_value <- y_max  # Use same max value as Y axis of flighting chart
 
  # Initialize Plotly chart
  p <- plot_ly()
 
  # Define zone coordinates (with hoverinfo = "none" to not show names)
  zones <- list(
    startup = list(
      name = "Startup",
      xmin = 0,
      xmax = key_points_df$Index_X[key_points_df$Key_point == "Breakthrough"],
      color = "#FFFFFF",
      alpha = 0.55
    ),
    growth = list(
      name = "Growth",
      xmin = key_points_df$Index_X[key_points_df$Key_point == "Breakthrough"],
      xmax = key_points_df$Index_X[key_points_df$Key_point == "Optimal begins"],
      color = "#FFFFFF",
      alpha = 0.01
    ),
    optimal = list(
      name = "Optimal Range",
      xmin = key_points_df$Index_X[key_points_df$Key_point == "Optimal begins"],
      xmax = key_points_df$Index_X[key_points_df$Key_point == "Saturation begins"],
      color = "#9370DB",
      alpha = 0.6
    ),
    diminishing = list(
      name = "Diminishing",
      xmin = key_points_df$Index_X[key_points_df$Key_point == "Saturation begins"],
      xmax = key_points_df$Index_X[key_points_df$Key_point == "Full saturation"],
      color = "#FFFFFF",
      alpha = 0.01
    ),
    saturated = list(
      name = "Saturated",
      xmin = key_points_df$Index_X[key_points_df$Key_point == "Full saturation"],
      xmax = x_max_value,
      color = "#FFFFFF",
      alpha = 0.55
    )
  )
 
  # Add zones
  for (zone_name in names(zones)) {
    zone <- zones[[zone_name]]
   
    p <- p %>% add_trace(
      x = c(zone$xmin, zone$xmax, zone$xmax, zone$xmin, zone$xmin),
      y = c(0, 0, 1, 1, 0),
      type = "scatter",
      mode = "none",
      fill = "toself",
      fillcolor = adjustcolor(zone$color, alpha.f = zone$alpha),
      hoverinfo = "none",  # Don't show hover for zones
      showlegend = FALSE
    )
  }
 
  # Add S curve (slightly thicker)
  p <- p %>% add_trace(
    data = data_s_curves,
    x = ~indexing,
    y = ~s_curve_idx,
    type = "scatter",
    mode = "lines",
    line = list(color = "#1A1A1A", width = 1.2),
    hoverinfo = "none",
    showlegend = FALSE
  )
 
  # VARIABLE FOR S CURVE POINT SCALABILITY
  s_point_size <- 7
 
  # Add key points (larger size for points, using s_point_size)
  for (i in 1:nrow(key_points_df)) {
    point <- key_points_df[i, ]
   
    # Change specific colors to dark gray (for points that were red, pink or blue)
    pointColor <- point$Color
    if (pointColor %in% c("#377EB8", "#FF69B4", "#E41A1C")) {
      pointColor <- "#444444"  # Dark gray
    }
   
    p <- p %>% add_trace(
      x = point$Index_X,
      y = point$S_Curve_Value,
      type = "scatter",
      mode = "markers",
      marker = list(
        color = pointColor,
        size = s_point_size
      ),
      hoverinfo = "text",
      text = paste0(
        "<b>", point$Key_point, "</b><br>",
        "X: ", format(round(point$Index_X, 2), big.mark = ","), "<br>",
        "Y: ", format(round(point$S_Curve_Value * 100, 1), big.mark = ","), "%"
      ),
      showlegend = FALSE
    )
  }
 
  # Configure chart layout with identical ticks to flighting chart
  p <- p %>% layout(
    xaxis = list(
      title = list(
        text = "Activity",
        font = list(size = 12, family = "Arial, sans-serif", color = "#000000"),
        standoff = 15
      ),
      range = c(0, x_max_value),
      tickmode = "array",
      tickvals = tick_values,  # Use same tick values as Y axis of flighting chart
      ticktext = axis_labels,  # Use same custom labels
      gridcolor = "#EEEEEE",
      automargin = TRUE
    ),
    yaxis = list(
      title = list(
        text = "Index Value",
        font = list(size = 12, family = "Arial, sans-serif", color = "#000000"),
        standoff = 15
      ),
      tickformat = ".0%",
      range = c(0, 1),
      gridcolor = "#EEEEEE",
      automargin = TRUE
    ),
    hovermode = "closest",
    hoverdistance = 50,
    hoverlabel = list(
      bgcolor = "white",
      font = list(size = 12, color = "black", family = "Arial, sans-serif"),
      bordercolor = "darkgray",
      namelength = -1
    ),
    showlegend = FALSE,
    margin = list(l = 70, r = 50, b = 70, t = 20, pad = 5),
    paper_bgcolor = "#FFFFFF",
    plot_bgcolor = "#FFFFFF"
  ) %>%
  config(
    displayModeBar = FALSE,
    responsive = TRUE
  )
 
  # Save key points to use in flighting chart
  attr(p, "key_points") <- key_points_df
  return(p)
}