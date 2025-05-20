# charts_flighting.R
# =============================================================================
# Contiene:
# 1. render_variable_flighting: Renderiza un gráfico comparando KPI y una variable seleccionada (sin transformar).
#
# Nota: Este módulo asume que las funciones de Shiny (req, validate, need) y plotly
#       están disponibles en el entorno de ejecución.
# =============================================================================

# Asegurarse de que las dependencias necesarias estén disponibles
# library(shiny)
# library(plotly)
# library(dplyr) # Opcional

# =============================================================================
# Renders a flighting chart comparing KPI and the selected variable (ENHANCED)
# =============================================================================
render_variable_flighting <- function(df, kpi_univ, variable_univ, geography_univ) {
  # --- Validaciones Iniciales ---
  shiny::req(df, kpi_univ, variable_univ, geography_univ)
  shiny::validate(
    shiny::need(variable_univ != "N/A" && !is.null(variable_univ) && variable_univ != "", "Please select a valid variable for analysis."),
    shiny::need(kpi_univ != "N/A" && !is.null(kpi_univ) && kpi_univ != "", "Please select a valid KPI.")
  )

  date_col <- if ("Period" %in% names(df)) "Period" else if ("periodo" %in% names(df)) "periodo" else NULL
  shiny::validate(shiny::need(!is.null(date_col), "Date column ('Period' or 'periodo') not found."))
  shiny::validate(shiny::need(kpi_univ %in% names(df), paste("KPI column '", kpi_univ, "' not found.")))
  shiny::validate(shiny::need(variable_univ %in% names(df), paste("Variable column '", variable_univ, "' not found.")))

  # --- Preparación de Datos ---
  data_to_plot <- tryCatch({
    df_temp <- df
    df_temp[["Date_Plot"]] <- as.Date(df_temp[[date_col]]) # Usar un nombre consistente
    df_temp <- df_temp[!is.na(df_temp[["Date_Plot"]]), ]
    if (nrow(df_temp) == 0) stop("No valid date entries after NA removal.")
    
    df_temp <- df_temp[order(df_temp[["Date_Plot"]]), ]
    
    df_temp[["KPI_Value"]] <- as.numeric(df_temp[[kpi_univ]])
    df_temp[["Variable_Value"]] <- as.numeric(df_temp[[variable_univ]])
    df_temp[["Hover_Date"]] <- format(df_temp[["Date_Plot"]], "%b %Y")

    if (all(is.na(df_temp[["KPI_Value"]]))) stop("All KPI values are NA or non-numeric.")
    if (all(is.na(df_temp[["Variable_Value"]]))) stop("All Variable values are NA or non-numeric.")
    
    df_temp
  }, error = function(e) {
    warning(paste("Error preparing data for flighting chart:", e$message))
    NULL
  })
  
  shiny::validate(shiny::need(!is.null(data_to_plot) && nrow(data_to_plot) > 0, "Failed to prepare data for flighting chart. Check input columns and values."))

  # --- Calcular Correlación, P-valor y Estadísticas ---
  valid_cor_idx <- which(is.finite(data_to_plot$KPI_Value) & is.finite(data_to_plot$Variable_Value))
  correlation <- NA_real_
  p_value <- NA_real_
  sig_stars <- ""

  if (length(valid_cor_idx) >= 3) {
    kpi_subset <- data_to_plot$KPI_Value[valid_cor_idx]
    var_subset <- data_to_plot$Variable_Value[valid_cor_idx]

    if (stats::sd(kpi_subset, na.rm = TRUE) != 0 && stats::sd(var_subset, na.rm = TRUE) != 0) {
      correlation <- stats::cor(kpi_subset, var_subset)
      test_result <- tryCatch(stats::cor.test(kpi_subset, var_subset), error = function(e) NULL)
      if (!is.null(test_result)) p_value <- test_result$p.value

      if (!is.na(p_value)) {
        if (p_value < 0.001) sig_stars <- "***"
        else if (p_value < 0.01) sig_stars <- "**"
        else if (p_value < 0.05) sig_stars <- "*"
        else if (p_value < 0.1) sig_stars <- "."
      }
    }
  }
  
  kpi_stats <- list(
    mean = mean(data_to_plot$KPI_Value, na.rm = TRUE),
    min = min(data_to_plot$KPI_Value[is.finite(data_to_plot$KPI_Value)], na.rm = TRUE),
    max = max(data_to_plot$KPI_Value[is.finite(data_to_plot$KPI_Value)], na.rm = TRUE)
  )
  var_stats <- list(
    mean = mean(data_to_plot$Variable_Value, na.rm = TRUE),
    min = min(data_to_plot$Variable_Value[is.finite(data_to_plot$Variable_Value)], na.rm = TRUE),
    max = max(data_to_plot$Variable_Value[is.finite(data_to_plot$Variable_Value)], na.rm = TRUE)
  )

  # --- Título ---
  title_text <- paste0("'", kpi_univ, "' vs '", variable_univ, "' (", geography_univ, ")")
  if (!is.na(correlation)) {
    title_text <- paste0(title_text, " - Correlation: ", round(correlation, 2), sig_stars)
  }

  # --- Rangos de Ejes Y ---
  determine_plot_range <- function(min_val, max_val, mean_val) { # Misma función que en el otro chart
    if (!is.finite(min_val) && !is.finite(max_val)) { 
        center <- if (is.finite(mean_val) && mean_val != 0) mean_val else if (is.finite(mean_val) && mean_val == 0) 0 else 1
        return(if(center==0) c(-1,1) else c(center * 0.9, center * 1.1))
    }
    if(!is.finite(min_val)) min_val <- if(is.finite(max_val)) max_val - abs(max_val*0.2 +1) else if(is.finite(mean_val)) mean_val - abs(mean_val*0.2+1) else -1
    if(!is.finite(max_val)) max_val <- if(is.finite(min_val)) min_val + abs(min_val*0.2+1) else if(is.finite(mean_val)) mean_val + abs(mean_val*0.2+1) else 1
    
    current_range <- max_val - min_val
    if (current_range == 0 || !is.finite(current_range)) {
        center <- if (min_val != 0) min_val else if (mean_val !=0) mean_val else 0
        return(if(center==0) c(-0.5,0.5) else c(center - abs(center*0.1)+0.01, center + abs(center*0.1)+0.01))
    }
    padding <- current_range * 0.1
    return(c(min_val - padding, max_val + padding))
  }
  kpi_y_range <- determine_plot_range(kpi_stats$min, kpi_stats$max, kpi_stats$mean)
  var_y_range <- determine_plot_range(var_stats$min, var_stats$max, var_stats$mean)

  # --- Configuración Plotly ---
  font_family <- "Arial, sans-serif"
  axis_title_size <- 12
  tick_font_size <- 10
  legend_font_size <- 11
  main_title_size <- 15
  
  color_kpi <- "#1f77b4"
  color_var <- "#ff7f0e"      # Naranja Plotly (diferente de la transformada)
  grid_color <- "#e8e8e8"
  plot_bg_color <- "#f9f9f9"
  paper_bg_color <- "#ffffff"

  p <- plotly::plot_ly(data_to_plot, x = ~Date_Plot, source = "flighting_plot") %>%
    plotly::add_trace(
      y = ~KPI_Value, name = kpi_univ, type = "scatter", mode = "lines+markers",
      line = list(color = color_kpi, width = 2), marker = list(color = color_kpi, size = 5, line = list(color=paper_bg_color, width=1)),
      yaxis = "y1", hoverinfo = "text",
      text = ~paste0("<b>", Hover_Date, "</b><br>", kpi_univ, ": ", round(KPI_Value,2), "<extra></extra>")
    ) %>%
    plotly::add_trace(
      y = ~Variable_Value, name = variable_univ, type = "scatter", mode = "lines+markers",
      line = list(color = color_var, width = 2), marker = list(color = color_var, size = 5, line = list(color=paper_bg_color, width=1)),
      yaxis = "y2", hoverinfo = "text",
      text = ~paste0("<b>", Hover_Date, "</b><br>", variable_univ, ": ", round(Variable_Value,2), "<extra></extra>")
    ) %>%
    plotly::layout(
      title = list(text = title_text, font = list(family = font_family, size = main_title_size, color = "#333"), y = 0.97, x = 0.05, xanchor = 'left'),
      xaxis = list(
        title = list(text="Time Period", font=list(family=font_family, size=axis_title_size), standoff=10),
        type = "date", tickformat = "%b %Y", dtick = "M3", tickangle = -30,
        gridcolor = grid_color, automargin = TRUE,
        tickfont = list(family = font_family, size = tick_font_size),
        rangeslider = list(visible = FALSE),
        rangeselector = list(
            buttons = list(
                list(count=6, label="6m", step="month", stepmode="backward"),
                list(count=1, label="1Y", step="year", stepmode="backward"),
                list(count=2, label="2Y", step="year", stepmode="backward"),
                list(step="all", label="All")),
            x = 0.01, y = 1.1, yanchor="top", bgcolor = "#efefef", font = list(family = font_family, size = 10)
        )
      ),
      yaxis = list(
        title = list(text=kpi_univ, font=list(family=font_family, size=axis_title_size, color=color_kpi), standoff=10),
        range = kpi_y_range, side = "left", automargin = TRUE, gridcolor = grid_color,
        tickfont = list(family = font_family, size = tick_font_size, color=color_kpi),
        showline = TRUE, linecolor = color_kpi, linewidth = 1.5, zerolinecolor = grid_color
      ),
      yaxis2 = list(
        title = list(text=variable_univ, font=list(family=font_family, size=axis_title_size, color=color_var), standoff=10),
        range = var_y_range, overlaying = "y", side = "right", automargin = TRUE,
        gridcolor = "transparent", tickfont = list(family = font_family, size = tick_font_size, color=color_var),
        showline = TRUE, linecolor = color_var, linewidth = 1.5, zerolinecolor = grid_color
      ),
      legend = list(
        orientation = "h", xanchor = "center", x = 0.5, y = -0.2,
        font = list(family = font_family, size = legend_font_size),
        bgcolor = "rgba(255,255,255,0.8)", bordercolor = "#ccc", borderwidth = 1
      ),
      margin = list(l = 60, r = 70, t = 80, b = 80, pad = 5), # t ajustado para rangeselector
      hovermode = "x unified",
      plot_bgcolor = plot_bg_color,
      paper_bgcolor = paper_bg_color
    )

  # --- Añadir Líneas de Media y Anotación de P-valor ---
  plot_shapes <- list()
  plot_annotations <- list()

  if (is.finite(kpi_stats$mean)) {
    plot_shapes[[length(plot_shapes) + 1]] <- list(
      type = "line", layer = "below",
      x0 = min(data_to_plot$Date_Plot, na.rm = TRUE), x1 = max(data_to_plot$Date_Plot, na.rm = TRUE),
      y0 = kpi_stats$mean, y1 = kpi_stats$mean, yref = "y1",
      line = list(color = sprintf("rgba(%s,0.5)", paste(grDevices::col2rgb(color_kpi), collapse=",")), width = 1, dash = "dot") # Usar color con alpha
    )
  }
  if (is.finite(var_stats$mean)) {
    plot_shapes[[length(plot_shapes) + 1]] <- list(
      type = "line", layer = "below",
      x0 = min(data_to_plot$Date_Plot, na.rm = TRUE), x1 = max(data_to_plot$Date_Plot, na.rm = TRUE),
      y0 = var_stats$mean, y1 = var_stats$mean, yref = "y2",
      line = list(color = sprintf("rgba(%s,0.5)", paste(grDevices::col2rgb(color_var), collapse=",")), width = 1, dash = "dot")
    )
  }
  
  # Anotación de P-valor (si es significativo)
  if (!is.na(p_value) && p_value < 0.1) { # Mostrar si p < 0.1
      plot_annotations[[length(plot_annotations) + 1]] <- list(
          x = 1, y = 1.05, xref = "paper", yref = "paper", xanchor = "right", yanchor="bottom",
          text = paste0("p-value: ", format(p_value, digits = 2, nsmall = 2), sig_stars),
          showarrow = FALSE, font = list(family = font_family, size = 10, color = if(p_value < 0.05) "#28a745" else "#6c757d") # Verde si <0.05
      )
  }

  if (length(plot_shapes) > 0) p <- p %>% plotly::layout(shapes = plot_shapes)
  if (length(plot_annotations) > 0) p <- p %>% plotly::layout(annotations = plot_annotations)
  
  return(p)
}
