# R/modules/common/transformations.R
# =============================================================================
# Módulo: transformations
# Contiene funciones para transformar datos, calcular derivadas, manejar
# escalas, detectar outliers y generar gráficas (Flighting y S-Curve).
# =============================================================================

# Función de transformación S-Curve
s_curve_transform <- function(x, shape = "s-origin", alpha = 0.85, beta = 1, maxValuePct = 100) {
  if (!is.numeric(x) || length(x) == 0) return(numeric(0))
  if (all(is.na(x))) return(x)
  max_val <- max(x, na.rm = TRUE)
  if (!is.finite(max_val) || max_val == 0) return(x)
  
  i_max <- length(x)
  result <- switch(
    tolower(shape),
    "s-shaped" = {
      (beta / 1e10)^(alpha^seq_len(i_max))
    },
    "s-origin" = {
      (beta / 1e9)^(alpha^seq_len(i_max)) - (beta / 1e9)
    },
    "indexp" = {
      1 - exp(-alpha * seq_len(i_max) / 10)
    },
    x
  )
  return(result)
}

# Función para indexar la S-Curve
s_curve_indexing <- function(serie, alpha, beta, index_step = 1) {
  i_max <- if (is.data.frame(serie)) nrow(serie) else length(serie)
  if (i_max < 1) return(numeric(0))
  index <- seq(0, (i_max - 1), by = index_step)
  beta_calc <- beta / 1e9
  beta_calc^(alpha^index) - beta_calc
}

# Primera derivada
first_derivative <- function(x, shape = "s-origin", alpha, beta, index_step = 1) {
  i_max <- if (is.data.frame(x)) nrow(x) else length(x)
  if (i_max < 1) return(numeric(0))
  index <- seq(0, (i_max - 1), by = index_step)
  switch(
    tolower(shape),
    "s-origin" = (beta / 1e9)^(alpha^index) * alpha^index * log(beta / 1e9) * log(alpha),
    "s-shaped" = (beta / 1e10)^(alpha^index) * alpha^index * log(beta / 1e10) * log(alpha),
    "indexp" = (alpha * exp(-(alpha * index) / 10)) / 10,
    rep(NA, i_max)
  )
}

# Segunda derivada
second_derivative <- function(x, shape = "s-origin", alpha, beta, index_step = 1) {
  i_max <- if (is.data.frame(x)) nrow(x) else length(x)
  if (i_max < 1) return(numeric(0))
  index <- seq(0, (i_max - 1), by = index_step)
  switch(
    tolower(shape),
    "s-origin" = {
      log(beta / 1e9) * log(alpha) * (
        ((beta / 1e9)^(alpha^index) * (alpha^(2 * index)) * log(beta / 1e9) * log(alpha)) +
          ((beta / 1e9)^(alpha^index) * (alpha^index) * log(alpha))
      )
    },
    "s-shaped" = {
      log(beta / 1e10) * log(alpha) * (
        ((beta / 1e10)^(alpha^index) * (alpha^(2 * index)) * log(beta / 1e10) * log(alpha)) +
          ((beta / 1e10)^(alpha^index) * (alpha^index) * log(alpha))
      )
    },
    "indexp" = (-(alpha^2) * exp(-(alpha * index) / 10)) / 100,
    rep(NA, i_max)
  )
}

# Tercera derivada
third_derivative <- function(x, shape = "s-origin", alpha, beta, index_step = 1) {
  i_max <- if (is.data.frame(x)) nrow(x) else length(x)
  if (i_max < 1) return(numeric(0))
  index <- seq(0, (i_max - 1), by = index_step)
  switch(
    tolower(shape),
    "s-origin" = {
      log(beta / 1e9) * log(alpha) * (
        ((beta / 1e9)^(alpha^index) * (alpha^(3 * index)) * (log(beta / 1e9))^2 * (log(alpha))^2) +
          3 * ((beta / 1e9)^(alpha^index) * (alpha^(2 * index)) * log(beta / 1e9) * (log(alpha))^2) +
          ((beta / 1e9)^(alpha^index) * (alpha^index) * (log(alpha))^2)
      )
    },
    "s-shaped" = {
      log(beta / 1e10) * log(alpha) * (
        ((beta / 1e10)^(alpha^index) * (alpha^(3 * index)) * (log(beta / 1e10))^2 * (log(alpha))^2) +
          3 * ((beta / 1e10)^(alpha^index) * (alpha^(2 * index)) * log(beta / 1e10) * (log(alpha))^2) +
          ((beta / 1e10)^(alpha^index) * (alpha^index) * (log(alpha))^2)
      )
    },
    "indexp" = ((alpha^3) * exp(-(alpha * index) / 10)) / 1000,
    rep(NA, i_max)
  )
}

# Función para aplicar la transformación según el tipo seleccionado
apply_transformation <- function(data, type = "Linear", alpha = 1, beta = 1, maxval = 100, decay = 1, lag = 0) {
  if (is.null(data) || length(data) == 0) return(numeric(0))
  if (all(is.na(data))) return(data)
  # Forzar a 0 valores negativos
  data[data < 0] <- 0
  
  if (lag > 0) {
    if (lag >= length(data)) {
      data <- rep(NA, length(data))
    } else {
      data <- c(rep(NA, lag), head(data, -lag))
    }
  }
  data <- data * decay
  
  out <- switch(
    type,
    "Linear" = data,
    "S Origin" = s_curve_transform(data, "s-origin", alpha, beta, maxval),
    "S Shaped" = s_curve_transform(data, "s-shaped", alpha, beta, maxval),
    "Index Exp" = 1 - exp(-alpha * data),
    "Log" = log1p(data),
    "Exp" = exp(data),
    "Power" = data^alpha,
    "Moving Avg" = {
      if (length(data) < 3) rep(NA, length(data)) else zoo::rollmean(data, k = 3, fill = NA, align = "right")
    },
    data
  )
  return(out)
}

# Funciones auxiliares de escalado y manejo de outliers
calculate_scale_info <- function(mean_value) {
  if (mean_value < 1000) {
    list(number = 1, suffix = "")
  } else if (mean_value < 1e6) {
    list(number = 1e3, suffix = "K")
  } else if (mean_value < 1e9) {
    list(number = 1e6, suffix = "M")
  } else if (mean_value < 1e12) {
    list(number = 1e9, suffix = "B")
  } else {
    list(number = 1e12, suffix = "T")
  }
}

handle_outliers <- function(data, method = "iqr", threshold = 1.5) {
  if (length(data) < 4) return(data)
  
  q1 <- quantile(data, 0.25, na.rm = TRUE)
  q3 <- quantile(data, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  lower_bound <- q1 - threshold * iqr
  upper_bound <- q3 + threshold * iqr
  
  data[data < lower_bound | data > upper_bound] <- NA
  return(data)
}

# =============================================================================
# Funciones para generar las gráficas Flighting y S-Curve
# =============================================================================

create_flighting_chart <- function(
  data_chart,
  alpha,
  beta,
  max_val_pct,
  decay = 1,
  lag = 0,
  var_name = "Variable"
) {
  if (!is.data.frame(data_chart) || !all(c("Period", "value") %in% names(data_chart))) {
    return(plot_ly() %>% layout(title = "Datos inválidos para flighting chart."))
  }
  if (nrow(data_chart) < 1) {
    return(plot_ly() %>% layout(title = "No hay filas para flighting chart."))
  }
  if (!is.numeric(data_chart$value)) {
    return(plot_ly() %>% layout(title = "La columna 'value' no es numérica."))
  }
  
  var_activity <- data_chart %>% mutate(value = ifelse(value == 0, NA, value))
  
  val_no_na <- var_activity$value[!is.na(var_activity$value)]
  if (length(val_no_na) == 0) {
    return(plot_ly() %>% layout(title = "Todos los valores son NA o cero en flighting chart."))
  }
  qs <- quantile(val_no_na, probs = c(0.2, 0.4, 0.6, 0.8, 1.0))
  key_points <- data.frame(
    Key_point = c("Breakthrough", "Optimal begins", "Saturation begins", "Full saturation", "Max efficiency"),
    indexing = qs
  )
  max_activity <- max(val_no_na, na.rm = TRUE)
  
  n_52 <- min(52, nrow(var_activity))
  last_52 <- tail(var_activity, n_52) %>% filter(value > 0)
  Avg_52w <- mean(last_52$value, na.rm = TRUE)
  if (is.na(Avg_52w)) Avg_52w <- 0
  
  scale_info <- calculate_scale_info(max_activity)
  
  rect_data <- data.frame(
    xmin = min(var_activity$Period, na.rm = TRUE),
    xmax = max(var_activity$Period, na.rm = TRUE),
    ymin = key_points$indexing[key_points$Key_point == "Optimal begins"],
    ymax = key_points$indexing[key_points$Key_point == "Saturation begins"]
  )
  
  p <- ggplot(var_activity, aes(x = Period, y = value)) +
    geom_rect(data = rect_data, inherit.aes = FALSE,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "blue", alpha = 0.2) +
    geom_line(color = "black") +
    geom_point(aes(text = paste("Date:", Period, "<br>Value:", round(value, 2))),
               color = "red", show.legend = FALSE) +
    geom_hline(aes(yintercept = Avg_52w, color = "52-week avg"), linetype = "dashed") +
    scale_color_manual(values = c("52-week avg" = "green"), name = "") +
    scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
    scale_y_continuous(labels = scales::unit_format(unit = scale_info$suffix,
                                                    scale = 1 / scale_info$number,
                                                    big.mark = ","), n.breaks = 10) +
    labs(title = paste("Flighting -", var_name),
         x = "Period", y = var_name) +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplotly(p, tooltip = "text") %>%
    layout(hovermode = "closest", showlegend = TRUE,
           legend = list(orientation = "h", x = 0.5, xanchor = "center")) %>%
    add_annotations(
      x = 0.9, y = 1.02,
      text = "Optimal Range",
      xref = "paper", yref = "paper",
      showarrow = FALSE,
      font = list(color = "blue")
    )
}

create_s_curve_chart <- function(data_chart, alpha, beta, max_val_pct, decay = 1, lag = 0, var_name = "Variable") {
  if (!is.data.frame(data_chart) || !all(c("Period", "value") %in% names(data_chart))) {
    return(plot_ly() %>% layout(title = "Datos inválidos para S-Curve chart."))
  }
  val_no_na <- data_chart$value[!is.na(data_chart$value)]
  if (length(val_no_na) < 1) {
    return(plot_ly() %>% layout(title = "No hay datos válidos para S-Curve."))
  }
  max_activity <- max(val_no_na, na.rm = TRUE)
  if (!is.finite(max_activity) || max_activity == 0) max_activity <- 1
  
  i_max <- 500
  index <- seq(0, i_max, by = 1)
  Max_value <- max_activity * (max_val_pct / 100)
  
  data_s_curves <- data.frame(index = index) %>%
    mutate(
      indexing = (index * Max_value) / 100,
      s_curve_index = s_curve_indexing(indexing, alpha, beta),
      first_deriv = first_derivative(s_curve_index, "s-origin", alpha, beta),
      second_deriv = second_derivative(s_curve_index, "s-origin", alpha, beta),
      third_deriv = third_derivative(s_curve_index, "s-origin", alpha, beta),
      ROI_max_eff = ifelse(indexing != 0, s_curve_index / indexing, NA)
    )
  
  max_1st_deriv <- if (nrow(data_s_curves) > 0) which.max(data_s_curves$first_deriv) else 1
  max_2nd_deriv <- if (nrow(data_s_curves) > 0) which.max(data_s_curves$second_deriv) else 1
  min_2nd_deriv <- if (nrow(data_s_curves) > 0) which.min(data_s_curves$second_deriv) else 1
  max_ROI_eff <- if (nrow(data_s_curves) > 0) which.max(data_s_curves$ROI_max_eff) else 1
  
  full_saturation_idx <- which.min(abs(data_s_curves$s_curve_index - 0.98))
  if (length(full_saturation_idx) == 0) full_saturation_idx <- nrow(data_s_curves)
  
  key_points <- data.frame(
    Key_point = c("Breakthrough", "Optimal begins", "Saturation begins", "Full saturation", "Max efficiency"),
    indexing = c(
      data_s_curves$indexing[max_2nd_deriv],
      data_s_curves$indexing[max_1st_deriv],
      data_s_curves$indexing[min_2nd_deriv],
      data_s_curves$indexing[full_saturation_idx],
      data_s_curves$indexing[max_ROI_eff]
    ),
    s_curve_index = c(
      data_s_curves$s_curve_index[max_2nd_deriv],
      data_s_curves$s_curve_index[max_1st_deriv],
      data_s_curves$s_curve_index[min_2nd_deriv],
      data_s_curves$s_curve_index[full_saturation_idx],
      data_s_curves$s_curve_index[max_ROI_eff]
    )
  )
  
  df52 <- data_chart %>% filter(value > 0)
  n_52 <- min(52, nrow(df52))
  last_52 <- tail(df52, n_52)
  Avg_52w <- mean(last_52$value, na.rm = TRUE)
  if (is.na(Avg_52w)) Avg_52w <- 0
  
  scale_info <- calculate_scale_info(max_activity)
  
  rect_data <- data.frame(
    xmin = key_points$indexing[key_points$Key_point == "Optimal begins"],
    xmax = key_points$indexing[key_points$Key_point == "Saturation begins"],
    ymin = 0,
    ymax = 1
  )
  
  p <- ggplot(data_s_curves, aes(x = indexing, y = s_curve_index)) +
    geom_rect(data = rect_data, inherit.aes = FALSE,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "blue", alpha = 0.2) +
    geom_line(color = "black") +
    geom_point(data = key_points, aes(x = indexing, y = s_curve_index), color = "black") +
    geom_point(data = data.frame(Avg_52w = Avg_52w), 
               aes(x = Avg_52w, y = s_curve_transform(Avg_52w, "s-origin", alpha, beta, max_val_pct)),
               color = "green", size = 3) +
    scale_x_continuous(labels = scales::unit_format(unit = scale_info$suffix, 
                                                    scale = 1 / scale_info$number, 
                                                    big.mark = ","), n.breaks = 10) +
    scale_y_continuous(labels = scales::percent_format(scale = 100), n.breaks = 10) +
    labs(title = paste("S-Curve for", var_name),
         x = "Activity", y = "Index Value") +
    theme_minimal()
  
  ggplotly(p, tooltip = c("x", "y")) %>%
    layout(hovermode = "closest", showlegend = FALSE,
           title = list(
             text = paste0("S-Curve for ", var_name,
                           "<br><sup>Alpha: ", alpha,
                           " | Beta: ", beta,
                           " | MaxVal%: ", max_val_pct,
                           "</sup>")
           ))
}