# S_Curve_Helpers.R


# Módulo de Funciones Helper (Transformaciones, Derivadas, Escalado, Outliers)

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

# -------------------------------------------------------------------------
# s_curve_indexing:
#   Returns the S-Curve Y value given an X (indexing).
#   indexing => 0..(some maximum)
# -------------------------------------------------------------------------

s_curve_indexing <- function(serie, alpha, beta, index_step = 1) {
  if (length(serie) == 0) return(numeric(0))
  
  if (length(serie) == 1) {
    beta_calc <- beta / 1e9
    return(beta_calc^(alpha^serie) - beta_calc)
  }
  
  i_max <- if (is.data.frame(serie)) nrow(serie) else length(serie)
  if (i_max < 1) return(numeric(0))
  
  index <- seq(0, (i_max - 1), by = index_step)
  beta_calc <- beta / 1e9
  beta_calc^(alpha^index) - beta_calc
}

first_derivative <- function(x, shape = "s-origin", alpha, beta, index_step = 1) {
  i_max <- length(x)
  if (i_max < 1) return(numeric(0))
  index <- seq(0, (i_max - 1), by = index_step)
  switch(
    tolower(shape),
    "s-origin" = (beta / 1e9)^(alpha^index) * alpha^index * log(beta / 1e9) * log(alpha),
    "s-shaped" = (beta / 1e10)^(alpha^index) * alpha^index * log(beta / 1e10) * log(alpha),
    "indexp"   = (alpha * exp(-(alpha * index) / 10)) / 10,
    rep(NA, i_max)
  )
}

second_derivative <- function(x, shape = "s-origin", alpha, beta, index_step = 1) {
  i_max <- length(x)
  if (i_max < 1) return(numeric(0))
  index <- seq(0, (i_max - 1), by = index_step)
  switch(
    tolower(shape),
    "s-origin" = {
      log(beta / 1e9) * log(alpha) * (
        ((beta / 1e9)^(alpha^index) * alpha^(2 * index) * log(beta / 1e9) * log(alpha)) +
        ((beta / 1e9)^(alpha^index) * alpha^index * log(alpha))
      )
    },
    "s-shaped" = {
      log(beta / 1e10) * log(alpha) * (
        ((beta / 1e10)^(alpha^index) * alpha^(2 * index) * log(beta / 1e10) * log(alpha)) +
        ((beta / 1e10)^(alpha^index) * alpha^index * log(alpha))
      )
    },
    "indexp"   = (-(alpha^2) * exp(-(alpha * index) / 10)) / 100,
    rep(NA, i_max)
  )
}

third_derivative <- function(x, shape = "s-origin", alpha, beta, index_step = 1) {
  i_max <- length(x)
  if (i_max < 1) return(numeric(0))
  index <- seq(0, (i_max - 1), by = index_step)
  switch(
    tolower(shape),
    "s-origin" = {
      log(beta / 1e9) * log(alpha) * (
        ((beta / 1e9)^(alpha^index) * alpha^(3 * index) * (log(beta / 1e9))^2 * (log(alpha))^2) +
        3 * ((beta / 1e9)^(alpha^index) * alpha^(2 * index) * log(beta / 1e9) * (log(alpha))^2) +
        ((beta / 1e9)^(alpha^index) * alpha^index * (log(alpha))^2)
      )
    },
    "s-shaped" = {
      log(beta / 1e10) * log(alpha) * (
        ((beta / 1e10)^(alpha^index) * alpha^(3 * index) * (log(beta / 1e10))^2 * (log(alpha))^2) +
        3 * ((beta / 1e10)^(alpha^index) * alpha^(2 * index) * log(beta / 1e10) * (log(alpha))^2) +
        ((beta / 1e10)^(alpha^index) * alpha^index * (log(alpha))^2)
      )
    },
    "indexp"   = ((alpha^3) * exp(-(alpha * index) / 10)) / 1000,
    rep(NA, i_max)
  )
}

# -------------------------------------------------------------------------
# get_point_on_curve:
#   Finds the exact X point on the S-curve that yields the closest Y value
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# find_x_for_y_on_curve:
#   Finds the exact X value on the curve that corresponds to a given Y value
# -------------------------------------------------------------------------
find_x_for_y_on_curve <- function(target_y, alpha, beta, max_value, precision = 0.0001) {
  # Enhanced validation
  if (!is.finite(target_y) || target_y <= 0) return(0)
  if (target_y >= 1) return(max_value)
  
  # Function to calculate Y given X
  calc_y <- function(x) {
    beta_calc <- beta / 1e9
    return(beta_calc^(alpha^x) - beta_calc)
  }
  
  # Binary search to find the X that produces the target Y
  lower_x <- 0
  upper_x <- max_value * 3  # Wide range to ensure finding the point
  
  iter <- 0
  max_iter <- 100  # Limit iterations to prevent infinite loops
  
  while ((upper_x - lower_x) > precision && iter < max_iter) {
    mid_x <- (lower_x + upper_x) / 2
    mid_y <- calc_y(mid_x)
    
    if (abs(mid_y - target_y) < precision) {
      return(mid_x)  # Found a sufficiently close point
    }
    
    if (mid_y < target_y) {
      lower_x <- mid_x
    } else {
      upper_x <- mid_x
    }
    
    iter <- iter + 1
  }
  
  return((lower_x + upper_x) / 2)
}

# -------------------------------------------------------------------------
# find_y_for_x_on_curve:
#   Calculates the exact Y value on the curve for a given X value
#   FIXED: Improved handling to ensure an exact point on the curve
# -------------------------------------------------------------------------
find_y_for_x_on_curve <- function(x_value, alpha, beta) {
  # Stricter validation to avoid issues with the 52w_Avg point
  if (is.na(x_value) || !is.finite(x_value)) {
    x_value <- 0
    return(0)
  }
  
  if (x_value < 0) {
    x_value <- 0
  }
  
  # For very large x_value, prevent numerical issues
  if (x_value > 1000) {
    return(0.98) # Approximation to full saturation
  }
  
  # Original S-curve formula
  beta_calc <- beta / 1e9
  y_val <- beta_calc^(alpha^x_value) - beta_calc
  
  # Avoid invalid results (NaN, Inf, etc.)
  if (!is.finite(y_val)) {
    if (x_value < 0.1) return(0)
    if (x_value > 100) return(0.98)
    return(0.5) # Midpoint fallback
  }
  
  return(y_val)
}


# -------------------------------------------------------------------------
# apply_transformation:
#   
#   
# -------------------------------------------------------------------------
apply_transformation <- function(data, type = "Linear", alpha = 1, beta = 1, maxval = 100, decay = 1, lag = 0) {
  if (is.null(data) || length(data) == 0) return(numeric(0))
  if (all(is.na(data))) return(data)
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
    "Linear"     = data,
    "S Origin"   = s_curve_transform(data, "s-origin", alpha, beta, maxval),
    "S Shaped"   = s_curve_transform(data, "s-shaped", alpha, beta, maxval),
    "Index Exp"  = 1 - exp(- (alpha / 10) * (data / max(data, na.rm = TRUE))),
    "Log"        = log1p(data),
    "Exp"        = 1 - exp(-(data / alpha)),
    "Power"      = data^alpha,
    "Moving Avg" = {
      if (length(data) < 3) rep(NA, length(data))
      else zoo::rollmean(data, k = 3, fill = NA, align = "right")
    },
    data
  )
  return(out)
}




# -------------------------------------------------------------------------
# calculate_scale_info:
#   Determines the scaling factor and suffix (K, M, B, T) for graph axes.
# -------------------------------------------------------------------------

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

# -------------------------------------------------------------------------
# handle_outliers:
#   
# -------------------------------------------------------------------------
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