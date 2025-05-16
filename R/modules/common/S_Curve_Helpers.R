# S_Curve_Helpers.R

# Módulo de Funciones Helper (Transformaciones, Derivadas, Escalado, Outliers)

# -------------------------------------------------------------------------
# s_curve_transform:
#   Aplica una transformación a los datos. Para curvas S ("s-origin", "s-shaped"),
#   la forma de la curva se genera basada en la longitud de la serie de entrada,
#   y luego se escala usando max(x) y maxValuePct.
#
# Parámetros para S-Curves:
#   alpha: Controla la curvatura. DEBE ESTAR ENTRE 0 y 1 (exclusivo) para
#          las curvas S de tipo K^(alpha^index) para asegurar un crecimiento hacia 1.
#          Si alpha >= 1, la curva tenderá a 0 o -K.
#   beta:  Usado para calcular la base K de la exponenciación (K = beta / divisor).
#          Típicamente beta = 1.
#   maxValuePct: Porcentaje del valor máximo de x que la curva S debe alcanzar.
# -------------------------------------------------------------------------
s_curve_transform <- function(x, shape = "s-origin", alpha = 0.85, beta = 1, maxValuePct = 100) {
  if (!is.numeric(x) || length(x) == 0) return(numeric(0))
  if (all(is.na(x))) return(x)

  max_x_val <- max(x, na.rm = TRUE)
  if (!is.finite(max_x_val)) { # Si max_x_val no es finito, no se puede escalar. Devolver x.
      warning("max(x) is not finite. Cannot apply S-curve transformation scaling. Returning original x.")
      return(x)
  }
   if (max_x_val == 0 && any(tolower(shape) %in% c("s-origin", "s-shaped", "indexp"))) {
    # Si max_x_val es 0, la escala de salida será 0.
    return(rep(0, length(x)))
  }


  i_max <- length(x)
  idx_sequence_from_0 <- seq(0, i_max - 1) # Index from 0 to N-1
  idx_sequence_from_1 <- seq_len(i_max)    # Index from 1 to N (original behavior for some)

  # Factor de escala para la salida final de las curvas S
  output_scaler <- max_x_val * (maxValuePct / 100)

  result <- switch(
    tolower(shape),
    "s-shaped" = {
      if (alpha <= 0 || alpha >= 1) {
        warning(paste("For 's-shaped', alpha should be > 0 and < 1 for growth. Current alpha:", alpha, ". Output may be near zero or decay."))
      }
      if (beta <= 0) warning(paste("For 's-shaped', beta should be > 0. Current beta:", beta))

      beta_calc <- beta / 1e10 # Base K
      
      # Curva base: K^(alpha^index), index desde 0. Rango: [K, 1] si 0<alpha<1
      raw_curve <- beta_calc^(alpha^idx_sequence_from_0)
      
      # Normalizar a [0, 1]
      # (value - min_val) / (max_val - min_val)
      # Min val teórica es beta_calc (cuando alpha^idx -> 1, i.e. idx=0), max val teórica es 1 (cuando alpha^idx -> 0)
      min_raw_val <- beta_calc 
      max_raw_val <- 1
      
      if (abs(max_raw_val - min_raw_val) < .Machine$double.eps^0.5) { # Evitar división por cero si el rango es mínimo
        normalized_curve <- ifelse(raw_curve >= max_raw_val - .Machine$double.eps^0.5, 1, 0)
      } else {
        normalized_curve <- (raw_curve - min_raw_val) / (max_raw_val - min_raw_val)
      }
      normalized_curve[is.nan(normalized_curve)] <- 0 # Sanity check for NaN from 0/0
      output_scaler * normalized_curve
    },
    "s-origin" = {
      if (alpha <= 0 || alpha >= 1) {
        warning(paste("For 's-origin', alpha should be > 0 and < 1 for growth. Current alpha:", alpha, ". Output may be near zero or decay."))
      }
      if (beta <= 0) warning(paste("For 's-origin', beta should be > 0. Current beta:", beta))
      
      beta_calc <- beta / 1e9 # Base K
      
      # Curva base: K^(alpha^index) - K, index desde 0. Rango: [0, 1-K] si 0<alpha<1
      raw_curve <- beta_calc^(alpha^idx_sequence_from_0) - beta_calc
      
      # Normalizar a [0, 1]
      # (value - min_val) / (max_val - min_val)
      # Min val teórica es 0, max val teórica es 1-K
      min_raw_val <- 0
      max_raw_val <- 1 - beta_calc
      
      if (abs(max_raw_val - min_raw_val) < .Machine$double.eps^0.5) {
        normalized_curve <- ifelse(raw_curve >= max_raw_val - .Machine$double.eps^0.5, 1, 0)
      } else {
        normalized_curve <- (raw_curve - min_raw_val) / (max_raw_val - min_raw_val)
      }
      normalized_curve[is.nan(normalized_curve)] <- 0
      output_scaler * normalized_curve
    },
    "indexp" = { # Esta curva ya usa los valores de x normalizados
      if (alpha <= 0) warning(paste("For 'indexp', alpha should be > 0. Current alpha:", alpha))
      # Normalizar x para que la forma de la curva sea consistente independientemente de la escala de x
      # x_norm va de 0 a 1 si x >= 0
      x_norm <- x / max_x_val 
      x_norm[is.nan(x_norm)] <- 0 # si max_x_val era 0
      x_norm[x < 0] <- 0 # Asegurar que los valores negativos no afecten negativamente exp.
      
      # La fórmula original usaba seq_len(i_max), lo cual es extraño si 'data' son valores.
      # Asumimos que se quiere transformar 'x_norm'
      # 1 - exp(-alpha_param * x_norm)
      # El alpha/10 original es un factor de escala para alpha.
      # Si alpha es la "sensibilidad", un alpha más grande hace que suba más rápido.
      # El "10" puede ser un parámetro configurable o absorbido en alpha.
      # Para mantener consistencia con la firma, usamos alpha como está.
      # Si 'data' en apply_transformation es 'x', entonces usamos x_norm.
      # La fórmula original era: 1 - exp(- (alpha / 10) * (data / max(data, na.rm = TRUE)))
      # que es 1 - exp(- (alpha / 10) * x_norm)
      # Esto produce valores en [0,1), que luego se escalan.
      raw_curve <- 1 - exp(-(alpha / 10) * x_norm)
      output_scaler * raw_curve # Escalar por el maximo original de x
    },
    x # Default case
  )
  return(result)
}

# -------------------------------------------------------------------------
# s_curve_indexing:
#   Devuelve el valor Y normalizado [0,1] de la curva S "s-origin" dado un X (índice).
#   serie => Puede ser un escalar (el índice x) o un vector (para determinar i_max).
#   alpha => Parámetro de forma (0 < alpha < 1 para crecimiento).
#   beta  => Parámetro base (usado como beta/1e9).
#   index_step => Paso del índice.
#   NOTA: Esta función devuelve el valor Y normalizado [0,1] de la forma canónica.
# -------------------------------------------------------------------------
s_curve_indexing <- function(serie, alpha, beta, index_step = 1) {
  if (length(serie) == 0) return(numeric(0))

  # Validaciones de alpha y beta
  if (alpha <= 0 || alpha >= 1) {
    warning(paste("s_curve_indexing: alpha should be > 0 and < 1 for growth. Current alpha:", alpha))
  }
  if (beta <= 0) warning(paste("s_curve_indexing: beta should be > 0. Current beta:", beta))

  beta_calc <- beta / 1e9 # Base K

  if (length(serie) == 1 && inherits(serie, "numeric")) { # Un solo valor de índice
    index_val <- serie
    if (index_val < 0) index_val <- 0 # Índice no puede ser negativo
    
    raw_val <- beta_calc^(alpha^index_val) - beta_calc
    
    # Normalizar a [0, 1]
    min_raw_val <- 0
    max_raw_val <- 1 - beta_calc
    if (abs(max_raw_val - min_raw_val) < .Machine$double.eps^0.5) {
      return(ifelse(raw_val >= max_raw_val - .Machine$double.eps^0.5, 1, 0))
    } else {
      return((raw_val - min_raw_val) / (max_raw_val - min_raw_val))
    }
  }

  i_max <- if (is.data.frame(serie)) nrow(serie) else length(serie)
  if (i_max < 1) return(numeric(0))

  index_seq <- seq(0, (i_max - 1) * index_step, by = index_step) # Asegurar que el índice máximo sea considerado

  raw_curve <- beta_calc^(alpha^index_seq) - beta_calc

  min_raw_val <- 0
  max_raw_val <- 1 - beta_calc
  if (abs(max_raw_val - min_raw_val) < .Machine$double.eps^0.5) {
    normalized_curve <- ifelse(raw_curve >= max_raw_val - .Machine$double.eps^0.5, 1, 0)
  } else {
    normalized_curve <- (raw_curve - min_raw_val) / (max_raw_val - min_raw_val)
  }
  return(normalized_curve)
}

# -------------------------------------------------------------------------
# Derivadas:
#   Calculan las derivadas de las formas canónicas de las curvas S.
#   Las derivadas son de las funciones K^(alpha^index) o similares, *antes* de la normalización final y escalado.
#   Esto es importante porque el escalado lineal no afecta la forma relativa de las derivadas (solo su magnitud).
#   'x' aquí es un placeholder para determinar la longitud de la secuencia de índices.
# -------------------------------------------------------------------------

first_derivative <- function(x, shape = "s-origin", alpha, beta, index_step = 1) {
  i_max <- length(x)
  if (i_max < 1) return(numeric(0))
  idx <- seq(0, (i_max - 1) * index_step, by = index_step)

  # Validaciones de alpha y beta
  if (alpha <= 0) warning(paste("Derivative: alpha should be > 0. Current alpha:", alpha))
  # Para K^(A^i), A<1 es crecimiento, A>1 es decaimiento. Ambas son válidas para derivar.
  if (beta <= 0) warning(paste("Derivative: beta should be > 0. Current beta:", beta))

  # La derivada de f(idx) = K^(alpha^idx) es f'(idx) = K^(alpha^idx) * alpha^idx * log(K) * log(alpha)
  # La derivada de f(idx) = K^(alpha^idx) - K es la misma, ya que K es una constante.
  
  beta_calc <- switch(
    tolower(shape),
    "s-origin" = beta / 1e9,
    "s-shaped" = beta / 1e10,
    1 # Default, no se usará para indexp
  )

  switch(
    tolower(shape),
    "s-origin" = , # Ambas usan la misma base para la derivada de la parte exponencial
    "s-shaped" = {
      if (beta_calc <= 0 || beta_calc >=1) { # log(beta_calc) requiere beta_calc > 0; beta_calc >=1 cambia el signo de log(beta_calc)
          #warning(paste("Derivative: beta_calc (beta/divisor) must be >0 and <1 for log(beta_calc) to be negative as expected. Current beta_calc:", beta_calc))
          # Si beta_calc es 0 o negativo, log da NaN o error. Si es 1, log es 0.
          # Si es >1, log es positivo.
          # Devolver NA o ceros si los parámetros no son válidos para la fórmula.
          if(beta_calc <= 0 || alpha <=0) return(rep(NA_real_, i_max))
          if(beta_calc == 1) return(rep(0, i_max)) # Si K=1, K^f(x) = 1, derivada es 0.
      }
      term_exp <- beta_calc^(alpha^idx)
      term_exp * alpha^idx * log(beta_calc) * log(alpha)
    },
    "indexp"   = { # Derivada de 1 - exp(-k * idx), donde k = alpha/10. Es k * exp(-k * idx)
      # La fórmula original en s_curve_transform usa x_norm, no un índice directo.
      # Aquí, para ser consistentes con el uso de 'idx', asumimos que 'idx' es el input normalizado.
      # O, si es un índice, la derivada es con respecto a ese índice.
      # Derivada de 1 - exp(-(alpha_param * idx_sequence)/C)
      # d/di (1 - exp(-a*i/C)) = -exp(-a*i/C) * (-a/C) = (a/C)exp(-a*i/C)
      # Aquí, C=10.
      k <- alpha / 10
      k * exp(-k * idx)
    },
    rep(NA_real_, i_max) # Default
  )
}

# NOTA: Las segundas y terceras derivadas pueden volverse complejas y numéricamente inestables.
# Se basan en la misma lógica de K^(alpha^idx). Se simplifican aquí para mayor claridad.
# La fórmula completa de la segunda derivada de f(i) = K^(A^i) es:
# f''(i) = K^(A^i) * (A^i * log(K) * log(A))^2  +  K^(A^i) * (A^i * (log(A))^2 * log(K) + A^i * log(K) * log(A))
# f''(i) = f(i) * [ (A^i * log(K) * log(A))^2 + (log(A))^2 * A^i * log(K) + log(K) * A^i * log(A) ]
# Esto es f(i) * A^i * log(K) * log(A) * [ A^i * log(K) * log(A) + log(A) + 1 ] ... no, es más simple:
# f'(i) = f(i) * A^i * log(K) * log(A).
# Sea g(i) = A^i * log(K) * log(A). Entonces f'(i) = f(i) * g(i).
# f''(i) = f'(i)g(i) + f(i)g'(i)
# g'(i) = A^i * (log(A))^2 * log(K) * log(A)  <- No, g'(i) = A^i * log(A) * (log(K) * log(A)) = A^i * (log(A))^2 * log(K)
# g'(i) = (A^i * log A) * logK * logA = A^i * (logA)^2 * logK
# f''(i) = (f(i) * g(i)) * g(i) + f(i) * (A^i * (log(A))^2 * log(K))
# f''(i) = f(i) * [ (g(i))^2 + A^i * (log(A))^2 * log(K) ]
# f''(i) = K^(A^i) * [ (A^i * log(K) * log(A))^2 + A^i * (log(A))^2 * log(K) ]

second_derivative <- function(x, shape = "s-origin", alpha, beta, index_step = 1) {
  i_max <- length(x)
  if (i_max < 1) return(numeric(0))
  idx <- seq(0, (i_max - 1) * index_step, by = index_step)

  if (alpha <= 0) warning(paste("Derivative: alpha should be > 0. Current alpha:", alpha))
  if (beta <= 0) warning(paste("Derivative: beta should be > 0. Current beta:", beta))

  beta_calc <- switch(
    tolower(shape),
    "s-origin" = beta / 1e9,
    "s-shaped" = beta / 1e10,
    1 # Default
  )

  switch(
    tolower(shape),
    "s-origin" = ,
    "s-shaped" = {
      if (beta_calc <= 0 || alpha <=0) return(rep(NA_real_, i_max))
      if (beta_calc == 1) return(rep(0, i_max))

      # f(i) = K^(A^i)
      # f''(i) = K^(A^i) * A^i * log(K) * (log(A))^2 * (A^i * log(K) + 1) (Simplificado de la original, puede ser diferente)
      # Usando la derivación f''(i) = f(i) * [ (A^i * log(K) * log(A))^2 + A^i * (log(A))^2 * log(K) ]
      term_exp_K_Ai = beta_calc^(alpha^idx)
      Ai = alpha^idx
      logK = log(beta_calc)
      logA = log(alpha)

      term1 = (Ai * logK * logA)^2
      term2 = Ai * (logA)^2 * logK
      
      term_exp_K_Ai * (term1 + term2)
    },
    "indexp"   = { # Derivada de k * exp(-k * idx) es -k^2 * exp(-k * idx)
      k <- alpha / 10
      -k^2 * exp(-k * idx)
    },
    rep(NA_real_, i_max)
  )
}

# La tercera derivada es aún más compleja. Se omite la fórmula explícita por brevedad,
# pero seguiría el mismo patrón de derivación.
# Para "indexp": d/di (-k^2 * exp(-k*i)) = k^3 * exp(-k*i)
third_derivative <- function(x, shape = "s-origin", alpha, beta, index_step = 1) {
  i_max <- length(x)
  if (i_max < 1) return(numeric(0))
  idx <- seq(0, (i_max - 1) * index_step, by = index_step)
  
  # ... (validaciones similares para alpha, beta) ...

  beta_calc <- switch(
    tolower(shape),
    "s-origin" = beta / 1e9,
    "s-shaped" = beta / 1e10,
    1 # Default
  )

  switch(
    tolower(shape),
    "s-origin" = ,
    "s-shaped" = {
      # La fórmula analítica es muy larga. Para fines prácticos, podría calcularse numéricamente si es necesario.
      # O si se usa mucho, derivarla cuidadosamente y simplificarla.
      # Por ahora, devolvemos NA para indicar que no está implementada de forma robusta aquí.
      warning("Third derivative for 's-origin'/'s-shaped' is complex and not fully implemented here. Returning NA.")
      rep(NA_real_, i_max)
    },
    "indexp"   = {
      k <- alpha / 10
      k^3 * exp(-k * idx)
    },
    rep(NA_real_, i_max)
  )
}


# -------------------------------------------------------------------------
# find_x_for_y_on_curve:
#   Encuentra el valor X (índice) en la curva S "s-origin" (normalizada a [0,1])
#   que corresponde a un valor Y (normalizado, 0 <= target_y <= 1).
#   max_index_range: El rango máximo esperado para el índice X a buscar.
# -------------------------------------------------------------------------
find_x_for_y_on_curve <- function(target_y, alpha, beta, max_index_range = 1000, precision = 0.0001) {
  # target_y es el valor Y normalizado (0 a 1)
  if (!is.finite(target_y)) return(NA_real_)
  if (target_y < 0) target_y <- 0
  if (target_y > 1) target_y <- 1

  if (alpha <= 0 || alpha >= 1) {
    warning("find_x_for_y_on_curve: alpha should be > 0 and < 1. Result may be unreliable.")
    # Podría retornar NA o intentar proceder con cautela.
  }
  if (beta <= 0) {
     warning("find_x_for_y_on_curve: beta should be > 0. Result may be unreliable.")
     return(NA_real_)
  }

  beta_calc <- beta / 1e9 # Para s-origin

  # Función para calcular Y normalizado dado un índice X
  # Y_norm = (K^(A^X) - K) / (1 - K)
  calc_y_norm <- function(idx_val) {
    if (idx_val < 0) idx_val <- 0 # Índice no negativo
    raw_val <- beta_calc^(alpha^idx_val) - beta_calc
    min_raw_val <- 0
    max_raw_val <- 1 - beta_calc
    if (abs(max_raw_val - min_raw_val) < .Machine$double.eps^0.5) {
      return(ifelse(raw_val >= max_raw_val - .Machine$double.eps^0.5, 1, 0))
    } else {
      return((raw_val - min_raw_val) / (max_raw_val - min_raw_val))
    }
  }
  
  # Intento de solución analítica (puede ser inestable o fallar si los logs no están definidos)
  # target_y = (beta_calc^(alpha^x) - beta_calc) / (1 - beta_calc)
  # target_y * (1 - beta_calc) = beta_calc^(alpha^x) - beta_calc
  # target_y * (1 - beta_calc) + beta_calc = beta_calc^(alpha^x)
  # Let Y_adjusted = target_y * (1 - beta_calc) + beta_calc
  # log_beta_calc(Y_adjusted) = alpha^x
  # log_alpha(log_beta_calc(Y_adjusted)) = x
  
  y_adjusted <- target_y * (1 - beta_calc) + beta_calc
  
  if (y_adjusted <= 0 || y_adjusted > 1) { # y_adjusted debe estar en (0,1] para log_beta_calc si beta_calc < 1
      # Si target_y es 0, y_adjusted es beta_calc. log_beta_calc(beta_calc) = 1. log_alpha(1) = 0. x=0.
      if (abs(target_y - 0) < precision) return(0)
      # Si target_y es 1, y_adjusted es 1. log_beta_calc(1) = 0. log_alpha(0) -> -Inf. x -> Inf.
      if (abs(target_y - 1) < precision && (alpha > 0 && alpha < 1)) return(max_index_range) # Aproximación para el máximo
      # En otros casos, la solución analítica puede no ser válida, recurrir a búsqueda binaria.
  } else if (abs(beta_calc - 1) < precision || alpha <=0 || alpha >=1 ) {
      # Evitar log(0) o log de base inválida
  } else {
      val_log_beta <- log(y_adjusted) / log(beta_calc)
      if (val_log_beta > 0 && val_log_beta < Inf && alpha > 0 && alpha < 1) { # alpha^x debe ser > 0
          # log_alpha(val_log_beta)
          # alpha^x = val_log_beta. x log(alpha) = log(val_log_beta)
          # x = log(val_log_beta) / log(alpha)
          analytical_x <- log(val_log_beta) / log(alpha)
          if (is.finite(analytical_x) && analytical_x >= 0) {
              # Verificar si este x produce el target_y
              if (abs(calc_y_norm(analytical_x) - target_y) < precision * 10) { # Un poco más de tolerancia
                  return(analytical_x)
              }
          }
      }
  }


  # Búsqueda binaria como fallback o principal método
  lower_x <- 0
  upper_x <- max_index_range # Rango de búsqueda para el índice X
  iter <- 0
  max_iter <- 200 # Aumentado por si el rango es grande o la convergencia lenta

  # Casos extremos
  if (abs(target_y - calc_y_norm(lower_x)) < precision) return(lower_x)
  if (abs(target_y - calc_y_norm(upper_x)) < precision) return(upper_x)


  while ((upper_x - lower_x) > precision && iter < max_iter) {
    mid_x <- (lower_x + upper_x) / 2
    mid_y <- calc_y_norm(mid_x)

    if (abs(mid_y - target_y) < precision) {
      return(mid_x)
    }

    # Asumiendo que calc_y_norm es monotónicamente creciente con x
    if (mid_y < target_y) {
      lower_x <- mid_x
    } else {
      upper_x <- mid_x
    }
    iter <- iter + 1
  }
  
  # Devolver el mejor estimado si no se alcanza la precisión exacta
  final_x <- (lower_x + upper_x) / 2
  # Verificar si el final_x es razonable
  if (abs(calc_y_norm(final_x) - target_y) > precision * 100) { # Si está muy lejos, algo falló
      #warning("find_x_for_y_on_curve: Binary search did not converge well.")
      # Podría retornar NA o el valor menos malo.
      # Esto puede pasar si target_y está fuera del rango [0,1] o alpha no es (0,1)
      if (target_y <= calc_y_norm(0) + precision) return(0) # Si target es muy bajo, devolver 0
      if (target_y >= calc_y_norm(max_index_range) - precision) return(max_index_range) # Si target es muy alto
  }
  return(final_x)
}

# -------------------------------------------------------------------------
# find_y_for_x_on_curve:
#   Calcula el valor Y normalizado [0,1] en la curva S "s-origin" para un X (índice) dado.
# -------------------------------------------------------------------------
find_y_for_x_on_curve <- function(x_value, alpha, beta) {
  if (is.na(x_value) || !is.finite(x_value) || x_value < 0) {
    x_value <- 0 # Tratar inválidos/negativos como 0
  }
  
  # No hay un límite superior estricto para x_value en la fórmula,
  # pero valores muy grandes harán que alpha^x_value sea 0 si alpha < 1.
  # La función s_curve_indexing ya maneja esto y normaliza.
  
  # Usar s_curve_indexing para consistencia, ya que devuelve el valor normalizado.
  # s_curve_indexing espera 'serie' como primer argumento.
  return(s_curve_indexing(serie = x_value, alpha = alpha, beta = beta))
}


# -------------------------------------------------------------------------
# apply_transformation:
#   Aplica varias transformaciones a los datos.
#   Se han ajustado las llamadas a s_curve_transform.
# -------------------------------------------------------------------------
apply_transformation <- function(data, type = "Linear", alpha = 1, beta = 1, maxval = 100, decay = 1, lag = 0) {
  if (is.null(data) || length(data) == 0) return(numeric(0))
  if (all(is.na(data))) return(data)
  
  # Asegurar que los datos no sean negativos antes de ciertas transformaciones (log, power, s-curve input for max_val)
  # Para S-curves, max_val se usa para escalar, por lo que data puede tener negativos si se maneja.
  # Pero si max(data) es negativo, output_scaler será negativo.
  # Es más seguro trabajar con data >= 0 para S-curves y otras no lineales.
  # data[data < 0] <- 0 # Comentado, ya que el escalador usará max(data) tal cual.
                        # Si max(data) es negativo, el resultado será negativo, lo cual puede ser o no deseado.
                        # Si se requiere data >= 0, debe hacerse antes de llamar a esta función o aquí explícitamente.

  if (lag > 0) {
    if (lag >= length(data)) {
      data <- rep(NA_real_, length(data)) # Usar NA_real_ para consistencia de tipo
    } else {
      # head(data, -lag) es correcto para quitar del final.
      # c(rep(NA,...), data_sin_lag)
      data <- c(rep(NA_real_, lag), utils::head(data, -lag))
    }
  }
  
  # Aplicar decaimiento si es necesario.
  # data * decay^seq_along(data) o similar si es un decaimiento secuencial.
  # El código original es data * decay, que es un simple factor de escala.
  data <- data * decay # Asumiendo que 'decay' es un factor simple.

  out <- switch(
    type,
    "Linear"     = data,
    "S Origin"   = s_curve_transform(data, "s-origin", alpha, beta, maxval), # maxval es maxValuePct
    "S Shaped"   = s_curve_transform(data, "s-shaped", alpha, beta, maxval), # maxval es maxValuePct
    "Index Exp"  = {
        # Esta transformación usa alpha y los datos normalizados.
        # s_curve_transform con "indexp" ya maneja la normalización de 'data' y escalado.
        s_curve_transform(data, "indexp", alpha, beta, maxval) # beta no se usa en indexp de s_curve_transform
    },
    "Log"        = {
        data_log <- ifelse(data < 0, NA_real_, data) # Log solo para no negativos
        log1p(data_log) # log1p(x) = log(1+x), maneja mejor x pequeños.
    },
    "Exp"        = { # 1 - exp(-(data / alpha_scale_factor))
        # alpha aquí actúa como un factor de escala para 'data'.
        # Si alpha es muy pequeño, data/alpha es grande, exp tiende a 0, resultado a 1.
        # Si alpha es muy grande, data/alpha es pequeño, exp tiende a 1, resultado a 0.
        # Asegurar alpha > 0
        alpha_scale <- if(alpha <=0) {warning("Exp transform: alpha should be > 0. Using 1."); 1} else alpha
        1 - exp(-(data / alpha_scale))
    },
    "Power"      = {
        data_power <- ifelse(data < 0 & (alpha %% 1 != 0), NA_real_, data) # Raíces de negativos son complejas
        data_power^alpha
    },
    "Moving Avg" = {
      if (length(data) < 3) rep(NA_real_, length(data))
      else {
        # zoo::rollmean requiere zoo. Verificar si está cargado o usar stats::filter
        if (requireNamespace("zoo", quietly = TRUE)) {
            zoo::rollmean(data, k = 3, fill = NA, align = "right")
        } else {
            warning("zoo package not available for Moving Avg. Returning data.")
            data # O implementar una media móvil simple con stats::filter
        }
      }
    },
    data # Default: devolver los datos sin cambios (o con lag/decay si se aplicó)
  )
  return(out)
}


# -------------------------------------------------------------------------
# calculate_scale_info:
#   Determina el factor de escala y sufijo (K, M, B, T) para ejes de gráficos.
# -------------------------------------------------------------------------
calculate_scale_info <- function(mean_value) {
  if (is.na(mean_value) || !is.finite(mean_value)) return(list(number = 1, suffix = ""))
  abs_mean_value <- abs(mean_value) # Usar valor absoluto para la escala

  if (abs_mean_value < 1000) {
    list(number = 1, suffix = "")
  } else if (abs_mean_value < 1e6) {
    list(number = 1e3, suffix = "K")
  } else if (abs_mean_value < 1e9) {
    list(number = 1e6, suffix = "M")
  } else if (abs_mean_value < 1e12) {
    list(number = 1e9, suffix = "B")
  } else {
    list(number = 1e12, suffix = "T")
  }
}

# -------------------------------------------------------------------------
# handle_outliers:
#   Identifica y reemplaza outliers con NA usando el método IQR.
# -------------------------------------------------------------------------
handle_outliers <- function(data, method = "iqr", threshold = 1.5) {
  if (!is.numeric(data) || length(data) < 4 || all(is.na(data))) return(data)

  # Calcular cuantiles solo sobre datos finitos
  finite_data <- data[is.finite(data)]
  if (length(finite_data) < 4) return(data) # No suficientes puntos finitos para IQR robusto

  q1 <- quantile(finite_data, 0.25, na.rm = TRUE)
  q3 <- quantile(finite_data, 0.75, na.rm = TRUE)
  iqr_val <- q3 - q1

  # Si IQR es cero (todos los valores en Q1-Q3 son iguales), no hay outliers por este método,
  # a menos que haya valores fuera de este rango constante.
  if (iqr_val == 0) {
    # Si todos los datos finitos son iguales, no hay outliers.
    # Si hay algunos diferentes, q1 y q3 podrían ser iguales si hay muchos valores repetidos.
    # En este caso, cualquier cosa diferente de q1 (o q3) es un outlier.
    # Pero la fórmula estándar de lower/upper bound se vuelve q1 y q3.
    # No se cambian datos si IQR es 0, a menos que se quiera un manejo especial.
    return(data) 
  }

  lower_bound <- q1 - threshold * iqr_val
  upper_bound <- q3 + threshold * iqr_val

  # Reemplazar outliers con NA. Asegurarse de no afectar los NA existentes de forma no intencionada.
  data[data < lower_bound | data > upper_bound] <- NA_real_ # Usar NA_real_
  return(data)
}
