# =============================================================================
# Boxplot para la pestaña Univariada.
# =============================================================================


render_boxplot_univariate <- function(df, variable_univ, geography_univ) {
  # --- Dependencias ---
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("El paquete 'ggplot2' es necesario para esta visualización de boxplot.")
  }
  has_scales <- requireNamespace("scales", quietly = TRUE) # Para formato de números

  # --- Validación Inicial de Entradas ---
  shiny::req(df, variable_univ, geography_univ)
  shiny::validate(
    shiny::need(variable_univ != "N/A" && !is.null(variable_univ) && variable_univ != "",
                "Por favor, seleccione una variable válida para el boxplot.")
  )

  # --- Validación de Existencia de Variable y Conversión ---
  if (!variable_univ %in% names(df)) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = paste("Error: Variable no encontrada"),
                      subtitle = variable_univ) +
        ggplot2::theme_void() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("Variable '", variable_univ, "' no encontrada en los datos."), size = 5)
    )
  }

  # Extraer y convertir datos a numérico
  box_data_raw <- tryCatch({
    as.numeric(df[[variable_univ]])
  }, error = function(e) {
    warning(paste("Error convirtiendo la variable '", variable_univ, "' a numérica:", e$message))
    rep(NA_real_, nrow(df)) # Devolver NAs si la conversión falla
  })

  # Verificar si hay datos válidos después de la conversión
  if (length(box_data_raw) == 0 || all(is.na(box_data_raw))) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = paste("Boxplot para", variable_univ),
                      subtitle = "No hay datos válidos disponibles.") +
        ggplot2::theme_void() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No hay datos numéricos válidos o todos son NA.", size = 5)
    )
  }

  # --- Filtrado de NA y Manejo de Ceros ---
  valid_data <- box_data_raw[!is.na(box_data_raw)]
  zero_note <- NULL # Inicializar nota sobre ceros

  if (length(valid_data) > 0) {
    zero_count <- sum(valid_data == 0, na.rm = TRUE) # na.rm TRUE por si acaso, aunque ya filtramos NAs.
    total_valid_count <- length(valid_data)
    
    # Lógica de exclusión de ceros:
    # Si los ceros son una minoría (ej. < 25% de los datos válidos no-NA),
    # excluirlos puede ayudar a visualizar mejor la distribución del resto de los datos.
    # Si los ceros son una proporción significativa, deben ser parte de la visualización.
    if (zero_count > 0 && (zero_count / total_valid_count) < 0.25) {
      valid_data <- valid_data[valid_data != 0]
      percentage_zeros <- round(100 * zero_count / total_valid_count, 1)
      zero_note <- paste0(
        "Nota: Se excluyeron ", zero_count, " valores cero (",
        percentage_zeros, "% de los datos válidos no-NA) para una mejor visualización de la distribución."
      )
    }
  }

  # Verificar si quedan datos después del filtrado de ceros
  if (length(valid_data) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = paste("Boxplot para", variable_univ),
                      subtitle = "No hay datos no-cero o no-NA disponibles.") +
        ggplot2::theme_void() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "Todos los datos válidos eran ceros (y fueron excluidos) o no quedaron datos.", size = 5)
    )
  }

  # --- Cálculo de Estadísticas para Anotaciones ---
  data_stats <- list(
    min = min(valid_data, na.rm = TRUE),
    q1 = stats::quantile(valid_data, 0.25, na.rm = TRUE, type = 7), # type 7 es el default de R
    median = stats::median(valid_data, na.rm = TRUE),
    mean = mean(valid_data, na.rm = TRUE),
    q3 = stats::quantile(valid_data, 0.75, na.rm = TRUE, type = 7),
    max = max(valid_data, na.rm = TRUE),
    iqr = NA # Se calculará después
  )
  data_stats$iqr <- data_stats$q3 - data_stats$q1

  # Data frame para ggplot
  plot_df <- data.frame(value = valid_data, category = factor("Distribución"))

  # --- Decisión de Escala Logarítmica ---
  # Usar escala log si el rango es amplio y todos los datos son positivos.
  use_log_scale <- FALSE
  if (data_stats$min > 0 && data_stats$max / data_stats$min > 100 && is.finite(data_stats$max) && is.finite(data_stats$min)) {
    use_log_scale <- TRUE
  }

  # --- Construcción del Gráfico ggplot2 ---
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = category, y = value))

  # Añadir puntos jitter sólo para outliers (para no sobrecargar si hay muchos puntos)
  # Definición de outliers: debajo de Q1 - 1.5*IQR o encima de Q3 + 1.5*IQR
  lower_bound <- data_stats$q1 - 1.5 * data_stats$iqr
  upper_bound <- data_stats$q3 + 1.5 * data_stats$iqr
  
  outliers_df <- subset(plot_df, value < lower_bound | value > upper_bound)
  if (nrow(outliers_df) > 0) {
      p <- p + ggplot2::geom_jitter(
        data = outliers_df,
        width = 0.15, # Menos dispersión horizontal para puntos
        alpha = 0.6,
        color = "#2F4F4F", # DarkSlateGray
        size = 2
      )
  }

  # Boxplot principal
  p <- p + ggplot2::geom_boxplot(
    fill = "#ADD8E6", # LightBlue
    color = "#4682B4", # SteelBlue
    alpha = 0.8,
    width = 0.4, # Ancho del boxplot
    outlier.shape = NA # Ocultar outliers por defecto de geom_boxplot, ya los manejamos con geom_jitter
  )

  # Punto para la media
  if(is.finite(data_stats$mean)) {
    p <- p + ggplot2::geom_point(
      data = data.frame(category = factor("Distribución"), value = data_stats$mean), # Necesita df
      ggplot2::aes(y = value),
      shape = 23, # Diamante
      fill = "#FF8C00", # DarkOrange
      color = "#A0522D", # Sienna
      size = 3.5,
      show.legend = FALSE
    )
  }

  # Anotaciones para media y mediana (ajustar posición para que no se solapen)
  annotation_y_offset_factor <- if(use_log_scale) 1.3 else 0.05 * (data_stats$max - data_stats$min)
  if (!is.finite(annotation_y_offset_factor) || annotation_y_offset_factor == 0) annotation_y_offset_factor = abs(data_stats$median * 0.1) + 0.1

  if (is.finite(data_stats$mean)) {
      p <- p + ggplot2::annotate(
        "text", x = 1.25, y = data_stats$mean, # Ajustar x para posición horizontal
        label = paste("Media:", if(has_scales) scales::label_comma(accuracy=0.01)(data_stats$mean) else round(data_stats$mean, 2)),
        hjust = 0, color = "#A0522D", size = 3.2
      )
  }
  if (is.finite(data_stats$median)) {
      median_y_pos <- data_stats$median
      # Si la media y la mediana están muy juntas, desplazar la etiqueta de la mediana
      if (is.finite(data_stats$mean) && abs(data_stats$mean - data_stats$median) < annotation_y_offset_factor) {
          median_y_pos <- data_stats$median - annotation_y_offset_factor # Mover hacia abajo
          # Asegurar que no se mueva demasiado si la escala es log y el valor es pequeño
          if (use_log_scale && median_y_pos <= 0) median_y_pos = data_stats$median * 0.8
      }
      p <- p + ggplot2::annotate(
        "text", x = 1.25, y = median_y_pos,
        label = paste("Mediana:", if(has_scales) scales::label_comma(accuracy=0.01)(data_stats$median) else round(data_stats$median, 2)),
        hjust = 0, color = "#00008B", size = 3.2 # DarkBlue
      )
  }
  
  # --- Aplicar Escalas y Etiquetas ---
  # Formateador de etiquetas
  label_formatter <- if(has_scales) scales::label_number(accuracy=0.01, scale_cut = scales::cut_short_scale()) else ggplot2::waiver()

  if (use_log_scale) {
    p <- p + ggplot2::scale_y_log10(labels = label_formatter)
    y_axis_label <- paste("Valor de", variable_univ, "(Escala Log10)")
  } else {
    p <- p + ggplot2::scale_y_continuous(labels = label_formatter)
    y_axis_label <- paste("Valor de", variable_univ)
  }

  p <- p + ggplot2::labs(
    title = paste("Distribución de", variable_univ),
    subtitle = paste("Geografía:", geography_univ),
    caption = zero_note,
    x = NULL, # Sin etiqueta para el eje x (categoría)
    y = y_axis_label
  )

  # --- Tema y Estilo ---
  p <- p + ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 10)),
      plot.subtitle = ggplot2::element_text(size = 13, color = "#4A4A4A", hjust = 0.5, margin = ggplot2::margin(b = 15)),
      plot.caption = ggplot2::element_text(size = 10, color = "#707070", hjust = 0, margin = ggplot2::margin(t = 15)),
      axis.title.y = ggplot2::element_text(size = 12, face = "bold", color = "#303030", margin = ggplot2::margin(r=10)),
      axis.text.y = ggplot2::element_text(size = 10, color = "#505050"),
      axis.text.x = ggplot2::element_blank(), # Ocultar texto del eje x ("Distribución")
      axis.ticks.x = ggplot2::element_blank(), # Ocultar marcas del eje x
      panel.grid.major.y = ggplot2::element_line(color = "#E0E0E0", linetype = "dashed"),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(), # Sin rejilla vertical
      panel.background = ggplot2::element_rect(fill = "#F8F8F8", colour = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(20, 20, 20, 20) # t, r, b, l
    ) +
    ggplot2::coord_flip() # Boxplot horizontal

  return(p)
}