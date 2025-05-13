# Download_Transformed_Handler.R

# =============================================================================
# Manejadores de descarga para datos transformados en módulos univariado y multivariado
# =============================================================================

# Manejador para descargar datos transformados en módulo univariado
univariate_download_handler <- function(input, output, session, rv) {
  # Configura el manejador de descarga para datos transformados univariados
  #
  # Args:
  #   input: Objeto input de Shiny
  #   output: Objeto output de Shiny
  #   session: Objeto session de Shiny
  #   rv: Valores reactivos compartidos
  
  output$download_univariate <- downloadHandler(
    filename = function() {
      # Generar nombre de archivo con variable, transformación y fecha
      variable_name <- ifelse(is.null(input$variable_univ) || input$variable_univ == "N/A", 
                             "variable", 
                             gsub("[^a-zA-Z0-9]", "_", input$variable_univ))
      trans_type <- gsub(" ", "_", input$transformation_univ)
      paste0("univariate_", variable_name, "_", trans_type, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      tryCatch({
        withProgress(message = 'Preparing download...', value = 0.1, {
          # Obtener datos filtrados
          data <- rv$filtered_data
          if (is.null(data) || nrow(data) == 0) {
            stop("No hay datos disponibles para descargar.")
          }
          
          # Convertir la columna de fecha antes de filtrar
          if ("Period" %in% names(data)) {
            data$Period <- as.Date(data$Period)
          } else if ("periodo" %in% names(data)) {
            data$periodo <- as.Date(data$periodo)
          }
          
          # Aplicar filtro de fecha si está disponible
          if (!is.null(input$date_range_univ)) {
            date_col <- if ("Period" %in% names(data)) "Period" else if ("periodo" %in% names(data)) "periodo" else NULL
            if (!is.null(date_col)) {
              data <- data[data[[date_col]] >= input$date_range_univ[1] & 
                          data[[date_col]] <= input$date_range_univ[2], ]
            }
          }
          
          # Filtrar por geografía si es necesario
          if (!is.null(input$geography_univ) && input$geography_univ != "N/A") {
            geo_col <- if ("Geography" %in% names(data)) "Geography" else if ("Geografia" %in% names(data)) "Geografia" else NULL
            if (!is.null(geo_col) && input$geography_univ != "Total") {
              data <- data %>% filter(.data[[geo_col]] == input$geography_univ)
            } else if (input$geography_univ == "Total") {
              # Manejar caso "Total" - agrupar y sumar datos
              date_col <- if ("Period" %in% names(data)) "Period" else if ("periodo" %in% names(data)) "periodo" else NULL
              if (!is.null(date_col)) {
                numeric_cols <- names(data)[sapply(data, is.numeric)]
                data <- data %>%
                  group_by(across(all_of(date_col))) %>%
                  summarise(across(all_of(numeric_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
              }
            }
          }
          
          # Aplicar filtros adicionales si están disponibles (producto, campaña, etc.)
          if (!is.null(input$product_univ) && input$product_univ != "N/A") {
            prod_col <- if ("Product" %in% names(data)) "Product" else if ("Producto" %in% names(data)) "Producto" else NULL
            if (!is.null(prod_col)) {
              data <- data %>% filter(.data[[prod_col]] == input$product_univ)
            }
          }
          
          if (!is.null(input$campaign_univ) && input$campaign_univ != "N/A") {
            camp_col <- if ("Campaign" %in% names(data)) "Campaign" else if ("Campaña" %in% names(data)) "Campaña" else NULL
            if (!is.null(camp_col)) {
              data <- data %>% filter(.data[[camp_col]] == input$campaign_univ)
            }
          }
          
          if (!is.null(input$outlet_univ) && input$outlet_univ != "N/A") {
            outlet_col <- if ("Outlet" %in% names(data)) "Outlet" else NULL
            if (!is.null(outlet_col)) {
              data <- data %>% filter(.data[[outlet_col]] == input$outlet_univ)
            }
          }
          
          if (!is.null(input$creative_univ) && input$creative_univ != "N/A") {
            creative_col <- if ("Creative" %in% names(data)) "Creative" else if ("Creativo" %in% names(data)) "Creativo" else NULL
            if (!is.null(creative_col)) {
              data <- data %>% filter(.data[[creative_col]] == input$creative_univ)
            }
          }
          
          incProgress(0.3)
          
          # Verificar que existe la variable seleccionada
          if (is.null(input$variable_univ) || !(input$variable_univ %in% names(data))) {
            stop("La variable seleccionada no está disponible en los datos.")
          }
          
          # Aplicar transformación
          var_data <- data[[input$variable_univ]]
          transformed_data <- apply_transformation(
            var_data,
            type    = input$transformation_univ,
            alpha   = input$alpha_univ,
            beta    = input$beta_univ,
            maxval  = input$maxval_univ,
            decay   = input$decay_univ,
            lag     = input$lag_univ
          )
          
          incProgress(0.3)
          
          # Crear nombre para la columna transformada
          trans_col_name <- paste0(input$variable_univ, "_", gsub(" ", "_", input$transformation_univ))
          
          # Añadir la columna transformada al dataframe
          data[[trans_col_name]] <- transformed_data
          
          # Seleccionar solo las columnas necesarias para la descarga
          date_col <- if ("Period" %in% names(data)) "Period" else if ("periodo" %in% names(data)) "periodo" else NULL
          
          download_cols <- c(date_col, input$variable_univ, trans_col_name)
          # Agregar KPI si está seleccionado
          if (!is.null(input$kpi_univ) && input$kpi_univ != "N/A" && input$kpi_univ %in% names(data)) {
            download_cols <- c(download_cols, input$kpi_univ)
          }
          
          # Filtrar solo las columnas seleccionadas
          download_data <- data[, download_cols, drop = FALSE]
          
          # Añadir metadatos sobre la transformación
          metadata <- data.frame(
            Parametro = c("Variable", "Transformación", "Alpha", "Beta", "Max Value %", "Decay", "Lag", "Fecha Inicio", "Fecha Fin", "Fecha Descarga"),
            Valor = c(
              input$variable_univ,
              input$transformation_univ,
              input$alpha_univ,
              input$beta_univ,
              input$maxval_univ,
              input$decay_univ,
              input$lag_univ,
              ifelse(!is.null(input$date_range_univ), as.character(input$date_range_univ[1]), NA),
              ifelse(!is.null(input$date_range_univ), as.character(input$date_range_univ[2]), NA),
              as.character(Sys.Date())
            )
          )
          
          incProgress(0.2)
          
          # Escribir datos y metadatos a archivos separados
          write.csv(download_data, file, row.names = FALSE, na = "")
          
          # Crear archivo de metadatos con el mismo nombre pero con sufijo _metadata
          metadata_file <- sub("\\.csv$", "_metadata.csv", file)
          write.csv(metadata, metadata_file, row.names = FALSE)
          
          incProgress(0.1)
        })
        
        notifyUser("Archivo de datos transformados univariados descargado correctamente.", "message", duration = 4)
        
      }, error = function(e) {
        notifyUser(paste("Error en la descarga de datos univariados:", e$message), "error", duration = 5)
      })
    }
  )
}

# Manejador para descargar datos transformados en módulo multivariado
multivariate_download_handler <- function(input, output, session, rv) {
  # Configura los manejadores de descarga para datos transformados multivariados
  # (modo suma y modo individual)
  #
  # Args:
  #   input: Objeto input de Shiny
  #   output: Objeto output de Shiny
  #   session: Objeto session de Shiny
  #   rv: Valores reactivos compartidos
  
  # 1. Manejador para modo suma
  output$download_multivariate_sum <- downloadHandler(
    filename = function() {
      paste0("multivariate_summed_", Sys.Date(), ".csv")
    },
    content = function(file) {
      tryCatch({
        withProgress(message = 'Preparing summed data download...', value = 0.1, {
          # Obtener datos filtrados
          data <- rv$filtered_data
          if (is.null(data) || nrow(data) == 0) {
            stop("No hay datos disponibles para descargar.")
          }
          
          # Convertir la columna de fecha antes de filtrar
          if ("Period" %in% names(data)) {
            data$Period <- as.Date(data$Period)
          } else if ("periodo" %in% names(data)) {
            data$periodo <- as.Date(data$periodo)
          }
          
          # Aplicar filtro de fecha si está disponible
          if (!is.null(input$date_range_multi)) {
            date_col <- if ("Period" %in% names(data)) "Period" else if ("periodo" %in% names(data)) "periodo" else NULL
            if (!is.null(date_col)) {
              data <- data[data[[date_col]] >= input$date_range_multi[1] & 
                          data[[date_col]] <= input$date_range_multi[2], ]
            }
          }
          
          # Filtrar por geografía si es necesario
          if (!is.null(input$geography_multi) && input$geography_multi != "N/A") {
            geo_col <- if ("Geography" %in% names(data)) "Geography" else if ("Geografia" %in% names(data)) "Geografia" else NULL
            if (!is.null(geo_col) && input$geography_multi != "Total") {
              data <- data %>% filter(.data[[geo_col]] == input$geography_multi)
            } else if (input$geography_multi == "Total") {
              # Manejar caso "Total" - agrupar y sumar datos
              date_col <- if ("Period" %in% names(data)) "Period" else if ("periodo" %in% names(data)) "periodo" else NULL
              if (!is.null(date_col)) {
                numeric_cols <- names(data)[sapply(data, is.numeric)]
                data <- data %>%
                  group_by(across(all_of(date_col))) %>%
                  summarise(across(all_of(numeric_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
              }
            }
          }
          
          # Aplicar filtros adicionales (producto, campaña, etc.)
          if (!is.null(input$product_multi) && input$product_multi != "N/A") {
            prod_col <- if ("Product" %in% names(data)) "Product" else if ("Producto" %in% names(data)) "Producto" else NULL
            if (!is.null(prod_col)) {
              data <- data %>% filter(.data[[prod_col]] == input$product_multi)
            }
          }
          
          if (!is.null(input$campaign_multi) && input$campaign_multi != "N/A") {
            camp_col <- if ("Campaign" %in% names(data)) "Campaign" else if ("Campaña" %in% names(data)) "Campaña" else NULL
            if (!is.null(camp_col)) {
              data <- data %>% filter(.data[[camp_col]] == input$campaign_multi)
            }
          }
          
          if (!is.null(input$outlet_multi) && input$outlet_multi != "N/A") {
            outlet_col <- if ("Outlet" %in% names(data)) "Outlet" else NULL
            if (!is.null(outlet_col)) {
              data <- data %>% filter(.data[[outlet_col]] == input$outlet_multi)
            }
          }
          
          if (!is.null(input$creative_multi) && input$creative_multi != "N/A") {
            creative_col <- if ("Creative" %in% names(data)) "Creative" else if ("Creativo" %in% names(data)) "Creativo" else NULL
            if (!is.null(creative_col)) {
              data <- data %>% filter(.data[[creative_col]] == input$creative_multi)
            }
          }
          
          incProgress(0.2)
          
          # Identificar variables seleccionadas
          vars_selected <- c()
          if (!is.null(input$var1_multi) && input$var1_multi != "None") {
            vars_selected <- c(vars_selected, input$var1_multi)
          }
          if (!is.null(input$var2_multi) && input$var2_multi != "None") {
            vars_selected <- c(vars_selected, input$var2_multi)
          }
          if (!is.null(input$var3_multi) && input$var3_multi != "None") {
            vars_selected <- c(vars_selected, input$var3_multi)
          }
          if (!is.null(input$var4_multi) && input$var4_multi != "None") {
            vars_selected <- c(vars_selected, input$var4_multi)
          }
          
          vars_selected <- intersect(vars_selected, names(data))
          
          if (length(vars_selected) < 1) {
            stop("No hay variables válidas seleccionadas para la transformación.")
          }
          
          incProgress(0.2)
          
          # Calcular suma de variables
          data$sum_vars <- rowSums(data[, vars_selected, drop = FALSE], na.rm = TRUE)
          
          # Aplicar transformación a la suma
          data$trans_sum_vars <- apply_transformation(
            data$sum_vars,
            type    = input$trans_var1,
            alpha   = input$alpha_multi,
            beta    = input$beta_multi,
            maxval  = input$maxval_multi,
            decay   = input$decay_multi,
            lag     = input$lag_multi
          )
          
          incProgress(0.3)
          
          # Seleccionar solo las columnas necesarias para la descarga
          date_col <- if ("Period" %in% names(data)) "Period" else if ("periodo" %in% names(data)) "periodo" else NULL
          
          download_cols <- c(date_col, vars_selected, "sum_vars", "trans_sum_vars")
          # Agregar KPI si está seleccionado
          if (!is.null(input$kpi_multi) && input$kpi_multi != "None" && input$kpi_multi %in% names(data)) {
            download_cols <- c(download_cols, input$kpi_multi)
          }
          
          # Filtrar solo las columnas seleccionadas
          download_data <- data[, download_cols, drop = FALSE]
          
          # Crear metadatos para la transformación
          metadata <- data.frame(
            Parametro = c(
              "Variables Sumadas",
              "Transformación",
              "Alpha",
              "Beta",
              "Max Value %",
              "Decay",
              "Lag",
              "Fecha Inicio",
              "Fecha Fin",
              "Fecha Descarga"
            ),
            Valor = c(
              paste(vars_selected, collapse = ", "),
              ifelse(!is.null(input$trans_var1), input$trans_var1, "Linear"),
              ifelse(!is.null(input$alpha_multi), input$alpha_multi, NA),
              ifelse(!is.null(input$beta_multi), input$beta_multi, NA),
              ifelse(!is.null(input$maxval_multi), input$maxval_multi, NA),
              ifelse(!is.null(input$decay_multi), input$decay_multi, NA),
              ifelse(!is.null(input$lag_multi), input$lag_multi, NA),
              ifelse(!is.null(input$date_range_multi), as.character(input$date_range_multi[1]), NA),
              ifelse(!is.null(input$date_range_multi), as.character(input$date_range_multi[2]), NA),
              as.character(Sys.Date())
            )
          )
          
          incProgress(0.2)
          
          # Escribir datos y metadatos
          write.csv(download_data, file, row.names = FALSE, na = "")
          
          # Crear archivo de metadatos con el mismo nombre pero con sufijo _metadata
          metadata_file <- sub("\\.csv$", "_metadata.csv", file)
          write.csv(metadata, metadata_file, row.names = FALSE)
          
          incProgress(0.1)
        })
        
        notifyUser("Archivo de datos transformados multivariados (suma) descargado correctamente.", "message", duration = 4)
        
      }, error = function(e) {
        notifyUser(paste("Error en la descarga de datos multivariados (suma):", e$message), "error", duration = 5)
      })
    }
  )
  
  # 2. Manejador para modo individual
  output$download_multivariate_individual <- downloadHandler(
    filename = function() {
      paste0("multivariate_individual_", Sys.Date(), ".csv")
    },
    content = function(file) {
      tryCatch({
        withProgress(message = 'Preparing individual data download...', value = 0.1, {
          # Obtener datos filtrados
          data <- rv$filtered_data
          if (is.null(data) || nrow(data) == 0) {
            stop("No hay datos disponibles para descargar.")
          }
          
          # Convertir la columna de fecha antes de filtrar
          if ("Period" %in% names(data)) {
            data$Period <- as.Date(data$Period)
          } else if ("periodo" %in% names(data)) {
            data$periodo <- as.Date(data$periodo)
          }
          
          # Aplicar filtro de fecha si está disponible
          if (!is.null(input$date_range_multi)) {
            date_col <- if ("Period" %in% names(data)) "Period" else if ("periodo" %in% names(data)) "periodo" else NULL
            if (!is.null(date_col)) {
              data <- data[data[[date_col]] >= input$date_range_multi[1] & 
                          data[[date_col]] <= input$date_range_multi[2], ]
            }
          }
          
          # Filtrar por geografía si es necesario
          if (!is.null(input$geography_multi) && input$geography_multi != "N/A") {
            geo_col <- if ("Geography" %in% names(data)) "Geography" else if ("Geografia" %in% names(data)) "Geografia" else NULL
            if (!is.null(geo_col) && input$geography_multi != "Total") {
              data <- data %>% filter(.data[[geo_col]] == input$geography_multi)
            } else if (input$geography_multi == "Total") {
              # Manejar caso "Total" - agrupar y sumar datos
              date_col <- if ("Period" %in% names(data)) "Period" else if ("periodo" %in% names(data)) "periodo" else NULL
              if (!is.null(date_col)) {
                numeric_cols <- names(data)[sapply(data, is.numeric)]
                data <- data %>%
                  group_by(across(all_of(date_col))) %>%
                  summarise(across(all_of(numeric_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
              }
            }
          }
          
          # Aplicar filtros adicionales
          if (!is.null(input$product_multi) && input$product_multi != "N/A") {
            prod_col <- if ("Product" %in% names(data)) "Product" else if ("Producto" %in% names(data)) "Producto" else NULL
            if (!is.null(prod_col)) {
              data <- data %>% filter(.data[[prod_col]] == input$product_multi)
            }
          }
          
          if (!is.null(input$campaign_multi) && input$campaign_multi != "N/A") {
            camp_col <- if ("Campaign" %in% names(data)) "Campaign" else if ("Campaña" %in% names(data)) "Campaña" else NULL
            if (!is.null(camp_col)) {
              data <- data %>% filter(.data[[camp_col]] == input$campaign_multi)
            }
          }
          
          if (!is.null(input$outlet_multi) && input$outlet_multi != "N/A") {
            outlet_col <- if ("Outlet" %in% names(data)) "Outlet" else NULL
            if (!is.null(outlet_col)) {
              data <- data %>% filter(.data[[outlet_col]] == input$outlet_multi)
            }
          }
          
          if (!is.null(input$creative_multi) && input$creative_multi != "N/A") {
            creative_col <- if ("Creative" %in% names(data)) "Creative" else if ("Creativo" %in% names(data)) "Creativo" else NULL
            if (!is.null(creative_col)) {
              data <- data %>% filter(.data[[creative_col]] == input$creative_multi)
            }
          }
          
          incProgress(0.2)
          
          # Identificar variables seleccionadas
          vars_selected <- c()
          if (!is.null(input$var1_multi) && input$var1_multi != "None") {
            vars_selected <- c(vars_selected, input$var1_multi)
          }
          if (!is.null(input$var2_multi) && input$var2_multi != "None") {
            vars_selected <- c(vars_selected, input$var2_multi)
          }
          if (!is.null(input$var3_multi) && input$var3_multi != "None") {
            vars_selected <- c(vars_selected, input$var3_multi)
          }
          if (!is.null(input$var4_multi) && input$var4_multi != "None") {
            vars_selected <- c(vars_selected, input$var4_multi)
          }
          
          vars_selected <- intersect(vars_selected, names(data))
          
          if (length(vars_selected) < 1) {
            stop("No hay variables válidas seleccionadas para la descarga.")
          }
          
          incProgress(0.3)
          
          # Seleccionar columnas para descarga
          date_col <- if ("Period" %in% names(data)) "Period" else if ("periodo" %in% names(data)) "periodo" else NULL
          
          download_cols <- c(date_col, vars_selected)
          # Agregar KPI si está seleccionado
          if (!is.null(input$kpi_multi) && input$kpi_multi != "None" && input$kpi_multi %in% names(data)) {
            download_cols <- c(download_cols, input$kpi_multi)
          }
          
          # Filtrar solo las columnas seleccionadas
          download_data <- data[, download_cols, drop = FALSE]
          
          # Crear metadatos simples
          metadata <- data.frame(
            Parametro = c(
              "Variables Seleccionadas",
              "Fecha Inicio",
              "Fecha Fin",
              "Fecha Descarga"
            ),
            Valor = c(
              paste(vars_selected, collapse = ", "),
              ifelse(!is.null(input$date_range_multi), as.character(input$date_range_multi[1]), NA),
              ifelse(!is.null(input$date_range_multi), as.character(input$date_range_multi[2]), NA),
              as.character(Sys.Date())
            )
          )
          
          incProgress(0.2)
          
          # Escribir datos y metadatos
          write.csv(download_data, file, row.names = FALSE, na = "")
          
          # Crear archivo de metadatos con el mismo nombre pero con sufijo _metadata
          metadata_file <- sub("\\.csv$", "_metadata.csv", file)
          write.csv(metadata, metadata_file, row.names = FALSE)
          
          incProgress(0.1)
        })
        
        notifyUser("Archivo de datos individuales descargado correctamente.", "message", duration = 4)
        
      }, error = function(e) {
        notifyUser(paste("Error en la descarga de datos multivariados (individuales):", e$message), "error", duration = 5)
      })
    }
  )
}