# R/modules/common/global_filters.R
# =============================================================================
# Módulo: global_filters
# Actualiza los selectInputs y controles globales según las columnas del dataset,
# incluyendo filtros adicionales para Product, Campaign, Outlet y Creative.
# =============================================================================

library(stringr)
library(dplyr)

# # Función para extraer la información de las columnas métricas con formato:
# # "VariableName_Product_Campaign_Outlet_Creative"
# extract_metric_info <- function(data) {
#   metric_cols <- setdiff(names(data), c("Geography", "Geografia", "Period", "periodo"))
#   metric_cols <- metric_cols[sapply(data[metric_cols], is.numeric)]
#   info_list <- lapply(metric_cols, function(col) {
#     parts <- unlist(str_split(col, "_"))
#     if (length(parts) == 5) {
#       data.frame(
#         colname = col,
#         variable = parts[1],
#         product = parts[2],
#         campaign = parts[3],
#         outlet = parts[4],
#         creative = parts[5],
#         stringsAsFactors = FALSE
#       )
#     } else {
#       NULL
#     }
#   })
#   info_df <- do.call(rbind, info_list)
#   return(info_df)
# }

extract_metric_info <- function(data) {
  metric_cols <- setdiff(names(data), c("Geography", "Geografia", "Period", "periodo"))
  metric_cols <- metric_cols[sapply(data[metric_cols], is.numeric)]
  
  info_list <- lapply(metric_cols, function(col) {
    parts <- unlist(str_split(col, "_"))
    
    # Si la columna tiene exactamente 5 partes, se asignan normalmente
    if (length(parts) == 5) {
      return(data.frame(
        colname = col,
        variable = parts[1],
        product = parts[2],
        campaign = parts[3],
        outlet = parts[4],
        creative = parts[5],
        stringsAsFactors = FALSE
      ))
    } else {
      # Si no tiene 5 partes, incluirla con valores NA en las demás columnas
      return(data.frame(
        colname = col,
        variable = parts[1],  # Se usa la primera parte como el nombre de la variable
        product = NA,
        campaign = NA,
        outlet = NA,
        creative = NA,
        stringsAsFactors = FALSE
      ))
    }
  })
  
  # Combinar todas las filas generadas
  info_df <- do.call(rbind, info_list)
  return(info_df)
}


update_global_filters <- function(data, input, session) {
  req(data)
  
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  
  # Variables para medios y gasto
  media_keywords <- c("Impressions", "Circulation", "Clicks", "Display", "OOH", "OLV", "Magazine", "Newspaper")
  spend_keywords <- c("Cost", "Spend")
  
  MEDIA_VARIABLES <- grep(paste(media_keywords, collapse = "|"), names(data), value = TRUE, ignore.case = TRUE)
  SPEND_VARIABLES <- grep(paste(spend_keywords, collapse = "|"), MEDIA_VARIABLES, value = TRUE, ignore.case = TRUE)
  
  MEDIA_VARIABLES <- setdiff(MEDIA_VARIABLES, SPEND_VARIABLES)
  MEDIA_VARIABLES <- intersect(MEDIA_VARIABLES, numeric_cols)
  SPEND_VARIABLES <- intersect(SPEND_VARIABLES, numeric_cols)
  
  # Actualizar selectInputs generales
  updateSelectInput(session, "kpi", choices = numeric_cols)
  
  # Se asignan valores por defecto para que los reactives que dependen de ellos se activen
  updateSelectInput(session, "media_vars", 
                    choices = MEDIA_VARIABLES, 
                    selected = if(length(MEDIA_VARIABLES) > 0) MEDIA_VARIABLES else NULL)
  updateSelectInput(session, "spend_vars", 
                    choices = SPEND_VARIABLES, 
                    selected = if(length(SPEND_VARIABLES) > 0) SPEND_VARIABLES else NULL)
  
  updateSelectInput(session, "base_vars", choices = numeric_cols)
  
  updateSelectInput(session, "kpi_univ", choices = numeric_cols)
  
  # Para el panel multivariate (actualizamos también los selectInputs de variables)
  updateSelectInput(session, "kpi_multi", choices = numeric_cols)
  updateSelectInput(session, "var1_multi", choices = numeric_cols)
  updateSelectInput(session, "var2_multi", choices = numeric_cols)
  updateSelectInput(session, "var3_multi", choices = numeric_cols)
  updateSelectInput(session, "var4_multi", choices = c("None", numeric_cols))
  
  # Extraer información de las métricas para los filtros compuestos
  metric_info <- extract_metric_info(data)
  if (!is.null(metric_info) && nrow(metric_info) > 0) {
    updateSelectInput(session, "variable_univ", choices = metric_info$colname)
    updateSelectInput(session, "product_univ", choices = c("N/A", sort(unique(metric_info$product))), selected = "N/A")
    updateSelectInput(session, "campaign_univ", choices = c("N/A", sort(unique(metric_info$campaign))), selected = "N/A")
    updateSelectInput(session, "outlet_univ", choices = c("N/A", sort(unique(metric_info$outlet))), selected = "N/A")
    updateSelectInput(session, "creative_univ", choices = c("N/A", sort(unique(metric_info$creative))), selected = "N/A")
    
    updateSelectInput(session, "product_multi", choices = c("N/A", sort(unique(metric_info$product))), selected = "N/A")
    updateSelectInput(session, "campaign_multi", choices = c("N/A", sort(unique(metric_info$campaign))), selected = "N/A")
    updateSelectInput(session, "outlet_multi", choices = c("N/A", sort(unique(metric_info$outlet))), selected = "N/A")
    updateSelectInput(session, "creative_multi", choices = c("N/A", sort(unique(metric_info$creative))), selected = "N/A")
  } else {
    updateSelectInput(session, "variable_univ", choices = c("N/A"))
    updateSelectInput(session, "product_univ", choices = c("N/A"))
    updateSelectInput(session, "campaign_univ", choices = c("N/A"))
    updateSelectInput(session, "outlet_univ", choices = c("N/A"))
    updateSelectInput(session, "creative_univ", choices = c("N/A"))
    
    updateSelectInput(session, "product_multi", choices = c("N/A"))
    updateSelectInput(session, "campaign_multi", choices = c("N/A"))
    updateSelectInput(session, "outlet_multi", choices = c("N/A"))
    updateSelectInput(session, "creative_multi", choices = c("N/A"))
  }
  
  # Actualizar filtro de Geografía (soporta nombres en inglés y español)
  if ("Geography" %in% names(data) || "Geografia" %in% names(data)) {
    geo_col <- if("Geography" %in% names(data)) "Geography" else "Geografia"
    geos <- sort(unique(data[[geo_col]]))
    choices <- c("Total", geos)
    updateSelectInput(session, "geography_univ", choices = choices, selected = "Total")
    updateSelectInput(session, "geography_multi", choices = choices, selected = "Total")
    notifyUser("Filtros de Geografía actualizados.", "message", duration = 4)
  } else {
    notifyUser("Columna 'Geography' o 'Geografia' no encontrada. Filtros de geografía no actualizados.", "warning", duration = 6)
  }
  
  notifyUser("SelectInputs actualizados con las columnas del dataset.", "message", duration = 4)
}
