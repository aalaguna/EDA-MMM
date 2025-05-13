# Global_Filters.R

# =============================================================================
# Updates selectInputs and global controls based on dataset columns,
# including additional filters for Product, Campaign, Outlet, and Creative.
# =============================================================================

extract_metric_info <- function(data) {
  # Extracts metric information from dataframe columns
  #
  # Args:
  #   data: Dataframe with data
  #
  # Returns:
  #   Dataframe with detailed information for each metric column
  
  # Identify only numeric columns
  metric_cols <- setdiff(names(data), c("Geography", "Geografia", "Period", "periodo"))
  metric_cols <- metric_cols[sapply(data[metric_cols], is.numeric)]
  
  info_list <- lapply(metric_cols, function(col) {
    parts <- unlist(str_split(col, "_"))
    if (length(parts) == 5) {
      return(data.frame(
        colname   = col,
        variable  = parts[1],
        product   = parts[2],
        campaign  = parts[3],
        outlet    = parts[4],
        creative  = parts[5],
        stringsAsFactors = FALSE
      ))
    } else {
      # If it doesn't have 5 parts, still include it
      return(data.frame(
        colname   = col,
        variable  = parts[1],
        product   = NA,
        campaign  = NA,
        outlet    = NA,
        creative  = NA,
        stringsAsFactors = FALSE
      ))
    }
  })
  info_df <- do.call(rbind, info_list)
  return(info_df)
}

update_global_filters <- function(data, input, session) {
  # Updates all global filters based on the loaded dataframe
  #
  # Args:
  #   data: Dataframe with data
  #   input: Shiny input object
  #   session: Shiny session object
  
  req(data)
  
  # Identify numeric columns
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  
  # Possible media and cost variables
  media_keywords <- c("Impressions", "Circulation", "Clicks", 'Views', 
                      'Attendance', 'Dispatch', 'Delivered', 'Open', 
                      'Opened', 'GRPs', 'Sent', 'Sents',
                      "Display", "OOH", "OLV", "Magazine", "Newspaper")
  spend_keywords <- c("Cost", "Spend")
  competitors_to_exclude = c('Adthena', 'Kantar', 'Nielsen', 'Vivvix', 'Pathmathics')
  words_to_exclude = c('Day', 'Sentiment')
  
  
  MEDIA_VARIABLES <- grep(paste(media_keywords, collapse = "|"), names(data), value = TRUE, ignore.case = TRUE)
  MEDIA_VARIABLES <- grep(paste(competitors_to_exclude, collapse = "|"), MEDIA_VARIABLES, value = TRUE, ignore.case = TRUE, invert = TRUE)
  MEDIA_VARIABLES <- grep(paste(words_to_exclude, collapse = "|"), MEDIA_VARIABLES, value = TRUE, ignore.case = TRUE, invert = TRUE)
  SPEND_VARIABLES <- grep(paste(spend_keywords, collapse = "|"), MEDIA_VARIABLES, value = TRUE, ignore.case = TRUE)
  
  MEDIA_VARIABLES <- setdiff(MEDIA_VARIABLES, SPEND_VARIABLES)
  MEDIA_VARIABLES <- intersect(MEDIA_VARIABLES, numeric_cols)
  SPEND_VARIABLES <- intersect(SPEND_VARIABLES, numeric_cols)
  
  # Update KPI
  updateSelectInput(session, "kpi", choices = numeric_cols, selected = NULL)
  
  # # Update Media / Spend / Base
  # updateSelectInput(session, "media_vars", choices = MEDIA_VARIABLES, selected = MEDIA_VARIABLES)
  # updateSelectInput(session, "spend_vars", choices = SPEND_VARIABLES, selected = SPEND_VARIABLES)
  # updateSelectInput(session, "base_vars",  choices = numeric_cols,   selected = NULL)
  # 
  # Update Media / Spend / Base
  updateSelectizeInput(session, "media_vars", choices = MEDIA_VARIABLES, selected = MEDIA_VARIABLES, 
                       options = list(plugins = list("remove_button")))
  updateSelectizeInput(session, "spend_vars", choices = SPEND_VARIABLES, selected = SPEND_VARIABLES, 
                       options = list(plugins = list("remove_button")))
  updateSelectizeInput(session, "base_vars", choices = numeric_cols, selected = NULL, 
                       options = list(plugins = list("remove_button")))
  
  # Univariate panel: KPI
  updateSelectInput(session, "kpi_univ", choices = numeric_cols, selected = NULL)
  
  # Multivariate panel
  # Add "None" as the first option and set it as the default selection
  updateSelectInput(session, "kpi_multi",    choices = c("None", numeric_cols), selected = "None")
  updateSelectInput(session, "var1_multi",   choices = c("None", numeric_cols), selected = "None")
  updateSelectInput(session, "var2_multi",   choices = c("None", numeric_cols), selected = "None")
  updateSelectInput(session, "var3_multi",   choices = c("None", numeric_cols), selected = "None")
  updateSelectInput(session, "var4_multi",   choices = c("None", numeric_cols), selected = "None")
  
  # Metrics
  metric_info <- extract_metric_info(data)
  if (!is.null(metric_info) && nrow(metric_info) > 0) {
    updateSelectInput(session, "variable_univ", choices = metric_info$colname, selected = NULL)
    updateSelectInput(session, "product_univ",  choices = c("N/A", sort(unique(metric_info$product))),   selected = "N/A")
    updateSelectInput(session, "campaign_univ", choices = c("N/A", sort(unique(metric_info$campaign))), selected = "N/A")
    updateSelectInput(session, "outlet_univ",   choices = c("N/A", sort(unique(metric_info$outlet))),   selected = "N/A")
    updateSelectInput(session, "creative_univ", choices = c("N/A", sort(unique(metric_info$creative))), selected = "N/A")
    
    updateSelectInput(session, "product_multi",  choices = c("N/A", sort(unique(metric_info$product))),   selected = "N/A")
    updateSelectInput(session, "campaign_multi", choices = c("N/A", sort(unique(metric_info$campaign))), selected = "N/A")
    updateSelectInput(session, "outlet_multi",   choices = c("N/A", sort(unique(metric_info$outlet))),   selected = "N/A")
    updateSelectInput(session, "creative_multi", choices = c("N/A", sort(unique(metric_info$creative))), selected = "N/A")
  } else {
    # If there is no metric_info, set everything to "N/A"
    updateSelectInput(session, "variable_univ", choices = c("N/A"), selected = "N/A")
    updateSelectInput(session, "product_univ",  choices = c("N/A"), selected = "N/A")
    updateSelectInput(session, "campaign_univ", choices = c("N/A"), selected = "N/A")
    updateSelectInput(session, "outlet_univ",   choices = c("N/A"), selected = "N/A")
    updateSelectInput(session, "creative_univ", choices = c("N/A"), selected = "N/A")
    
    updateSelectInput(session, "product_multi",  choices = c("N/A"), selected = "N/A")
    updateSelectInput(session, "campaign_multi", choices = c("N/A"), selected = "N/A")
    updateSelectInput(session, "outlet_multi",   choices = c("N/A"), selected = "N/A")
    updateSelectInput(session, "creative_multi", choices = c("N/A"), selected = "N/A")
  }
  
  # Geography
  if ("Geography" %in% names(data) || "Geografia" %in% names(data)) {
    geo_col <- if ("Geography" %in% names(data)) "Geography" else "Geografia"
    geos <- sort(unique(data[[geo_col]]))
    choices <- c("Total", geos)
    updateSelectInput(session, "geography_univ",  choices = choices, selected = "Total")
    updateSelectInput(session, "geography_multi", choices = choices, selected = "Total")
    notifyUser("Geography filters updated.", "message", duration = 4)
  } else {
    notifyUser("Column 'Geography' or 'Geografia' not found. Geography filters not updated.", "warning", duration = 6)
  }
  
  notifyUser("SelectInputs updated with dataset columns.", "message", duration = 4)}
