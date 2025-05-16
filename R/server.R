# R/server.R
# =============================================================================
# Main server logic.
# Integrates data loading, initial global filters, date filtering,
# dynamic compound filters for all panels,
# module calls and download handling.
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(corrplot)
  library(zoo)
  library(stringr)
  library(DT)
  library(plotly)
  library(purrr)
  library(gridExtra)
  library(scales)
  library(RColorBrewer)
  library(moments) # Added for skewness and kurtosis in KPI Transformation
  library(lubridate)
})

# --- Load common modules ---
source("R/Modules/Common/Global_Filters.R", local = TRUE)
source("R/modules/common/download_handler.R", local = TRUE)
source("R/modules/common/download_transformed_handlers.R", local = TRUE)

# --- Load information modules ---
source("R/modules/information/data_loader.R", local = TRUE)
source("R/modules/information/date_filter.R", local = TRUE)
source("R/modules/information/dimensions_ui.R", local = TRUE)
source("R/modules/information/data_summary.R", local = TRUE)
source("R/modules/information/file_info.R", local = TRUE)

# --- Load Univariate modules ---
source("R/modules/univariate/Univariate_Server.R", local = TRUE)

# --- Load Multivariate modules ---
source("R/modules/multivariate/Multivariate_Server.R", local = TRUE)

# --- Load KPI Transformation modules ---
source("R/modules/kpi/KPI_Transformation_Server.R", local = TRUE)

server <- function(input, output, session) {
  # --- Global reactive values ---
  rv <- reactiveValues(
    data          = NULL, # Original loaded data
    filtered_data = NULL, # Data filtered ONLY by date
    date_col      = NULL  # Name of the detected date column
  )
  
  # --- 1. Data loading ---
  observeEvent(input$file, {
    data <- load_data(input$file)
    if (!is.null(data)) {
      # Try to detect date column
      date_col <- names(data)[tolower(names(data)) %in% c("period", "periodo")][1]
      if (!is.na(date_col)) {
        tryCatch(
          {
            data[[date_col]] <- as.Date(data[[date_col]])
            rv$date_col <- date_col
            # Set initial date range in the input
            updateDateRangeInput(session, "date_range_filter",
                                 start = min(data[[date_col]], na.rm = TRUE),
                                 end   = max(data[[date_col]], na.rm = TRUE)
            )
            showNotification(paste("Date column detected:", date_col), type = "message")
          },
          error = function(e) {
            showNotification(paste("Error converting column", date_col, "to Date:", e$message), type = "error")
            rv$date_col <- NULL # Reset if conversion fails
          }
        )
      } else {
        showNotification("Warning: Could not detect a date column named 'Period' or 'periodo'. Date filter will not work.", type = "warning", duration = 10)
        rv$date_col <- NULL
      }
      
      rv$data <- data
      rv$filtered_data <- data # Initially, date-filtered data is the full dataset
      showNotification("Data loaded successfully.", type = "message")
    } else {
      showNotification("Error loading data from the file.", type = "error")
      # Reset values if loading fails
      rv$data <- NULL
      rv$filtered_data <- NULL
      rv$date_col <- NULL
    }
  })
  
  # --- 2. Initial update of global filters ---
  observe({
    req(rv$data)
    update_global_filters(rv$data, input, session)
  })
  
  # --- 3. Date filtering ---
  filter_data_by_date(input, rv)
  
  # --- 4. Dynamic filtering and SelectInput update for Univariate ---
  
  # (a) Update of the "variable_univ" selectInput based on compound _univ filters
  observe({
    req(rv$data)
    metric_info <- extract_metric_info(rv$data)
    req(metric_info)
    
    # Filter available metrics based on _univ selections
    if (!is.null(input$product_univ) && all(input$product_univ != "N/A")) {
      metric_info <- metric_info %>% filter(product %in% input$product_univ)
    }
    if (!is.null(input$campaign_univ) && all(input$campaign_univ != "N/A")) {
      metric_info <- metric_info %>% filter(campaign %in% input$campaign_univ)
    }
    if (!is.null(input$outlet_univ) && all(input$outlet_univ != "N/A")) {
      metric_info <- metric_info %>% filter(outlet %in% input$outlet_univ)
    }
    if (!is.null(input$creative_univ) && all(input$creative_univ != "N/A")) {
      metric_info <- metric_info %>% filter(creative %in% input$creative_univ)
    }
    
    current_selection <- input$variable_univ
    choices <- if (nrow(metric_info) > 0) unique(metric_info$colname) else character(0)
    
    selected_choice <- if (!is.null(current_selection) && current_selection %in% choices) {
      current_selection
    } else if (length(choices) > 0) {
      choices[1]
    } else {
      NULL
    }
    
    updateSelectInput(session, "variable_univ", choices = choices, selected = selected_choice)
  })
  
  # (b) Interdependent dynamic update of compound _univ filters
  observe({
    req(rv$data)
    info <- extract_metric_info(rv$data)
    req(info)
    
    # Save current selections to restore them if still valid
    current_product <- input$product_univ
    current_campaign <- input$campaign_univ
    current_outlet <- input$outlet_univ
    current_creative <- input$creative_univ
    
    # Compute choices for Product (considering Campaign, Outlet, Creative)
    info_for_product <- info
    if (!is.null(current_campaign) && all(current_campaign != "N/A")) info_for_product <- filter(info_for_product, campaign %in% current_campaign)
    if (!is.null(current_outlet) && all(current_outlet != "N/A")) info_for_product <- filter(info_for_product, outlet %in% current_outlet)
    if (!is.null(current_creative) && all(current_creative != "N/A")) info_for_product <- filter(info_for_product, creative %in% current_creative)
    product_choices <- c("N/A", sort(unique(na.omit(info_for_product$product))))
    
    # Compute choices for Campaign (considering Product, Outlet, Creative)
    info_for_campaign <- info
    if (!is.null(current_product) && all(current_product != "N/A")) info_for_campaign <- filter(info_for_campaign, product %in% current_product)
    if (!is.null(current_outlet) && all(current_outlet != "N/A")) info_for_campaign <- filter(info_for_campaign, outlet %in% current_outlet)
    if (!is.null(current_creative) && all(current_creative != "N/A")) info_for_campaign <- filter(info_for_campaign, creative %in% current_creative)
    campaign_choices <- c("N/A", sort(unique(na.omit(info_for_campaign$campaign))))
    
    # Compute choices for Outlet (considering Product, Campaign, Creative)
    info_for_outlet <- info
    if (!is.null(current_product) && all(current_product != "N/A")) info_for_outlet <- filter(info_for_outlet, product %in% current_product)
    if (!is.null(current_campaign) && all(current_campaign != "N/A")) info_for_outlet <- filter(info_for_outlet, campaign %in% current_campaign)
    if (!is.null(current_creative) && all(current_creative != "N/A")) info_for_outlet <- filter(info_for_outlet, creative %in% current_creative)
    outlet_choices <- c("N/A", sort(unique(na.omit(info_for_outlet$outlet))))
    
    # Compute choices for Creative (considering Product, Campaign, Outlet)
    info_for_creative <- info
    if (!is.null(current_product) && all(current_product != "N/A")) info_for_creative <- filter(info_for_creative, product %in% current_product)
    if (!is.null(current_campaign) && all(current_campaign != "N/A")) info_for_creative <- filter(info_for_creative, campaign %in% current_campaign)
    if (!is.null(current_outlet) && all(current_outlet != "N/A")) info_for_creative <- filter(info_for_creative, outlet %in% current_outlet)
    creative_choices <- c("N/A", sort(unique(na.omit(info_for_creative$creative))))
    
    # Update SelectInputs, keeping the current selection if valid, otherwise default to 'N/A'
    updateSelectInput(session, "product_univ",
                      choices = product_choices,
                      selected = if (!is.null(current_product) && all(current_product %in% product_choices)) current_product else "N/A"
    )
    updateSelectInput(session, "campaign_univ",
                      choices = campaign_choices,
                      selected = if (!is.null(current_campaign) && all(current_campaign %in% campaign_choices)) current_campaign else "N/A"
    )
    updateSelectInput(session, "outlet_univ",
                      choices = outlet_choices,
                      selected = if (!is.null(current_outlet) && all(current_outlet %in% outlet_choices)) current_outlet else "N/A"
    )
    updateSelectInput(session, "creative_univ",
                      choices = creative_choices,
                      selected = if (!is.null(current_creative) && all(current_creative %in% creative_choices)) current_creative else "N/A"
    )
  })
  
  # --- 5. Dynamic filtering and SelectInput update for Multivariate ---
  
  # (a) Interdependent dynamic update of compound _multi filters
  observe({
    req(rv$data)
    info <- extract_metric_info(rv$data)
    req(info)
    
    # Save current selections
    current_product_multi <- input$product_multi
    current_campaign_multi <- input$campaign_multi
    current_outlet_multi <- input$outlet_multi
    current_creative_multi <- input$creative_multi
    
    # Compute options for Product (considering Campaign, Outlet, Creative _multi)
    info_product_multi <- info
    if (!is.null(current_campaign_multi) && all(current_campaign_multi != "N/A")) info_product_multi <- filter(info_product_multi, campaign %in% current_campaign_multi)
    if (!is.null(current_outlet_multi) && all(current_outlet_multi != "N/A")) info_product_multi <- filter(info_product_multi, outlet %in% current_outlet_multi)
    if (!is.null(current_creative_multi) && all(current_creative_multi != "N/A")) info_product_multi <- filter(info_product_multi, creative %in% current_creative_multi)
    product_choices_multi <- c("N/A", sort(unique(na.omit(info_product_multi$product))))
    
    # Compute options for Campaign (considering Product, Outlet, Creative _multi)
    info_campaign_multi <- info
    if (!is.null(current_product_multi) && all(current_product_multi != "N/A")) info_campaign_multi <- filter(info_campaign_multi, product %in% current_product_multi)
    if (!is.null(current_outlet_multi) && all(current_outlet_multi != "N/A")) info_campaign_multi <- filter(info_campaign_multi, outlet %in% current_outlet_multi)
    if (!is.null(current_creative_multi) && all(current_creative_multi != "N/A")) info_campaign_multi <- filter(info_campaign_multi, creative %in% current_creative_multi)
    campaign_choices_multi <- c("N/A", sort(unique(na.omit(info_campaign_multi$campaign))))
    
    # Compute options for Outlet (considering Product, Campaign, Creative _multi)
    info_outlet_multi <- info
    if (!is.null(current_product_multi) && all(current_product_multi != "N/A")) info_outlet_multi <- filter(info_outlet_multi, product %in% current_product_multi)
    if (!is.null(current_campaign_multi) && all(current_campaign_multi != "N/A")) info_outlet_multi <- filter(info_outlet_multi, campaign %in% current_campaign_multi)
    if (!is.null(current_creative_multi) && all(current_creative_multi != "N/A")) info_outlet_multi <- filter(info_outlet_multi, creative %in% current_creative_multi)
    outlet_choices_multi <- c("N/A", sort(unique(na.omit(info_outlet_multi$outlet))))
    
    # Compute options for Creative (considering Product, Campaign, Outlet _multi)
    info_creative_multi <- info
    if (!is.null(current_product_multi) && all(current_product_multi != "N/A")) info_creative_multi <- filter(info_creative_multi, product %in% current_product_multi)
    if (!is.null(current_campaign_multi) && all(current_campaign_multi != "N/A")) info_creative_multi <- filter(info_creative_multi, campaign %in% current_campaign_multi)
    if (!is.null(current_outlet_multi) && all(current_outlet_multi != "N/A")) info_creative_multi <- filter(info_creative_multi, outlet %in% current_outlet_multi)
    creative_choices_multi <- c("N/A", sort(unique(na.omit(info_creative_multi$creative))))
    
    # Update _multi SelectInputs
    updateSelectInput(session, "product_multi",
                      choices = product_choices_multi,
                      selected = if (!is.null(current_product_multi) && all(current_product_multi %in% product_choices_multi)) current_product_multi else "N/A"
    )
    updateSelectInput(session, "campaign_multi",
                      choices = campaign_choices_multi,
                      selected = if (!is.null(current_campaign_multi) && all(current_campaign_multi %in% campaign_choices_multi)) current_campaign_multi else "N/A"
    )
    updateSelectInput(session, "outlet_multi",
                      choices = outlet_choices_multi,
                      selected = if (!is.null(current_outlet_multi) && all(current_outlet_multi %in% outlet_choices_multi)) current_outlet_multi else "N/A"
    )
    updateSelectInput(session, "creative_multi",
                      choices = creative_choices_multi,
                      selected = if (!is.null(current_creative_multi) && all(current_creative_multi %in% creative_choices_multi)) current_creative_multi else "N/A"
    )
  })
  
  # (b) Dynamic update of VARIABLE SELECTION (var1_multi, etc.) for Multivariate
  observe({
    req(rv$data)
    metric_info <- extract_metric_info(rv$data)
    req(metric_info)
    
    # Filter metrics based on _multi selections
    if (!is.null(input$product_multi) && all(input$product_multi != "N/A")) {
      metric_info <- metric_info %>% filter(product %in% input$product_multi)
    }
    if (!is.null(input$campaign_multi) && all(input$campaign_multi != "N/A")) {
      metric_info <- metric_info %>% filter(campaign %in% input$campaign_multi)
    }
    if (!is.null(input$outlet_multi) && all(input$outlet_multi != "N/A")) {
      metric_info <- metric_info %>% filter(outlet %in% input$outlet_multi)
    }
    if (!is.null(input$creative_multi) && all(input$creative_multi != "N/A")) {
      metric_info <- metric_info %>% filter(creative %in% input$creative_multi)
    }
    
    # Get valid column names (metrics)
    choices <- if (nrow(metric_info) > 0) unique(metric_info$colname) else character(0)
    choices_with_none <- c("None", choices) # Add "None"
    
    # Helper function to update while preserving selection if valid
    update_multi_var_select <- function(inputId, current_selection) {
      selected_choice <- if (!is.null(current_selection) && current_selection %in% choices_with_none) {
        current_selection
      } else {
        "None" # Default to "None" if the current selection is no longer valid or is NULL
      }
      updateSelectInput(session, inputId, choices = choices_with_none, selected = selected_choice)
    }
    
    # Update variable selectors for the multivariate panel
    update_multi_var_select("kpi_multi", input$kpi_multi)
    update_multi_var_select("var1_multi", input$var1_multi)
    update_multi_var_select("var2_multi", input$var2_multi)
    update_multi_var_select("var3_multi", input$var3_multi)
    update_multi_var_select("var4_multi", input$var4_multi)
  })
  
  # --- 6. Exclusive filtering for the Multivariate panel (Geography) ---
  rv$filtered_data_multi <- reactive({
    # Depends on date-filtered data and the geography_multi input
    req(rv$filtered_data, input$geography_multi, rv$date_col)
    
    df <- rv$filtered_data # Start with data already filtered by date
    
    # Detect geography column
    geo_col <- NULL
    if ("Geography" %in% names(df)) {
      geo_col <- "Geography"
    } else if ("Geografia" %in% names(df)) geo_col <- "Geografia"
    
    # Apply geography filter
    if (!is.null(geo_col) && input$geography_multi != "Total" && input$geography_multi != "N/A") {
      # Filter by selected geography
      df <- df %>% filter(.data[[geo_col]] == input$geography_multi)
    } else if (is.null(geo_col) && input$geography_multi != "Total") {
      # If there's no geo column but something other than Total is selected, warn and return unfiltered df
      showNotification("Warning: No geography column found to filter in Multivariate.", type = "warning")
      # Return df as is (already filtered by date)
    } else if (input$geography_multi == "Total") {
      # If "Total" is selected, aggregate numerics grouped by date
      # Ensure the date column exists (already checked with req(rv$date_col))
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      
      # Group only by the detected date column
      df <- df %>%
        group_by(across(all_of(rv$date_col))) %>%
        summarise(across(all_of(numeric_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
    }
    
    return(df)
  })
  
  # --- 7. Modules for the Information tab ---
  dimensions_ui_module_server(input, output, session, rv)
  data_summary_module_server(input, output, session, rv)
  file_info_module_server(input, output, session, rv)
  
  # --- 8. Module calls: Univariate and Multivariate ---
  # The Univariate module uses rv$filtered_data and applies internal filters (geography_univ, variable_univ, etc.)
  univariate_module_server(input, output, session, rv)
  
  # The Multivariate module uses rv$filtered_data_multi()
  multivariate_module_server(input, output, session, rv)
  
  # --- 9. Module call: KPI Transformation (new) ---
  kpi_transformation_module_server(input, output, session, rv)
  
  # --- 10. Download module for analytical data ---
  download_handler_module(input, output, session, rv) # General download
  
  # --- 11. Handlers for transformed data downloads ---
  univariate_download_handler(input, output, session, rv)
  multivariate_download_handler(input, output, session, rv)
  
  # --- 12. Observer for animation classes in download buttons ---
  observe({
    # Initialize or reset the class (e.g., when loading the app or switching tabs)
    ids <- c("download_univariate", "download_multivariate_sum", 
             "download_multivariate_individual", "download_transformed_kpi", 
             "download_normalized_kpi")
    for (id in ids) {
      session$sendCustomMessage(
        type = "toggleDownloadButtonClass",
        message = list(id = id, downloading = FALSE)
      )
    }
  })
} # End of server function