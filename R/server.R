# =============================================================================
# Lógica principal del servidor.
# (univariate, multivariate, filtros, etc.) y se integra el módulo data_summary

# =============================================================================

suppressPackageStartupMessages({
    library(shiny)
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(corrplot)  # Si no se usa, puede quitarlo
    library(zoo)
    library(stringr)
    library(DT)
    library(plotly)
    library(purrr)
    library(gridExtra) # Si no se usa, puede quitarlo
})

# --- Cargar módulos comunes ---
source("R/modules/common/global_filters.R", local = TRUE)
source("R/modules/common/download_handler.R", local = TRUE)

# --- Cargar módulos Information ---
source("R/modules/information/data_loader.R", local = TRUE)
source("R/modules/information/date_filter.R", local = TRUE)
source("R/modules/information/dimensions_ui.R", local = TRUE)
source("R/modules/information/data_summary.R", local = TRUE)  # Módulo de resumen
source("R/modules/information/file_info.R", local = TRUE)

# --- Cargar módulos Univariate ---
source("R/modules/univariate/univariate_module.R", local = TRUE)

# --- Cargar módulos Multivariate ---
source("R/modules/multivariate/multivariate_module.R", local = TRUE)


server <- function(input, output, session) {

    # -------------------------------------------------------------------------
    # 1. Reactive general para almacenar datos y columna de fecha
    # -------------------------------------------------------------------------
    rv <- reactiveValues(
        data = NULL,
        filtered_data = NULL,
        filtered_data_multi = NULL,  # Para Multivariate
        date_col = NULL             # Columna de fecha detectada
    )

    # -------------------------------------------------------------------------
    # 2. Carga de datos y detección de columna de fecha
    # -------------------------------------------------------------------------
    observeEvent(input$file, {
        notifyUser("Se ha seleccionado un archivo para cargar.", "message")

        data <- load_data(input$file)
        if (!is.null(data)) {

            # Detección de la columna de fecha (centralizado)
            date_col <- names(data)[tolower(names(data)) %in% c("period", "periodo")][1] # Toma la primera coincidencia
            if (!is.na(date_col)) {  #Verificar que se encontró la columna
                data[[date_col]] <- as.Date(data[[date_col]])  # Convertir a Date
                rv$date_col <- date_col   # Guardar el nombre de la columna
                # Establecer rango inicial para dateRangeInput
                updateDateRangeInput(session, "date_range_filter",
                                     start = min(data[[date_col]], na.rm = TRUE),
                                     end = max(data[[date_col]], na.rm = TRUE))
            } else {
                showNotification("No se pudo detectar una columna de fecha (Period/Periodo).", type = "warning")
            }

            # Inicializar datos
            rv$data <- data
            rv$filtered_data <- data  # Inicializar filtered_data

            # Detectar variables de medios y gasto
            numeric_cols <- names(data)[sapply(data, is.numeric)]
            # Modificación para detección de variables
            media_vars <- grep("(Impressions|Circulation|Clicks|Display|OOH|OLV|Magazine|Newspaper)",
                               names(data), value = TRUE, ignore.case = TRUE)
            spend_vars <- grep("(Cost|Spend|Costo)",
                               names(data), value = TRUE, ignore.case = TRUE)

            # Asegurarse de que sean numéricas
            media_vars <- intersect(media_vars, numeric_cols)
            spend_vars <- intersect(spend_vars, numeric_cols)

            # Actualizar selectInputs con selección predeterminada
            updateSelectInput(session, "media_vars",
                              choices = media_vars,
                              selected = media_vars) #Seleccionar todas
            updateSelectInput(session, "spend_vars",
                              choices = spend_vars,
                              selected = spend_vars)  #Seleccionar todas
        }
    })

    # -------------------------------------------------------------------------
    # 3. Actualización de filtros globales
    # -------------------------------------------------------------------------
    observe({
        req(rv$data)
        update_global_filters(rv$data, input, session)
    })

    # -------------------------------------------------------------------------
    # 4. Filtrado por fecha (común para ambos paneles)
    # -------------------------------------------------------------------------
    filter_data_by_date(input, rv) # Utiliza rv$date_col

    # -------------------------------------------------------------------------
    # 5. Filtrado exclusivo para el panel Multivariate
    # -------------------------------------------------------------------------
    rv$filtered_data_multi <- reactive({
        req(rv$filtered_data, input$geography_multi)  # Depende de filtered_data
        df <- rv$filtered_data
        date_col <- rv$date_col
        req(date_col)

        if (input$geography_multi != "Total" && input$geography_multi != "N/A") {
            geo_col <- if("Geography" %in% names(df)) "Geography" else if("Geografia" %in% names(df)) "Geografia" else NULL
            if (!is.null(geo_col)) {
                df <- df %>% filter(.data[[geo_col]] == input$geography_multi)
            }
        } else if (input$geography_multi == "Total") {
            numeric_cols <- names(df)[sapply(df, is.numeric)]
            # Asegurar que la columna de fecha sea de tipo Date antes de agrupar
            df[[date_col]] <- as.Date(df[[date_col]])
            df <- df %>%
                group_by(across(all_of(date_col))) %>%
                summarise(across(all_of(numeric_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
        }
        return(df)  # Devolver explícitamente
    })

    # -------------------------------------------------------------------------
    # 6. (a) Actualización del selectInput "variable_univ" según filtros compuestos
    # -------------------------------------------------------------------------
    observe({
        req(rv$data)
        # Suponiendo que "extract_metric_info" es una función que existe en el
        # código global y extrae info de product, campaign, outlet, creative...
        metric_info <- extract_metric_info(rv$data)
        if (!is.null(input$product_univ) && input$product_univ != "N/A") {
            metric_info <- metric_info %>% filter(product == input$product_univ)
        }
        if (!is.null(input$campaign_univ) && input$campaign_univ != "N/A") {
            metric_info <- metric_info %>% filter(campaign == input$campaign_univ)
        }
        if (!is.null(input$outlet_univ) && input$outlet_univ != "N/A") {
            metric_info <- metric_info %>% filter(outlet == input$outlet_univ)
        }
        if (!is.null(input$creative_univ) && input$creative_univ != "N/A") {
            metric_info <- metric_info %>% filter(creative == input$creative_univ)
        }
        choices <- if (!is.null(metric_info) && nrow(metric_info) > 0) metric_info$colname else character(0)
        if (length(choices) == 0) choices <- "N/A"
        updateSelectInput(session, "variable_univ", choices = choices)
    })

    # -------------------------------------------------------------------------
    # 6. (b) Actualización dinámica de los filtros compuestos para Univariate
    # -------------------------------------------------------------------------
    observe({
        req(rv$data)
        info <- extract_metric_info(rv$data)

        # Filtro "Product"
        info_for_product <- info
        if (!is.null(input$campaign_univ) && input$campaign_univ != "N/A") {
            info_for_product <- info_for_product[info_for_product$campaign == input$campaign_univ, ]
        }
        if (!is.null(input$outlet_univ) && input$outlet_univ != "N/A") {
            info_for_product <- info_for_product[info_for_product$outlet == input$outlet_univ, ]
        }
        if (!is.null(input$creative_univ) && input$creative_univ != "N/A") {
            info_for_product <- info_for_product[info_for_product$creative == input$creative_univ, ]
        }
        product_choices <- sort(unique(info_for_product$product))
        product_choices <- c("N/A", product_choices)

        # Filtro "Campaign"
        info_for_campaign <- info
        if (!is.null(input$product_univ) && input$product_univ != "N/A") {
            info_for_campaign <- info_for_campaign[info_for_campaign$product == input$product_univ, ]
        }
        if (!is.null(input$outlet_univ) && input$outlet_univ != "N/A") {
            info_for_campaign <- info_for_campaign[info_for_campaign$outlet == input$outlet_univ, ]
        }
        if (!is.null(input$creative_univ) && input$creative_univ != "N/A") {
            info_for_campaign <- info_for_campaign[info_for_campaign$creative == input$creative_univ, ]
        }
        campaign_choices <- sort(unique(info_for_campaign$campaign))
        campaign_choices <- c("N/A", campaign_choices)

        # Filtro "Outlet"
        info_for_outlet <- info
        if (!is.null(input$product_univ) && input$product_univ != "N/A") {
            info_for_outlet <- info_for_outlet[info_for_outlet$product == input$product_univ, ]
        }
        if (!is.null(input$campaign_univ) && input$campaign_univ != "N/A") {
            info_for_outlet <- info_for_outlet[info_for_outlet$campaign == input$campaign_univ, ]
        }
        if (!is.null(input$creative_univ) && input$creative_univ != "N/A") {
            info_for_outlet <- info_for_outlet[info_for_outlet$creative == input$creative_univ, ]
        }
        outlet_choices <- sort(unique(info_for_outlet$outlet))
        outlet_choices <- c("N/A", outlet_choices)

        # Filtro "Creative"
        info_for_creative <- info
        if (!is.null(input$product_univ) && input$product_univ != "N/A") {
            info_for_creative <- info_for_creative[info_for_creative$product == input$product_univ, ]
        }
        if (!is.null(input$campaign_univ) && input$campaign_univ != "N/A") {
            info_for_creative <- info_for_creative[info_for_creative$campaign == input$campaign_univ, ]
        }
        if (!is.null(input$outlet_univ) && input$outlet_univ != "N/A") {
            info_for_creative <- info_for_creative[info_for_creative$outlet == input$outlet_univ, ]
        }
        creative_choices <- sort(unique(info_for_creative$creative))
        creative_choices <- c("N/A", creative_choices)

        updateSelectInput(session, "product_univ", choices = product_choices,
                          selected = if (input$product_univ %in% product_choices) input$product_univ else "N/A")
        updateSelectInput(session, "campaign_univ", choices = campaign_choices,
                          selected = if (input$campaign_univ %in% campaign_choices) input$campaign_univ else "N/A")
        updateSelectInput(session, "outlet_univ", choices = outlet_choices,
                          selected = if (input$outlet_univ %in% outlet_choices) input$outlet_univ else "N/A")
        updateSelectInput(session, "creative_univ", choices = creative_choices,
                          selected = if (input$creative_univ %in% creative_choices) input$creative_univ else "N/A")
    })

    # -------------------------------------------------------------------------
    # 7. (a) Actualización de filtros compuestos para Multivariate
    # -------------------------------------------------------------------------
    observe({
        req(rv$data)
        info <- extract_metric_info(rv$data)

        info_multi <- info
        if (!is.null(input$campaign_multi) && input$campaign_multi != "N/A") {
            info_multi <- info_multi[info_multi$campaign == input$campaign_multi, ]
        }
        if (!is.null(input$outlet_multi) && input$outlet_multi != "N/A") {
            info_multi <- info_multi[info_multi$outlet == input$outlet_multi, ]
        }
        if (!is.null(input$creative_multi) && input$creative_multi != "N/A") {
            info_multi <- info_multi[info_multi$creative == input$creative_multi, ]
        }
        product_choices <- sort(unique(info_multi$product))
        product_choices <- c("N/A", product_choices)

        info_campaign_multi <- info
        if (!is.null(input$product_multi) && input$product_multi != "N/A") {
            info_campaign_multi <- info_campaign_multi[info_campaign_multi$product == input$product_multi, ]
        }
        if (!is.null(input$outlet_multi) && input$outlet_multi != "N/A") {
            info_campaign_multi <- info_campaign_multi[info_campaign_multi$outlet == input$outlet_multi, ]
        }
        if (!is.null(input$creative_multi) && input$creative_multi != "N/A") {
            info_campaign_multi <- info_campaign_multi[info_campaign_multi$creative == input$creative_multi, ]
        }
        campaign_choices <- sort(unique(info_campaign_multi$campaign))
        campaign_choices <- c("N/A", campaign_choices)

        info_outlet_multi <- info
        if (!is.null(input$product_multi) && input$product_multi != "N/A") {
            info_outlet_multi <- info_outlet_multi[info_outlet_multi$product == input$product_multi, ]
        }
        if (!is.null(input$campaign_multi) && input$campaign_multi != "N/A") {
            info_outlet_multi <- info_outlet_multi[info_outlet_multi$outlet == input$campaign_multi, ]
        }
        if (!is.null(input$creative_multi) && input$creative_multi != "N/A") {
            info_outlet_multi <- info_outlet_multi[info_outlet_multi$creative == input$creative_multi, ]
        }
        outlet_choices <- sort(unique(info_outlet_multi$outlet))
        outlet_choices <- c("N/A", outlet_choices)

        info_creative_multi <- info
        if (!is.null(input$product_multi) && input$product_multi != "N/A") {
            info_creative_multi <- info_creative_multi[info_creative_multi$product == input$product_multi, ]
        }
        if (!is.null(input$campaign_multi) && input$campaign_multi != "N/A") {
            info_creative_multi <- info_creative_multi[info_creative_multi$campaign == input$campaign_multi, ]
        }
        if (!is.null(input$outlet_multi) && input$outlet_multi != "N/A") {
            info_creative_multi <- info_creative_multi[info_creative_multi$outlet == input$outlet_multi, ]
        }
        creative_choices <- sort(unique(info_creative_multi$creative))
        creative_choices <- c("N/A", creative_choices)

        updateSelectInput(session, "product_multi", choices = product_choices,
                          selected = if (input$product_multi %in% product_choices) input$product_multi else "N/A")
        updateSelectInput(session, "campaign_multi", choices = campaign_choices,
                          selected = if (input$campaign_multi %in% campaign_choices) input$campaign_multi else "N/A")
        updateSelectInput(session, "outlet_multi", choices = outlet_choices,
                          selected = if (input$outlet_multi %in% outlet_choices) input$outlet_multi else "N/A")
        updateSelectInput(session, "creative_multi", choices = creative_choices,
                          selected = if (input$creative_multi %in% creative_choices) input$creative_multi else "N/A")
    })

    # (b) Actualización dinámica de la selección de variables para Multivariate
    observe({
        req(rv$data)
        metric_info <- extract_metric_info(rv$data)
        if (!is.null(input$product_multi) && input$product_multi != "N/A") {
            metric_info <- metric_info %>% filter(product == input$product_multi)
        }
        if (!is.null(input$campaign_multi) && input$campaign_multi != "N/A") {
            metric_info <- metric_info %>% filter(campaign == input$campaign_multi)
        }
        if (!is.null(input$outlet_multi) && input$outlet_multi != "N/A") {
            metric_info <- metric_info %>% filter(outlet == input$outlet_multi)
        }
        if (!is.null(input$creative_multi) && input$creative_multi != "N/A") {
            metric_info <- metric_info %>% filter(creative == input$creative_multi)
        }
        choices <- if (!is.null(metric_info) && nrow(metric_info) > 0) metric_info$colname else character(0)
        if (length(choices) == 0) choices <- "N/A"
        updateSelectInput(session, "var1_multi", choices = choices)
        updateSelectInput(session, "var2_multi", choices = choices)
        updateSelectInput(session, "var3_multi", choices = choices)
        updateSelectInput(session, "var4_multi", choices = c("None", choices))
    })

    # -------------------------------------------------------------------------
    # 8. Módulos: dimensiones, resumen e información del archivo
    # -------------------------------------------------------------------------
    dimensions_ui_module_server(input, output, session, rv)  # Sin cambios
    data_summary_module_server(input, output, session, rv)   # El módulo de la tabla resumen
    file_info_module_server(input, output, session, rv)      # Sin cambios

    # -------------------------------------------------------------------------
    # 9. Llamadas a módulos: Univariate y Multivariate
    # -------------------------------------------------------------------------
    univariate_module_server(input, output, session, rv)    # Usa rv$filtered_data
    multivariate_module_server(input, output, session, rv)  # Usa rv$filtered_data_multi

    # -------------------------------------------------------------------------
    # 10. Módulo de descarga
    # -------------------------------------------------------------------------
    download_handler_module(input, output, session, rv) # Sin cambios

} 
