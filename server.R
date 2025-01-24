# 5 server.R
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
# Fuentes externas (ajusta las rutas si es necesario)
source("EDA/Scripts/EDA Functions.R")          # s_curve_transform, etc.
source("EDA/Scripts/S-Curve EDA Combination.R") # create_flighting_chart + create_s_curve_chart

server <- function(input, output, session) {
  
  # ReactiveValues para almacenar datos
  rv <- reactiveValues(
    data = NULL,
    filtered_data = NULL
  )
  
  
  # 0. Temporal and Cross Sectional Dimension -------------------------------
  
  # Mostrar el nombre de Temporal Dimension como texto (solo muestra Period si existe)
  output$temporal_dimension_ui <- renderUI({
    req(rv$data)
    
    # Detectar si existe una columna llamada "Period" 
    temporal_columns <- names(rv$data)[tolower(names(rv$data)) == "period"]
    
    if (length(temporal_columns) > 0) {
      # Mostrar "Period" si está presente
      div("Period")
    } else {
      # Mensaje si no se encuentra la columna Period
      div("No temporal dimension found in the dataset.")
    }
  })
  
  output$cross_sectional_dimension_ui <- renderUI({
    req(rv$data)
    
    # Detectar palabras claves 
    keywords <- c("Geography", "Product", "Campaign", "Outlet", "Creative")
    
    # Filtrar columnas 
    available_columns <- names(rv$data) %>% 
      .[. %in% keywords]
    
    if(length(available_columns) > 0){
      div(
        paste(available_columns, collapse = ", ")
      )
    } else{
      div("No cross sectional dimension found in the dataset.")
    }
  })
  
  # 1. Carga de datos ---------------------------------------
  loaded_data <- reactive({
    req(input$file)
    file <- input$file
    ext  <- tools::file_ext(file$name)
    
    tryCatch({
      if(ext == "csv"){
        data <- read.csv(file$datapath, stringsAsFactors = FALSE)
      } else if(ext == "RData"){
        env <- new.env()
        load(file$datapath, envir = env)
        objs <- ls(env)
        data_objs <- objs[sapply(objs, function(x) is.data.frame(get(x, envir = env)))]
        if(length(data_objs) == 0){
          stop("El archivo .RData no contiene ningún data.frame.")
        }
        data <- get(data_objs[1], envir = env)
        if(!is.data.frame(data)){
          stop("El objeto seleccionado no es un data.frame.")
        }
      } else {
        stop("Formato de archivo no soportado. Por favor, suba un archivo .csv o .RData.")
      }
      return(data)
    }, error = function(e){
      showNotification(paste("Error al cargar el archivo:", e$message),
                       type = "error")
      return(NULL)
    })
  })
  
  # # Observador que asigna rv$data cuando loaded_data() cambia

  # # Observador que asigna rv$data cuando loaded_data() cambia
  # observeEvent(loaded_data(), {
  #   rv$data <- loaded_data()
  #   req(rv$data)
  #   
  #   # Convertir columnas de fecha (Period o periodo) a Date
  #   if ("Period" %in% names(rv$data)) {
  #     rv$data$Period <- as.Date(rv$data$Period)
  #   }
  #   if ("periodo" %in% names(rv$data)) {
  #     rv$data$periodo <- as.Date(rv$data$periodo)
  #   }
  #   
  #   # Actualizar selects de variables
  #   numeric_cols <- names(rv$data)[sapply(rv$data, is.numeric)]
  #   
  #   # Identificar media y spend
  #   MEDIA_VARIABLES <- grep("(Impressions)|(Circulation)|(Clicks)|(Display)|(OOH)|(OLV)|(Magazine)|(Newspaper)",
  #                           names(rv$data), value = TRUE)
  #   SPEND_VARIABLES <- grep("(Cost)|(Spend)", MEDIA_VARIABLES, value = TRUE)
  #   MEDIA_VARIABLES <- setdiff(MEDIA_VARIABLES, SPEND_VARIABLES)
  #   MEDIA_VARIABLES <- intersect(MEDIA_VARIABLES, numeric_cols)
  #   SPEND_VARIABLES <- intersect(SPEND_VARIABLES, numeric_cols)
  #   
  #   # Actualizar inputs de "Data Management" y seleccionar automáticamente todas las variables de media
  #   updateSelectInput(session, "kpi", choices = numeric_cols)
  #   updateSelectInput(session, "media_vars", choices = MEDIA_VARIABLES, selected = MEDIA_VARIABLES)
  #   updateSelectInput(session, "spend_vars", choices = SPEND_VARIABLES, selected = NULL)
  #   updateSelectInput(session, "base_vars", choices = numeric_cols)
  #   
  #   updateSelectInput(session, "kpi_univ", choices = numeric_cols)
  #   updateSelectInput(session, "variable_univ", choices = numeric_cols)
  #   updateSelectInput(session, "kpi_multi", choices = numeric_cols)
  #   updateSelectInput(session, "var1_multi", choices = numeric_cols)
  #   updateSelectInput(session, "var2_multi", choices = numeric_cols)
  #   updateSelectInput(session, "var3_multi", choices = numeric_cols)
  #   updateSelectInput(session, "var4_multi", choices = c("None", numeric_cols))
  # })
  # Observador que asigna rv$data cuando loaded_data() cambia
  observeEvent(loaded_data(), {
    rv$data <- loaded_data()
    req(rv$data)
    
    # Convertir columnas de fecha (Period o periodo) a Date
    if ("Period" %in% names(rv$data)) {
      rv$data$Period <- as.Date(rv$data$Period)
    }
    if ("periodo" %in% names(rv$data)) {
      rv$data$periodo <- as.Date(rv$data$periodo)
    }
    
    # Actualizar selects de variables
    numeric_cols <- names(rv$data)[sapply(rv$data, is.numeric)]
    
    # Identificar media y spend
    MEDIA_VARIABLES <- grep("(Impressions|Circulation|Clicks|Display|OOH|OLV|Magazine|Newspaper|Social)", 
                            names(rv$data), value = TRUE)
    SPEND_VARIABLES <- grep("(Spend|Cost)", names(rv$data), value = TRUE)
    
    MEDIA_VARIABLES <- intersect(MEDIA_VARIABLES, numeric_cols)
    SPEND_VARIABLES <- intersect(SPEND_VARIABLES, numeric_cols)
    
    # Seleccionar automáticamente todas las variables de Media y Spend
    updateSelectInput(session, "media_vars", choices = MEDIA_VARIABLES, selected = MEDIA_VARIABLES)
    updateSelectInput(session, "spend_vars", choices = SPEND_VARIABLES, selected = SPEND_VARIABLES)
  })
  
  # 2. Observador para filtrar datos según la fecha seleccionada -----------
  observe({
    req(rv$data)
    df <- rv$data
    
    # Checar columna de fecha
    date_col <- NULL
    if ("Period" %in% names(df)) {
      date_col <- "Period"
    } else if ("periodo" %in% names(df)) {
      date_col <- "periodo"
    }
    
    if (!is.null(date_col)) {
      selected_range <- input$date_range_filter
      if (!is.null(selected_range)) {
        df <- df %>%
          filter(as.Date(.data[[date_col]]) >= as.Date(selected_range[1]) &
                   as.Date(.data[[date_col]]) <= as.Date(selected_range[2]))
      }
    }
    
    rv$filtered_data <- df
  })
  
  
  # 3. Filtros Globales -----------------------------------------------------
  
  # Bloque para el filtro de Geography
  observe({
    req(rv$data)  
    geography_column <- "Geography"  
    
    # Actualizar las opciones del filtro Geography basado en los datos cargados
    if (geography_column %in% names(rv$data)) {
      updateSelectInput(session, "geography_univ",
                        choices = unique(rv$data[[geography_column]]),
                        selected = unique(rv$data[[geography_column]])[1])
    } else {
      showNotification("The 'Geography' column was not found in the dataset.", type = "warning")
    }
  })
  
  # Filtrado de los datos basado en la selección de Geography
  filtered_geography_data <- reactive({
    req(rv$data, input$geography_univ)  # Ensure data and input exist
    rv$data %>% filter(Geography == input$geography_univ)
  })
  
  # Observador para validar el filtrado (puedes usarlo para depuración o mensajes)
  observe({
    req(filtered_geography_data())
    if (nrow(filtered_geography_data()) == 0) {
      showNotification("No data available for the selected geography.", type = "warning")
    } else {
      showNotification("Data successfully filtered by Geography.", type = "message")
    }
  })
  
  
  # # INFORMATION TAB ---------------------------------------------------------
 
  consolidated_table <- reactive({
    req(rv$filtered_data, input$media_vars) # Asegúrate de que los datos y el input existan
    
    # Variables seleccionadas
    media_vars <- input$media_vars
    
    # Verificar si hay variables seleccionadas
    if (length(media_vars) == 0) {
      return(data.frame()) # Devuelve una tabla vacía si no hay variables seleccionadas
    }
    
    # Datos en formato largo para las variables seleccionadas
    data_long <- rv$filtered_data %>%
      pivot_longer(
        cols = all_of(media_vars),
        names_to = "VariableName",
        values_to = "VariableValue"
      ) %>%
      filter(VariableValue > 0) # Filtrar valores mayores a 0
    
    # Calcular actividad y clasificar RAG/No-RAG
    rag_mapping <- data_long %>%
      group_by(Geography, VariableName) %>%
      summarise(Activity = sum(VariableValue, na.rm = TRUE), .groups = "drop") %>%
      group_by(VariableName) %>%
      mutate(Type = ifelse(length(unique(Activity)) == 1, "RAG", "No-RAG")) %>%
      ungroup()
    
    # Filtrar primeras geografías para RAG
    rag_mapping <- rag_mapping %>%
      group_by(VariableName) %>%
      filter(!(Type == "RAG" & row_number() > 1)) %>%
      ungroup()
    
    # Relacionar Variable Spend
    rag_mapping <- rag_mapping %>%
      mutate(
        `VariableSpend` = map_chr(
          VariableName,
          ~ {
            spend_candidate <- gsub("Circulation|Impressions|Clicks", "Spend", .x)
            if (spend_candidate %in% names(rv$filtered_data)) {
              return(spend_candidate)
            } else {
              return(NA_character_)
            }
          }
        )
      )
    
    # Agregar métricas relacionadas con Spend
    rag_mapping %>%
      mutate(
        Spend = map2_dbl(
          Geography, `VariableSpend`,
          ~ {
            if (!is.na(.y) && .y %in% names(rv$filtered_data) && !is.na(.x)) {
              filtered_data <- rv$filtered_data %>% filter(Geography == .x)
              if (nrow(filtered_data) > 0) {
                return(sum(filtered_data[[.y]], na.rm = TRUE))
              } else {
                return(NA_real_)
              }
            } else {
              return(NA_real_)
            }
          }
        ),
        `Activity Percentage` = round((Activity / sum(Activity, na.rm = TRUE)) * 100, 2),
        `Spend Percentage` = round((Spend / sum(Spend, na.rm = TRUE)) * 100, 2),
        `Activity # weeks` = map_dbl(VariableName, ~ sum(rv$filtered_data[[.x]] > 0, na.rm = TRUE)),
        `Activity distribution` = map_dbl(
          VariableName, 
          ~ round((sum(rv$filtered_data[[.x]] > 0, na.rm = TRUE) / nrow(rv$filtered_data)) * 100, 2)
        ),
        `Spend # weeks` = map2_dbl(
          Geography, `VariableSpend`,
          ~ {
            if (!is.na(.y) && .y %in% names(rv$filtered_data) && !is.na(.x)) {
              filtered_data <- rv$filtered_data %>% filter(Geography == .x)
              if (nrow(filtered_data) > 0) {
                return(sum(filtered_data[[.y]] > 0, na.rm = TRUE))
              } else {
                return(NA_real_)
              }
            } else {
              return(NA_real_)
            }
          }
        ),
        `Spend Distribution` = map2_dbl(
          Geography, `VariableSpend`,
          ~ {
            if (!is.na(.y) && .y %in% names(rv$filtered_data) && !is.na(.x)) {
              filtered_data <- rv$filtered_data %>% filter(Geography == .x)
              if (nrow(filtered_data) > 0) {
                return(round((sum(filtered_data[[.y]] > 0, na.rm = TRUE) / nrow(rv$filtered_data)) * 100, 2))
              } else {
                return(NA_real_)
              }
            } else {
              return(NA_real_)
            }
          }
        ),
        `CPC/CPM` = ifelse(
          grepl("Impressions", VariableName, ignore.case = TRUE),
          ifelse(Activity > 0, round((Spend / Activity) * 1000, 2), NA),
          ifelse(Activity > 0, round((Spend / Activity), 2), NA)
        )
      ) %>%
      mutate(across(where(is.numeric), ~ formatC(., format = "f", big.mark = ",", digits = 2))) %>%
      select(
        VariableName, VariableSpend, Geography, Type, Activity, Spend, 
        `Activity Percentage`, `Spend Percentage`, `Activity # weeks`, 
        `Activity distribution`, `Spend # weeks`, `Spend Distribution`, `CPC/CPM`
      ) %>%
      arrange(Type, Geography)
  })
  
  # Renderizar la tabla con un tamaño más pequeño para las letras
  output$consolidated_table <- renderTable({
    consolidated_table()
  }, striped = TRUE, bordered = TRUE, hover = TRUE, align = 'l', 
  spacing = "xs") # Esto ajusta el espaciado y reduce visualmente la tabla
  
  
  # UNIVARIATE TAB ----------------------------------------------------------
  
  
  # 1. Variable Flighting
  output$variable_flighting_chart <- renderPlotly({
    req(filtered_geography_data(), input$kpi_univ, input$variable_univ)
    
    # Identificar la columna de fecha
    date_col <- if ("Period" %in% names(filtered_geography_data())) {
      "Period"
    } else if ("periodo" %in% names(filtered_geography_data())) {
      "periodo"
    } else {
      NULL
    }
    req(date_col)  # Asegúrate de que existe la columna de fecha
    
    # Filtrar las columnas necesarias
    data_to_plot <- filtered_geography_data() %>%
      select(!!sym(date_col), KPI = !!sym(input$kpi_univ), Variable = !!sym(input$variable_univ))
    
    # Verificar si hay datos suficientes
    validate(
      need(nrow(data_to_plot) > 0, "No data available to plot.")
    )
    
    # Crear el gráfico con dos ejes Y
    plot_ly(data_to_plot) %>%
      add_lines(x = ~get(date_col), y = ~KPI, name = "KPI", line = list(color = 'blue')) %>%
      add_lines(x = ~get(date_col), y = ~Variable, name = "Variable", yaxis = "y2", line = list(color = 'red')) %>%
      layout(
        title = paste("KPI and Variable Over Time for Geography:", input$geography_univ),
        xaxis = list(title = "Time"),
        yaxis = list(title = "KPI", side = "left"),
        yaxis2 = list(title = "Variable", overlaying = "y", side = "right"),
        legend = list(orientation = "h", x = 0.3, y = -0.2)
      )
  })
  
  # 2. Variable Trans
  output$var_transf_chart <- renderPlotly({
    req(filtered_geography_data(), input$variable_univ, input$transformation_univ)
    
    var_name <- input$variable_univ
    
    # Transformación Lineal o Aplicación de Transformaciones
    if (input$transformation_univ == "Linear") {
      transformed_data <- filtered_geography_data()[[var_name]]
    } else {
      transformed_data <- apply_transformation(
        filtered_geography_data()[[var_name]],
        type   = input$transformation_univ,
        alpha  = input$alpha_univ,
        beta   = input$beta_univ,
        maxval = input$maxval_univ,
        decay  = input$decay_univ,
        lag    = input$lag_univ
      )
    }
    
    # Crear un dataframe con los datos transformados
    df_trans <- filtered_geography_data() %>%
      mutate(Transformed = transformed_data)
    
    # Identificar la columna de fecha
    date_col <- if ("Period" %in% names(filtered_geography_data())) "Period" else "periodo"
    req(date_col)
    
    # Crear el gráfico
    p <- ggplot(df_trans, aes_string(x = date_col, y = "Transformed")) +
      geom_line(color = "red") +
      theme_minimal() +
      labs(
        title = paste("Transformed Variable for Geography:", input$geography_univ),
        x = "Time",
        y = "Transformed Value"
      )
    
    ggplotly(p)
  })
  
  
  # 3. Boxplot 
  output$boxplot_univ <- renderPlot({
    req(filtered_geography_data(), input$variable_univ)
    
    box_data <- filtered_geography_data()[[input$variable_univ]]
    box_data <- box_data[!is.na(box_data)]
    
    if (length(box_data) < 1) {
      showNotification("Not enough data available for the boxplot.", type = "error")
      return(NULL)
    }
    
    ggplot(data.frame(val = box_data), aes(x = "", y = val)) +
      geom_boxplot(fill = "skyblue") +
      theme_minimal() +
      labs(
        title = paste("Boxplot for Geography:", input$geography_univ),
        x = "",
        y = input$variable_univ
      )
  })
  
  # 4. Texto con transformaciones aplicadas
  output$transformations_summary_univ <- renderPrint({
    req(input$transformation_univ, filtered_geography_data(), input$variable_univ)
    
    # Calcular el valor máximo para el boxplot
    box_data <- filtered_geography_data()[[input$variable_univ]]
    box_max <- max(box_data, na.rm = TRUE)
    
    # Mostrar información
    cat("Selected Transformation:", input$transformation_univ, "\n")
    if (input$transformation_univ %in% c("S Origin", "S Shaped")) {
      cat("Alpha:", input$alpha_univ, "\n")
      cat("Beta:", input$beta_univ, "\n")
      cat("MaxVal%:", input$maxval_univ, "\n")
    }
    cat("Decay:", input$decay_univ, "\n")
    cat("Lag:", input$lag_univ, "\n")
    cat("Max Value (Boxplot):", box_max, "\n")
    cat("Geography Selected:", input$geography_univ, "\n")
  })
  
  
  # 5. Curva S condicional (solo si "S Origin")
  output$s_curve_univariate_plot <- renderPlotly({
    req(filtered_geography_data(), input$variable_univ)
    validate(
      need(input$transformation_univ == "S Origin",
           "This plot is only displayed for 'S Origin'.")
    )
    
    var_name <- input$variable_univ
    alpha <- input$alpha_univ
    beta <- input$beta_univ
    max_val_pct <- input$maxval_univ
    decay <- input$decay_univ
    lag <- input$lag_univ
    
    df_scurve <- filtered_geography_data() %>%
      mutate(Period = if ("Period" %in% names(.)) as.Date(Period)
             else if ("periodo" %in% names(.)) as.Date(periodo)
             else as.Date(NA)) %>%
      select(Period, value = !!sym(var_name)) %>%
      filter(!is.na(Period))
    
    if (nrow(df_scurve) == 0) {
      showNotification("No available data for the S-Curve.", type = "error")
      return(NULL)
    }
    
    flighting_plot_gg <- tryCatch({
      create_flighting_chart(
        data_chart  = df_scurve,
        alpha       = alpha,
        beta        = beta,
        max_val_pct = max_val_pct,
        decay       = decay,
        lag         = lag,
        var_name    = var_name
      )
    }, error = function(e) {
      showNotification(paste("Error in Flighting Chart:", e$message), type = "error")
      return(NULL)
    })
    
    s_curve_plot_gg <- tryCatch({
      create_s_curve_chart(
        data_chart  = df_scurve,
        alpha       = alpha,
        beta        = beta,
        max_val_pct = max_val_pct,
        decay       = decay,
        lag         = lag,
        var_name    = var_name
      )
    }, error = function(e) {
      showNotification(paste("Error in S-Curve Chart:", e$message), type = "error")
      return(NULL)
    })
    
    if (is.null(flighting_plot_gg) || is.null(s_curve_plot_gg)) {
      return(NULL)
    }
    
    subplot(flighting_plot_gg, s_curve_plot_gg, nrows = 1,
            titleX = TRUE, titleY = TRUE) %>%
      layout(title = "S-Curve EDA")
  })
  
  
  # MULTIVARIATE TAB --------------------------------------------------------
  
  
  output$variables_chart_multi <- renderPlot({
    req(rv$filtered_data, input$var1_multi, input$var2_multi, input$var3_multi)
    
    data_chart <- rv$filtered_data
    date_col <- if("Period" %in% names(data_chart)) "Period" else "periodo"
    
    vars_to_select <- c(input$var1_multi, input$var2_multi, input$var3_multi)
    if(input$var4_multi != "None"){
      vars_to_select <- c(vars_to_select, input$var4_multi)
    }
    
    plot_data <- data_chart %>%
      select(all_of(vars_to_select), !!sym(date_col)) %>%
      pivot_longer(cols = all_of(vars_to_select),
                   names_to = "variable",
                   values_to = "value")
    
    # Si sum_all_vars == "true", transformamos cada variable
    if(input$sum_all_vars == "true"){
      for(var_i in unique(plot_data$variable)) {
        
        # Determinamos el tipo de transformación según la variable actual
        trans_type <- if(var_i == input$var1_multi){
          input$trans_var1
        } else if(var_i == input$var2_multi){
          input$trans_var2
        } else if(var_i == input$var3_multi){
          input$trans_var3
        } else if(var_i == input$var4_multi){
          input$trans_var4
        } else {
          "Linear"
        }
        
        idx <- plot_data$variable == var_i
        plot_data$value[idx] <- apply_transformation(
          plot_data$value[idx],
          trans_type,
          alpha  = input$alpha_multi,
          beta   = input$beta_multi,
          maxval = input$maxval_multi,
          decay  = input$decay_multi,
          lag    = input$lag_multi
        )
      }
    }
    
    ggplot(plot_data, aes_string(x = date_col, y = "value", color = "variable")) +
      geom_line() +
      theme_minimal() +
      labs(title = "Multiple Variables over Time",
           x = "Period",
           y = "Value") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$boxplot_multi <- renderPlot({
    req(rv$filtered_data, input$var1_multi, input$var2_multi, input$var3_multi)
    
    vars_to_select <- c(input$var1_multi, input$var2_multi, input$var3_multi)
    if(input$var4_multi != "None"){
      vars_to_select <- c(vars_to_select, input$var4_multi)
    }
    
    plot_data <- rv$filtered_data %>%
      select(all_of(vars_to_select)) %>%
      pivot_longer(cols = everything(),
                   names_to = "variable",
                   values_to = "value") %>%
      filter(!is.na(value))
    
    ggplot(plot_data, aes(x = variable, y = as.numeric(value))) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = "Distribution of Variables", x = "Variable", y = "Value") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$corr_matrix_multi <- renderPlot({
    req(rv$filtered_data, input$var1_multi, input$var2_multi, input$var3_multi)
    
    vars_to_correlate <- c(input$var1_multi, input$var2_multi, input$var3_multi)
    if(input$var4_multi != "None"){
      vars_to_correlate <- c(vars_to_correlate, input$var4_multi)
    }
    vars_to_correlate <- vars_to_correlate[vars_to_correlate != "None"]
    vars_to_correlate <- vars_to_correlate[vars_to_correlate %in% names(rv$filtered_data)]
    
    if(length(vars_to_correlate) < 2){
      showNotification("Se requieren al menos dos variables para la matriz de correlación.", type = "error")
      return(NULL)
    }
    
    cor_data <- rv$filtered_data %>%
      select(all_of(vars_to_correlate)) %>%
      mutate(across(everything(), as.numeric))
    
    # Eliminar columnas con varianza cero
    cor_data <- cor_data %>%
      select(where(~ sd(.) > 0))
    
    if(ncol(cor_data) < 2){
      showNotification("No hay suficientes variables con varianza positiva para la matriz de correlación.", type = "error")
      return(NULL)
    }
    if(nrow(cor_data) < 2){
      showNotification("No hay suficientes datos para la matriz de correlación.", type = "error")
      return(NULL)
    }
    
    cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")
    if(any(is.na(cor_matrix))){
      showNotification("La matriz de correlación contiene valores NA.", type = "warning")
    }
    
    corrplot(cor_matrix, method = "color",
             type = "upper",
             addCoef.col = "black",
             tl.col = "black",
             tl.srt = 45,
             diag = FALSE,
             number.cex = 0.7)
  })
  
  # S-Curve Multivariado (condicional)
  output$s_curve_multivariate_plot <- renderPlotly({
    req(rv$filtered_data, input$var1_multi, input$var2_multi, input$var3_multi)
    
    vars_to_sum <- c(input$var1_multi, input$var2_multi, input$var3_multi)
    if(input$var4_multi != "None"){
      vars_to_sum <- c(vars_to_sum, input$var4_multi)
    }
    
    # Aseguramos que existan en filtered_data
    available_vars <- vars_to_sum[vars_to_sum %in% names(rv$filtered_data)]
    if(length(available_vars) == 0){
      showNotification("No hay variables para la S-Curve Multivariada.", type = "error")
      return(NULL)
    }
    
    sum_variable <- rowSums(rv$filtered_data[available_vars], na.rm = TRUE)
    
    df_scurve_multi <- rv$filtered_data %>%
      mutate(Period = if("Period" %in% names(.)) as.Date(Period)
             else if("periodo" %in% names(.)) as.Date(periodo)
             else as.Date(NA)) %>%
      mutate(value = sum_variable) %>%
      select(Period, value) %>%
      filter(!is.na(Period))
    
    if(nrow(df_scurve_multi) == 0){
      showNotification("No hay datos disponibles para la S-Curve Multivariada.", type = "error")
      return(NULL)
    }
    
    var_name    <- "Sum of Selected Variables"
    alpha       <- input$alpha_multi
    beta        <- input$beta_multi
    max_val_pct <- input$maxval_multi
    decay       <- input$decay_multi
    lag         <- input$lag_multi
    
    flighting_plot_gg <- tryCatch({
      create_flighting_chart(
        data_chart  = df_scurve_multi,
        alpha       = alpha,
        beta        = beta,
        max_val_pct = max_val_pct,
        decay       = decay,
        lag         = lag,
        var_name    = var_name
      )
    }, error = function(e){
      showNotification(paste("Error en Flighting Chart Multivariado:", e$message),
                       type = "error")
      return(NULL)
    })
    
    s_curve_plot_gg <- tryCatch({
      create_s_curve_chart(
        data_chart  = df_scurve_multi,
        alpha       = alpha,
        beta        = beta,
        max_val_pct = max_val_pct,
        decay       = decay,
        lag         = lag,
        var_name    = var_name
      )
    }, error = function(e){
      showNotification(paste("Error en S-Curve Chart Multivariado:", e$message),
                       type = "error")
      return(NULL)
    })
    
    if(is.null(flighting_plot_gg) || is.null(s_curve_plot_gg)) {
      return(NULL)
    }
    
    subplot(flighting_plot_gg, s_curve_plot_gg,
            nrows = 1, titleX = TRUE, titleY = TRUE) %>%
      layout(title = "S-Curve EDA Multivariado")
  })
  
  
  # DOWNLOAD FILTERED ANALYTICAL --------------------------------------------
  
  
  output$download_analytical <- downloadHandler(
    filename = function(){ paste0("analytical_transformed_", Sys.Date(), ".csv") },
    content = function(file){
      download_data <- rv$filtered_data
      
      # Transformación univar
      if(!is.null(input$variable_univ)){
        newcol <- paste0(input$variable_univ, "_transformed")
        download_data[[newcol]] <- apply_transformation(
          rv$filtered_data[[input$variable_univ]],
          type   = input$transformation_univ,
          alpha  = input$alpha_univ,
          beta   = input$beta_univ,
          maxval = input$maxval_univ,
          decay  = input$decay_univ,
          lag    = input$lag_univ
        )
      }
      
      # Transformaciones multivariables si sum_all_vars == "true"
      if(input$sum_all_vars == "true"){
        vars_to_transform <- c(input$var1_multi, input$var2_multi, input$var3_multi)
        if(!is.null(input$var4_multi) && input$var4_multi != "None"){
          vars_to_transform <- c(vars_to_transform, input$var4_multi)
        }
        
        for(var_i in vars_to_transform){
          trans_type <- if(var_i == input$var1_multi){
            input$trans_var1
          } else if(var_i == input$var2_multi){
            input$trans_var2
          } else if(var_i == input$var3_multi){
            input$trans_var3
          } else if(var_i == input$var4_multi){
            input$trans_var4
          } else {
            "Linear"
          }
          
          newcol <- paste0(var_i, "_transformed")
          download_data[[newcol]] <- apply_transformation(
            rv$filtered_data[[var_i]],
            trans_type,
            alpha  = input$alpha_multi,
            beta   = input$beta_multi,
            maxval = input$maxval_multi,
            decay  = input$decay_multi,
            lag    = input$lag_multi
          )
        }
      }
      
      write.csv(download_data, file, row.names = FALSE, na = "")
    }
  )
}