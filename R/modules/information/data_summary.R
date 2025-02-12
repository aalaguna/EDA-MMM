# =============================================================================
# Módulo: data_summary
# Descripción: Renderiza la tabla resumen de los datos aplicando los cálculos y
# formateos solicitados. Para cada variable de medios se empareja su correspondiente
# variable de gasto, usando las palabras clave: Impressions, Clicks y Circulation.
# Adicionalmente, se busca la variable de gasto sustituyendo la palabra clave por 
# "Spend" o "Cost", de modo que se satisfaga el requerimiento de que las variables 
# de gasto pueden adoptar cualquiera de esos nombres.
#
# Lógica principal:
#   1. Para cada variable de medios seleccionada se identifica la palabra clave:
#      "Impressions", "Clicks" o "Circulation".
#   2. Se construyen uno o dos nombres de variable de gasto candidato(s) 
#      sustituyendo la palabra clave por "Spend" y por "Cost".
#   3. Se verifica si el/los candidatos existen en el dataset (insensible a mayúsc./minúsc.).
#   4. Se agregan los datos por Geography (y se cuentan las semanas en las que hubo
#      actividad/gasto) y se calculan los porcentajes y distribuciones para que 
#      sumen 100%.
#   5. Se calcula el CPC/CPM:
#      - Si la palabra clave es "Impressions" o "Circulation", se calcula CPM = (Spend / Activity) * 1000
#      - Si es "Clicks", se calcula CPC = (Spend / Activity)
#   6. Se clasifica la variable como "RAG" (misma actividad en todas las geografías) 
#      o "No-RAG" (actividad diferente por geografía).
#   7. Se devuelven las columnas finales con los nombres solicitados:
#
#      "Media Variable Name", "Spend Variable Name", "Type", "Geography",
#      "Activity", "Spend", "Activity Percentage", "Spend Percentage",
#      "# Weeks Activity", "Activity Distribution", "# Weeks Spend", 
#      "Spend Distribution", "CPC/CPM"
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)

data_summary_module_server <- function(input, output, session, rv) {
  
  # Función reactiva que genera la tabla consolidada
  consolidated_table <- reactive({
    # Se verifica que existan datos filtrados y variables de medios
    req(rv$filtered_data)
    req(input$media_vars)
    
    # Si no hay filas en los datos filtrados, retornamos mensaje
    if (nrow(rv$filtered_data) == 0) {
      return(data.frame(Message = "No hay datos disponibles."))
    }
    
    # Lista para almacenar los resultados por cada variable de medios
    results_list <- list()
    
    # Número total de semanas en el dataset (usando la columna de fecha detectada)
    total_weeks <- n_distinct(rv$filtered_data[[rv$date_col]])
    
    # Iterar sobre cada variable de medios seleccionada
    for (mvar in input$media_vars) {
      
      # Identificar la palabra clave en la variable de medios
      keyword <- NULL
      for (kw in c("Impressions", "Clicks", "Circulation")) {
        if (grepl(kw, mvar, ignore.case = TRUE)) {
          keyword <- kw
          break
        }
      }
      # Si no se encontró ninguna palabra clave, se omite la variable
      if (is.null(keyword)) next
      
      # Construir los nombres de la(s) variable(s) de gasto correspondiente(s)
      # Se buscan dos posibilidades: "Spend" y "Cost"
      possible_spend_candidates <- c(
        gsub(keyword, "Spend", mvar, ignore.case = TRUE),
        gsub(keyword, "Cost",  mvar, ignore.case = TRUE)
      )
      
      # Verificar cuál de los candidatos existe en el dataset
      spend_var <- NA
      for (candidate in possible_spend_candidates) {
        found <- names(rv$filtered_data)[
          tolower(names(rv$filtered_data)) == tolower(candidate)
        ]
        if (length(found) > 0) {
          spend_var <- found[1]
          break
        }
      }
      
      # Agregar datos de actividad (Media)
      media_agg <- rv$filtered_data %>%
        group_by(Geography) %>%
        summarise(
          Activity = sum(.data[[mvar]], na.rm = TRUE),
          Weeks_Activity = n_distinct(.data[[rv$date_col]][.data[[mvar]] > 0])
        ) %>%
        ungroup()
      
      # Agregar datos de gasto (Spend/Cost) si la variable existe
      if (!is.na(spend_var)) {
        spend_agg <- rv$filtered_data %>%
          group_by(Geography) %>%
          summarise(
            Spend = sum(.data[[spend_var]], na.rm = TRUE),
            Weeks_Spend = n_distinct(.data[[rv$date_col]][.data[[spend_var]] > 0])
          ) %>%
          ungroup()
      } else {
        # Si no se encontró la variable de gasto, se asignan valores NA
        spend_agg <- rv$filtered_data %>%
          distinct(Geography) %>%
          mutate(Spend = NA_real_, Weeks_Spend = NA_integer_)
      }
      
      # Unir las agregaciones por Geography
      merged_df <- left_join(media_agg, spend_agg, by = "Geography")
      
      # Calcular totales para los porcentajes (por variable de medios)
      total_activity <- sum(merged_df$Activity, na.rm = TRUE)
      total_spend    <- sum(merged_df$Spend, na.rm = TRUE)
      
      # Calcular porcentajes y distribuciones
      # Activity Percentage / Spend Percentage: % dentro de la variable de medios
      # Activity Distribution / Spend Distribution: % de semanas con actividad/spend
      merged_df <- merged_df %>%
        mutate(
          `Activity Percentage` = ifelse(total_activity > 0, round(Activity / total_activity * 100, 2), 0),
          `Spend Percentage`    = ifelse(total_spend > 0,    round(Spend / total_spend * 100, 2), 0),
          `Activity Distribution` = round(Weeks_Activity / total_weeks * 100, 2),
          `Spend Distribution`    = round(Weeks_Spend / total_weeks * 100, 2)
        )
      
      # Calcular CPC/CPM según la palabra clave (evitando división por cero)
      # Si Impressions o Circulation => CPM = (Spend / Activity) * 1000
      # Si Clicks => CPC = (Spend / Activity)
      merged_df <- merged_df %>%
        mutate(
          `CPC/CPM` = case_when(
            keyword %in% c("Impressions", "Circulation") & Activity > 0 ~ round(Spend / Activity * 1000, 2),
            keyword == "Clicks" & Activity > 0                          ~ round(Spend / Activity, 2),
            TRUE                                                        ~ NA_real_
          )
        )
      
      # Determinar el tipo: "RAG" si la actividad es idéntica en todas las geografías
      rag_type <- if (n_distinct(merged_df$Activity) == 1) "RAG" else "No-RAG"
      
      # Agregar columnas fijas y ordenar el dataframe según lo requerido
      merged_df <- merged_df %>%
        mutate(
          `Media Variable Name`  = mvar,
          `Spend Variable Name`  = ifelse(is.na(spend_var), "", spend_var),
          Type                   = rag_type
        ) %>%
        select(
          `Media Variable Name`, `Spend Variable Name`, Type, Geography, 
          Activity, Spend, `Activity Percentage`, `Spend Percentage`,
          Weeks_Activity, `Activity Distribution`, Weeks_Spend, 
          `Spend Distribution`, `CPC/CPM`
        )
      
      if (rag_type == "RAG") {
        
        merged_df <- merged_df %>% slice(1)
        
      }
      
      # Ajustamos los nombres de las columnas a la versión final solicitada
      colnames(merged_df) <- c(
        "Media Variable Name", 
        "Spend Variable Name", 
        "Type", 
        "Geography", 
        "Activity", 
        "Spend",
        "Activity Percentage", 
        "Spend Percentage", 
        "# Weeks Activity",
        "Activity Distribution",
        "# Weeks Spend",
        "Spend Distribution",
        "CPC/CPM"
      )
      
      results_list[[mvar]] <- merged_df
    } # Fin del for
    
    # Si no se obtuvo ningún resultado, se informa
    if (length(results_list) == 0) {
      return(data.frame(Message = "No se encontraron variables de medios con las palabras clave requeridas."))
    }
    
    # Combinar todas las tablas resultantes
    final_table <- bind_rows(results_list)
    
    
    # Procesamiento de la tabla final  
    final_table <- final_table %>%  
      # Paso 1: Calcular totales globales  
      summarise(  
        total_activity = sum(Activity, na.rm = TRUE),  
        total_spend = sum(Spend, na.rm = TRUE),  
        .groups = 'drop'  # Evitar que se mantenga el agrupamiento  
      ) %>%  
      # Paso 2: Unir los totales globales con la tabla original  
      right_join(final_table, by = character()) %>%  
      
      # Paso 3: Agrupar por 'Media Variable Name' para cálculos locales  
      group_by(`Media Variable Name`) %>%  
      mutate(  
        # Calcular el porcentaje global de Activity  
        `Activity Percentage` = ifelse(  
          total_activity > 0,  
          round(Activity / total_activity * 100, 2),   
          0  
        ),  
        # Calcular el porcentaje global de Spend  
        `Spend Percentage` = ifelse(  
          total_spend > 0,  
          round(Spend / total_spend * 100, 2),   
          0  
        )  
      ) %>%  
      # Paso 4: Desagrupar para evitar efectos en pasos posteriores  
      ungroup() %>%  
      # Paso 5: Ordenar la tabla por 'Media Variable Name' y 'Geography'  
      arrange(`Media Variable Name`, Geography)  
    
    # Devolver la tabla final procesada  
    return(final_table)})
    
  # Renderizar la tabla en la interfaz de usuario
  output$consolidated_table <- renderTable({
    table_data <- consolidated_table()
    
    # Si se retornó un mensaje, se muestra directamente
    if ("Message" %in% names(table_data)) {
      return(table_data)
    }
    
    # Formatear numéricamente las columnas para una mejor visualización
    table_data <- table_data %>%
      mutate(
        across(where(is.numeric),
               ~ format(
                 .,
                 big.mark = ",",
                 decimal.mark = ".",
                 nsmall = 2,
                 scientific = FALSE
               )
        )
      )
    
    return(table_data)
  }, striped = TRUE, bordered = TRUE, spacing = 'xs', align = 'l')
  
  # Configurar descarga de la tabla consolidada
  output$download_consolidated <- downloadHandler(
    filename = function() {
      paste("Summary_Table_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(consolidated_table(), file, row.names = FALSE)
    }
  )
  
  # Retornar la tabla final por si se necesita en otras partes
  return(consolidated_table)
}
