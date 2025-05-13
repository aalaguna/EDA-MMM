# Data_Summary.R

# =============================================================================
# Renders the data summary table by applying the required calculations 
# and formatting. For each media variable, the corresponding cost variable 
# is found using keywords: Impressions, Clicks, Circulation, etc.
# =============================================================================

data_summary_module_server <- function(input, output, session, rv) {
  # Server module for the data summary table
  #
  # Args:
  #   input: Shiny input object
  #   output: Shiny output object
  #   session: Shiny session object
  #   rv: Shared reactive values
  
  # Reactive function that generates the consolidated table
  consolidated_table <- reactive({
    # Requires filtered data and selected media variables
    req(rv$filtered_data)
    req(input$media_vars)
    
    if (nrow(rv$filtered_data) == 0) {
      return(data.frame(Message = "No data available."))
    }
    
    results_list <- list()
    df <- rv$filtered_data
    
    # Try to detect date column
    date_col <- rv$date_col
    total_weeks <- if (!is.null(date_col) && date_col %in% names(df)) {
      n_distinct(df[[date_col]])
    } else {
      NA
    }
    
    # For each media variable
    for (mvar in input$media_vars) {
      # Identify keyword
      keyword <- NULL
      for (kw in c("Impressions", "Clicks", "Circulation")) {
        if (grepl(kw, mvar, ignore.case = TRUE)) {
          keyword <- kw
          break
        }
      }
      if (is.null(keyword)) next
      
      # Possible cost variable
      possible_spend_candidates <- c(
        gsub(keyword, "Spend", mvar, ignore.case = TRUE),
        gsub(keyword, "Cost", mvar, ignore.case = TRUE)
      )
      spend_var <- NA
      for (candidate in possible_spend_candidates) {
        found <- names(df)[tolower(names(df)) == tolower(candidate)]
        if (length(found) > 0) {
          spend_var <- found[1]
          break
        }
      }
      
      # Media aggregation
      media_agg <- df %>%
        group_by(Geography) %>%
        summarise(
          Activity = sum(.data[[mvar]], na.rm = TRUE),
          Weeks_Activity = if (!is.na(total_weeks)) {
            n_distinct(.data[[date_col]][.data[[mvar]] > 0])
          } else { NA }
        ) %>%
        ungroup()
      
      # Cost aggregation if found
      if (!is.na(spend_var)) {
        spend_agg <- df %>%
          group_by(Geography) %>%
          summarise(
            Spend = sum(.data[[spend_var]], na.rm = TRUE),
            Weeks_Spend = if (!is.na(total_weeks)) {
              n_distinct(.data[[date_col]][.data[[spend_var]] > 0])
            } else { NA }
          ) %>%
          ungroup()
      } else {
        spend_agg <- df %>%
          distinct(Geography) %>%
          mutate(Spend = NA_real_, Weeks_Spend = NA_integer_)
      }
      
      # Merge
      merged_df <- left_join(media_agg, spend_agg, by = "Geography")
      

      # Summary
      total_activity <- sum(merged_df$Activity, na.rm = TRUE)
      total_spend    <- sum(merged_df$Spend, na.rm = TRUE)
      
      merged_df <- merged_df %>%
        mutate(
          `Activity %` = ifelse(total_activity > 0, round(Activity / total_activity * 100, 2), 0),
          `Spend %`    = ifelse(total_spend > 0,    round(Spend / total_spend * 100, 2), 0),
          `Activity Distribution %` = if (!is.na(total_weeks) && total_weeks > 0) {
            round(Weeks_Activity / total_weeks * 100, 2)
          } else { NA },
          `Spend Distribution %` = if (!is.na(total_weeks) && total_weeks > 0) {
            round(Weeks_Spend / total_weeks * 100, 2)
          } else { NA }
        )
      
      # CPC/CPM based on keyword
      merged_df <- merged_df %>%
        mutate(
          `CPC/CPM` = case_when(
            keyword %in% c("Impressions", "Circulation") & Activity > 0 ~ round(Spend / Activity * 1000, 2),
            keyword == "Clicks" & Activity > 0 ~ round(Spend / Activity, 2),
            TRUE ~ NA_real_
          )
        )
      
      # RAG or No-RAG
      rag_type <- if (n_distinct(merged_df$Activity) == 1) "RAG" else "No-RAG"
      
      merged_df <- merged_df %>%
        mutate(
          `Media Variable` = mvar,
          `Spend Variable` = ifelse(is.na(spend_var), "", spend_var),
          Type = rag_type
        ) %>%
        select(
          `Media Variable`,
          `Spend Variable`,
          Type,
          Geography,
          Activity,
          Spend,
          `Activity %`,
          `Spend %`,
          Weeks_Activity,
          `Activity Distribution %`,
          Weeks_Spend,
          `Spend Distribution %`,
          `CPC/CPM`
        )
      
      # If rag_type == "RAG", keep only the first row
      if (rag_type == "RAG") {
        merged_df <- merged_df %>% slice(1)
      }
      
      results_list[[mvar]] <- merged_df
    }
    
    if (length(results_list) == 0) {
      return(data.frame(Message = "No media variables found with the required keywords."))
    }
    
    final_table <- bind_rows(results_list)
    
    # Additional step: recalculate global totals within the table
    final_table <- final_table %>%
      summarise(
        total_activity = sum(Activity, na.rm = TRUE),
        total_spend    = sum(Spend, na.rm = TRUE),
        .groups        = 'drop'
      ) %>%
      right_join(final_table, by = character()) %>%
      group_by(`Media Variable`) %>%
      mutate(
        `Activity %` = ifelse(
          total_activity > 0,
          round(Activity / total_activity * 100, 2),
          0
        ),
        `Spend %` = ifelse(
          total_spend > 0,
          round(Spend / total_spend * 100, 2),
          0
        )
      ) %>%
      ungroup() %>%
      select(-total_activity, -total_spend) %>%
      arrange(`Media Variable`, Geography)
    
    return(final_table)
  })
  
  # Render the table
  output$consolidated_table <- renderDT({
    table_data <- consolidated_table()
    
    if ("Message" %in% names(table_data)) {
      return(datatable(table_data, options = list(dom = 't')))
    }
    
    # Format numeric columns
    table_data <- table_data %>%
      mutate(
        across(where(is.numeric),
               ~ format(
                 .,
                 big.mark       = ",",
                 decimal.mark   = ".",
                 nsmall         = 2,
                 scientific     = FALSE
               ))
      )
    
    datatable(
      table_data,
      filter = "top",
      rownames = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons'
    )
  })
  
  outputOptions(output, "consolidated_table", suspendWhenHidden = FALSE)
  
  output$download_consolidated <- downloadHandler(
    filename = function() {
      paste("Summary_Table_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(consolidated_table(), file, row.names = FALSE)
    }
  )
  
  return(consolidated_table)
}
