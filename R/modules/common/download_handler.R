# Download_Handler.R

# =============================================================================
# Configures the downloadHandler to export the transformed dataset,
# with robust validations.
# =============================================================================

download_handler_module <- function(input, output, session, rv) {
  # Sets up the download handler for the analytical file
  #
  # Args:
  #   input: Shiny input object
  #   output: Shiny output object
  #   session: Shiny session object
  #   rv: Shared reactive values
  
  output$download_analytical <- downloadHandler(
    filename = function() {
      paste0("analytical_transformed_", Sys.Date(), ".csv")
    },
    content = function(file) {
      tryCatch({
        download_data <- rv$filtered_data
        if (is.null(download_data) || nrow(download_data) == 0) {
          stop("No data available for download.")
        }
        
        # ---------------------------
        # UNIVARIATE TRANSFORMATION
        # ---------------------------
        if (!is.null(input$variable_univ) && input$variable_univ != "") {
          newcol <- paste0(input$variable_univ, "_transformed")
          download_data[[newcol]] <- apply_transformation(
            download_data[[input$variable_univ]],
            type   = input$transformation_univ,
            alpha  = input$alpha_univ,
            beta   = input$beta_univ,
            maxval = input$maxval_univ,
            decay  = input$decay_univ,
            lag    = input$lag_univ
          )
        }
        
        # --------------------------------
        # MULTIVARIATE TRANSFORMATION LOGIC
        # --------------------------------
        # If sum_all_vars is enabled, we transform each selected variable
        # using the respective transformation. Then (optionally) the user
        # can sum them for correlation. But in any case, we store them
        # as separate transformed columns.
        if (!is.null(input$sum_all_vars) && input$sum_all_vars == "true") {
          vars_to_transform <- c(input$var1_multi, input$var2_multi, input$var3_multi)
          if (!is.null(input$var4_multi) && input$var4_multi != "None" && input$var4_multi != "") {
            vars_to_transform <- c(vars_to_transform, input$var4_multi)
          }
          for (var_i in vars_to_transform) {
            if (!var_i %in% names(download_data)) next
            # Determine the selected transformation for each variable
            trans_type <- if (var_i == input$var1_multi) {
              input$trans_var1
            } else if (var_i == input$var2_multi) {
              input$trans_var2
            } else if (var_i == input$var3_multi) {
              input$trans_var3
            } else if (var_i == input$var4_multi) {
              input$trans_var4
            } else {
              "Linear"
            }
            newcol <- paste0(var_i, "_transformed")
            download_data[[newcol]] <- apply_transformation(
              download_data[[var_i]],
              type    = trans_type,
              alpha   = input$alpha_multi,
              beta    = input$beta_multi,
              maxval  = input$maxval_multi,
              decay   = input$decay_multi,
              lag     = input$lag_multi
            )
          }
        } else {
          # sum_all_vars == "false" => transform each variable individually
          # using trans_var1, trans_var2, etc.
          vars_ind <- c(input$var1_multi, input$var2_multi, input$var3_multi)
          if (!is.null(input$var4_multi) && input$var4_multi != "None") {
            vars_ind <- c(vars_ind, input$var4_multi)
          }
          for (var_i in vars_ind) {
            if (!var_i %in% names(download_data)) next
            trans_type <- if (var_i == input$var1_multi) {
              input$trans_var1
            } else if (var_i == input$var2_multi) {
              input$trans_var2
            } else if (var_i == input$var3_multi) {
              input$trans_var3
            } else if (var_i == input$var4_multi) {
              input$trans_var4
            } else {
              "Linear"
            }
            newcol <- paste0(var_i, "_transformed")
            download_data[[newcol]] <- apply_transformation(
              download_data[[var_i]],
              type    = trans_type,
              alpha   = input$alpha_multi,
              beta    = input$beta_multi,
              maxval  = input$maxval_multi,
              decay   = input$decay_multi,
              lag     = input$lag_multi
            )
          }
        }
        
        write.csv(download_data, file, row.names = FALSE, na = "")
        notifyUser("Transformed data file downloaded.", "message", duration = 3)
        
      }, error = function(e) {
        notifyUser(paste("Download error:", e$message), "error")
      })
    }
  )
}
