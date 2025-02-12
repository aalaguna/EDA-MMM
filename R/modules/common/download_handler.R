# R/modules_common/download_handler.R
# =============================================================================
# Módulo: download_handler
# Configura el downloadHandler para exportar el dataset transformado, con
# validaciones robustas.
# =============================================================================

download_handler_module <- function(input, output, session, rv) {
  output$download_analytical <- downloadHandler(
    filename = function() {
      paste0("analytical_transformed_", Sys.Date(), ".csv")
    },
    content = function(file) {
      tryCatch({
        download_data <- rv$filtered_data
        if (is.null(download_data) || nrow(download_data) == 0) {
          stop("No hay datos disponibles para descargar.")
        }
        # Transformación en panel univariado
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
        # Transformación en panel multivariado (si se suma)
        if (!is.null(input$sum_all_vars) && input$sum_all_vars == "true") {
          vars_to_transform <- c(input$var1_multi, input$var2_multi, input$var3_multi)
          if (!is.null(input$var4_multi) && input$var4_multi != "None" && input$var4_multi != "") {
            vars_to_transform <- c(vars_to_transform, input$var4_multi)
          }
          for (var_i in vars_to_transform) {
            if (!var_i %in% names(download_data)) next
            # Determinar el tipo de transformación según la variable seleccionada
            if (var_i == input$var1_multi) {
              trans_type <- input$trans_var1
            } else if (var_i == input$var2_multi) {
              trans_type <- input$trans_var2
            } else if (var_i == input$var3_multi) {
              trans_type <- input$trans_var3
            } else if (var_i == input$var4_multi) {
              trans_type <- input$trans_var4
            } else {
              trans_type <- "Linear"
            }
            newcol <- paste0(var_i, "_transformed")
            download_data[[newcol]] <- apply_transformation(
              download_data[[var_i]],
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
        notifyUser("Archivo analítico transformado descargado.", "message", duration = 3)
      }, error = function(e) {
        notifyUser(paste("Error en descarga:", e$message), "error")
      })
    }
  )
}
