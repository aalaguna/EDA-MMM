# Data_Processing.R

# =============================================================================
# Handles multivariate data processing (summing or individual transformations).
# =============================================================================

# (A) Summing
process_summed_data <- function(df, input) {
  # Sums selected variables into a new column
  #
  # Args:
  #   df: Dataframe with data
  #   input: Shiny input object to access selected variables
  #
  # Returns:
  #   Dataframe with new sum_vars column
  
  req(df)
  vars_selected <- c(input$var1_multi, input$var2_multi, input$var3_multi)
  if (!is.null(input$var4_multi) && input$var4_multi != "None" && input$var4_multi != "") {
    vars_selected <- c(vars_selected, input$var4_multi)
  }
  vars_selected <- intersect(vars_selected, names(df))
  if (length(vars_selected) < 1) {
    return(df)
  }
  df$sum_vars <- rowSums(df[, vars_selected, drop = FALSE], na.rm = TRUE)
  return(df)
}

# (B) Transform the sum
process_transformed_data <- function(df, input) {
  # Transforms the sum of variables
  #
  # Args:
  #   df: Dataframe with sum_vars column
  #   input: Shiny input object for transformation parameters
  #
  # Returns:
  #   Dataframe with new trans_sum_vars column
  
  req(df, "sum_vars" %in% names(df))
  transformed <- apply_transformation(df$sum_vars,
                                      type   = input$trans_var1, # using trans_var1
                                      alpha  = input$alpha_multi,
                                      beta   = input$beta_multi,
                                      maxval = input$maxval_multi,
                                      decay  = input$decay_multi,
                                      lag    = input$lag_multi)
  df$trans_sum_vars <- transformed
  df
}

# (C) Transform each selected variable individually
process_individual_vars <- function(df, input) {
  # Transforms each selected variable individually.
  # Only var1_multi will apply the transformation chosen in input$trans_var1.
  # Other variables are left in "Linear".
  #
  # Args:
  #   df: Dataframe with data
  #   input: Shiny input object for variables and parameters
  #
  # Returns:
  #   Dataframe with new transformed columns
  
  req(df)

  vars_selected <- c(input$var1_multi, input$var2_multi, input$var3_multi)
  if (!is.null(input$var4_multi) && input$var4_multi != "None") {
    vars_selected <- c(vars_selected, input$var4_multi)
  }
  vars_selected <- intersect(vars_selected, names(df))
  if (length(vars_selected) < 1) {
    return(df)
  }

  for (var_i in vars_selected) {
    # If the variable is the same as var1_multi, use input$trans_var1.
    # Others use "Linear".
    trans_type <- if (var_i == input$var1_multi) {
      input$trans_var1
    } else {
      "Linear"
    }
    
    newcol <- paste0(var_i, "_transformed")
    df[[newcol]] <- apply_transformation(
      df[[var_i]],
      type   = trans_type,
      alpha  = input$alpha_multi,
      beta   = input$beta_multi,
      maxval = input$maxval_multi,
      decay  = input$decay_multi,
      lag    = input$lag_multi
    )
  }

  return(df)
}