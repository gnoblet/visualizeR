#' @title Check if variables are in data frame
#'
#' @param df A data frame
#' @param vars A vector of variable names
#'
#' @return A stop statement
check_vars_in_df <- function(df, vars) {
  vars_nin <- setdiff(vars, colnames(df))

  if (length(vars_nin) > 0) {
    rlang::abort(glue::glue(
      "Variables ",
      glue::glue_collapse(vars_nin, sep = ", ", last = ", and "),
      " not found in data frame."
    ))
  }
}
