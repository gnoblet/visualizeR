#' @title Function to extract IMPACT colors as hex codes
#'
#' @param ... Character names of reach colors. If NULL returns all colors
#' @param unnamed Should the output vector be unnamed? Default to `TRUE`
#'
#' @return An hex code or hex codes named or unnamed
#'
#' @details This function needs to be modified to add colors
#'
#' @export
cols_impact <- function(..., unnamed = TRUE) {
  cols <- c(...)

  colors_impact <- c(white     = "#FFFFFF",
                    black     = "#000000",
                    main_blue = "#315975",
                    main_gray = "#58585A")

  if (is.null(cols)) {
    cols_to_return <- colors_impact
  } else {
    cols_to_return <- colors_impact[cols]
  }

  if(unnamed){
    cols_to_return <- unname(cols_to_return)
  }

  return(cols_to_return)
}
