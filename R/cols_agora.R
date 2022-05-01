#' @title Function to extract AGORA colors as hex codes
#'
#' @param ... Character names of reach colors. If NULL returns all colors
#' @param unnamed Should the output vector be unnamed? Default to `TRUE`
#'
#' @return An hex code or hex codes named or unnamed
#'
#' @details This function needs to be modified to add colors
#'
#' @export
cols_agora <- function(..., unnamed = TRUE) {
  cols <- c(...)

  colors_agora <- c(white         = "#FFFFFF",
                    black         = "#000000",
                    main_bordeaux = "#581522",
                    main_lt_beige = "#DDD8C4",
                    main_dk_beige = "#B7AD99",
                    main_lt_grey  = "#BCB8B1")

  if (is.null(cols)) {
    cols_to_return <- colors_agora
  } else {
    cols_to_return <- colors_agora[cols]
  }

  if(unnamed){
    cols_to_return <- unname(cols_to_return)
  }

  return(cols_to_return)
}
