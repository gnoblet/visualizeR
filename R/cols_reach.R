#' @title Function to extract REACH colors as hex codes
#'
#' @param ... Character names of reach colors. If NULL returns all colors
#' @param unnamed Should the output vector be unnamed? Default to `TRUE`
#'
#' @return An hex code or hex codes named or unnamed
#'
#' @details This function needs to be modified to add colors
#'
#' @export
cols_reach <- function(..., unnamed = TRUE) {
  cols <- c(...)

  colors_reach <- c(white        = "#FFFFFF",
                    black        = "#000000",
                    main_grey    = "#58585A",
                    main_red     = "#EE5859",
                    main_lt_grey = "#C7C8CA",
                    main_beige   = "#D2CBB8",
                    iroise_1     = "#DFECEF",
                    iroise_2     = "#B1D7E0",
                    iroise_3     = "#699DA3",
                    iroise_4     = "#236A7A",
                    iroise_5     = "#0C3842",
                    red_main_1   = "#AE2829",
                    red_main_2   = "#D05E5F",
                    red_main_3   = "#DB9797",
                    red_main_4   = "#EBC7C8",
                    red_main_5   = "#FAF2F2",
                    red_alt_1    = "#792a2e",
                    red_alt_2    = "#c0474a",
                    red_alt_3    = "#ee5859",
                    red_alt_4    = "#f49695",
                    red_alt_5    = "#f8d6d6",
                    red_alt_na   = "#f8f4f4",
                    lt_grey_1    = "#C6C6C6",
                    lt_grey_2    = "#818183",
                    grey3        = "#E3E3E3",
                    dk_grey      = "#464647",
                    two_dots_1   = "#706441",
                    two_dots_2   = "#56b4e9",
                    two_dots_flashy_1 = "gold1",
                    two_dots_flashy_2 = "blue2",
                    three_dots_1 = "aquamarine2",
                    three_dots_2 = "cornflowerbluer",
                    three_dots_3 = "brown1",
                    orpink       = "#f8aa9b",
                    pink         = "#f5a6a7",
                    lt_pink      = "#F9C6C7",
                    hot_pink     = "#ef6d6f",
                    mddk_red     = "#bf4749",
                    dk_red       = "#782c2e",
                    orange       = "#F69E61",
                    lt_green     = "#B0CFAC",
                    green        = "#84A181",
                    dk_green     = "#526450")

  if (is.null(cols)) {
    cols_to_return <- colors_reach
  } else {
    cols_to_return <- colors_reach[cols]
  }

  if(unnamed){
    cols_to_return <- unname(cols_to_return)
  }

  return(cols_to_return)
}
