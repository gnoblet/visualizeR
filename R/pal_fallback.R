#' @title Return function to interpolate a fallback palette base on viridis::magma()
#'
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param color_ramp_palette Should the output be a `grDevices::colorRampPalette` function or a vector of hex codes? Default to the latter with `FALSE`
#' @param discrete Boolean. Discrete or not? Default to FALSE.
#' @param n Number of colors in the palette. Default to 5. Passe to `viridis::magma()`
#' @param ... Other parameters to pass to `grDevices::colorRampPalette()`
#'
#' @return A color palette
#'
#' @export
pal_fallback <- function(reverse = FALSE,
                         color_ramp_palette = FALSE,
                         discrete = FALSE,
                         n = 5,
                         ...){

  pal <- if(discrete) { viridisLite::viridis(n) } else {viridisLite::magma(n)}

  if (reverse) pal <- rev(pal)

  if (color_ramp_palette) {
    rlang::check_installed("grDevices", reason = "Package \"grDevices\" needed for `pal_fallback()` with 'color_ramp_palette' set to `TRUE` to work. Please install it.")

    pal <- grDevices::colorRampPalette(pal, ...)
  }

  return(pal)

}
