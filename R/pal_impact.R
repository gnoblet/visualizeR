#' @title Return function to interpolate an IMPACT color palette
#'
#' @param palette Character name of a palette in IMPACT palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param color_ramp_palette Should the output be a `grDevices::colorRampPalette` function or a vector of hex codes? Default to the former with `TRUE`
#' @param show_palettes Should the ouput be the set of palettes names to pick from? Default to `FALSE`
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @return A color palette
#'
#' @export
pal_impact <- function(palette = "main", reverse = FALSE, color_ramp_palette = FALSE, show_palettes = FALSE, ...) {


  palettes_impact <- list(
    `main`            = cols_impact("black", "white", "main_blue", "main_grey"),
    `primary`         = cols_impact("black", "white"),
    `secondary`       = cols_impact("main_blue", "main_grey")
  )

  if (show_palettes) return(names(palettes_impact))

  pal <- palettes_impact[[palette]]

  if (reverse) pal <- rev(pal)

  if (color_ramp_palette) {
    rlang::check_installed("grDevices", reason = "Package \"grDevices\" needed for `pal_impact()` woth 'color_ramp_palette' set to `TRUE` to work. Please install it.")

    pal <- grDevices::colorRampPalette(pal, ...)
  }

  return(pal)
}
