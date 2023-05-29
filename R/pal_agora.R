#' @title Return function to interpolate an AGORA color palette
#'
#' @param palette Character name of a palette in AGORA palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param color_ramp_palette Should the output be a `grDevices::colorRampPalette` function or a vector of hex codes? Default to the former with `TRUE`
#' @param show_palettes Should the ouput be the set of palettes names to pick from? Default to `FALSE`
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @return A color palette
#'
#' @export
pal_agora <- function(palette = "main", reverse = FALSE, color_ramp_palette = FALSE, show_palettes = FALSE, ...) {


  palettes_agora <- list(
    `main`            = cols_agora("main_bordeaux", "main_dk_beige", "main_lt_grey", "main_lt_beige"),
    `primary`         = cols_agora("main_bordeaux", "main_dk_beige"),
    `secondary`       = cols_agora( "main_lt_grey", "main_lt_beige")
  )

  if (show_palettes) return(names(palettes_agora))

  pal <- palettes_agora[[palette]]

  if (reverse) pal <- rev(pal)

  if (color_ramp_palette) {
    rlang::check_installed("grDevices", reason = "Package \"grDevices\" needed for `pal_agora()` woth 'color_ramp_palette' set to `TRUE` to work. Please install it.")

    pal <- grDevices::colorRampPalette(pal, ...)
  }

  return(pal)
}
