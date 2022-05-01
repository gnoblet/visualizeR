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
    `main`            = cols_agora("main_grey", "main_red", "main_lt_grey", "main_beige"),
    `primary`         = cols_agora("main_grey", "main_red"),
    `secondary`       = cols_agora("main_lt_grey", "main_beige"),
    `two_dots`        = cols_agora("two_dots_1", "two_dots_2"),
    `two_dots_flashy` = cols_agora("two_dots_flashy_1", "two_dots_flashy_2"),
    `red_main`        = cols_agora("red_main_1", "red_main_2", "red_main_3", "red_main_4", "red_main_5"),
    `red_alt`         = cols_agora("red_alt_1", "red_alt_2", "red_alt_3", "red_alt_4", "red_alt_5"),
    `iroise`          = cols_agora("iroise_1", "iroise_2", "iroise_3", "iroise_4", "iroise_5"),
    `discrete_6`      = cols_agora("dk_grey", "red_main_1", "main_beige", "red_main_2", "lt_grey_2", "red_4")
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
