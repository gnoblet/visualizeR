#' @title Return function to interpolate a REACH color palette
#'
#' @param palette Character name of a palette in REACH palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param color_ramp_palette Should the output be a `grDevices::colorRampPalette` function or a vector of hex codes? Default to the former with `TRUE`
#' @param show_palettes Should the ouput be the set of palettes names to pick from? Default to `FALSE`
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @return A color palette
#'
#' @export
pal_reach <- function(palette = "main", reverse = FALSE, color_ramp_palette = FALSE, show_palettes = FALSE, ...) {

  palettes_reach <- list(
    `main`            = cols_reach("main_grey", "main_red", "main_lt_grey", "main_beige"),
    `primary`         = cols_reach("main_grey", "main_red"),
    `secondary`       = cols_reach("main_lt_grey", "main_beige"),
    `two_dots`        = cols_reach("two_dots_1", "two_dots_2"),
    `two_dots_flashy` = cols_reach("two_dots_flashy_1", "two_dots_flashy_2"),
    `red_main`        = cols_reach("red_main_1", "red_main_2", "red_main_3", "red_main_4", "red_main_5"),
    `red_main_5`      = cols_reach("red_main_1", "red_main_2", "red_main_3", "red_main_4", "red_main_5"),
    `red_alt`         = cols_reach("red_alt_1", "red_alt_2", "red_alt_3", "red_alt_4", "red_alt_5"),
    `red_alt_5`       = cols_reach("red_alt_1", "red_alt_2", "red_alt_3", "red_alt_4", "red_alt_5"),
    `iroise`          = cols_reach("iroise_1", "iroise_2", "iroise_3", "iroise_4", "iroise_5"),
    `iroise_5`        = cols_reach("iroise_1", "iroise_2", "iroise_3", "iroise_4", "iroise_5"),
    `discrete_6`      = cols_reach("dk_grey", "red_main_1", "main_beige", "red_main_2", "lt_grey_2", "red_4"),
    `red_2` = cols_reach("red_less_4_1", "red_less_4_3"),
    `red_3` = cols_reach("red_less_4_1", "red_less_4_2", "red_less_4_3"),
    `red_4` = cols_reach("red_less_4_1", "red_less_4_2", "red_less_4_3", "red_less_4_4"),
    `red_5` = cols_reach("red_5_1", "red_5_2", "red_5_3", "red_5_4", "red_5_5"),
    `red_6` = cols_reach("red_less_7_1", "red_less_2", "red_less_7_3", "red_less_7_4", "red_less_7_5", "red_less_7_6"),
    `red_7` = cols_reach("red_less_7_1", "red_less_7_2", "red_less_7_3", "red_less_7_4", "red_less_7_5", "red_less_7_6", "red_less_7_7"),
    `green_2` = cols_reach("green_2_1", "green_2_2"),
    `green_3` = cols_reach("green_3_1", "green_3_2", "green_3_3"),
    `green_4` = cols_reach("green_4_1", "green_4_2", "green_4_3", "green_4_4"),
    `green_5` = cols_reach("green_5_1", "green_5_2", "green_5_3", "green_5_4", "green_5_5"),
    `green_6` = cols_reach("green_6_1", "green_6_2", "green_6_3", "green_6_4", "green_6_5", "green_6_6"),
    `green_7` = cols_reach("green_7_1", "green_7_2", "green_7_3", "green_7_4", "green_7_5", "green_7_6", "green_7_7"),
    `artichoke_2` = cols_reach("artichoke_2_1", "artichoke_2_2"),
    `artichoke_3` = cols_reach("artichoke_3_1", "artichoke_3_2", "artichoke_3_3"),
    `artichoke_4` = cols_reach("artichoke_4_1", "artichoke_4_2", "artichoke_4_3", "artichoke_4_4"),
    `artichoke_5` = cols_reach("artichoke_5_1", "artichoke_5_2", "artichoke_5_3", "artichoke_5_4", "artichoke_5_5"),
    `artichoke_6` = cols_reach("artichoke_6_1", "artichoke_6_2", "artichoke_6_3", "artichoke_6_4", "artichoke_6_5", "artichoke_6_6"),
    `artichoke_7` = cols_reach("artichoke_7_1", "artichoke_7_2", "artichoke_7_3", "artichoke_7_4", "artichoke_7_5", "artichoke_7_6", "artichoke_7_7"),
    `blue_2` = cols_reach("blue_2_1", "blue_2_2"),
    `blue_3` = cols_reach("blue_3_1", "blue_3_2", "blue_3_3"),
    `blue_4` = cols_reach("blue_4_1", "blue_4_2", "blue_4_3", "blue_4_4"),
    `blue_5` = cols_reach("blue_5_1", "blue_5_2", "blue_5_3", "blue_5_4", "blue_5_5"),
    `blue_6` = cols_reach("blue_6_1", "blue_6_2", "blue_6_3", "blue_6_4", "blue_6_5", "blue_6_6"),
    `blue_7` = cols_reach("blue_7_1", "blue_7_2", "blue_7_3", "blue_7_4", "blue_7_5", "blue_7_6", "blue_7_7")
  )

  if (show_palettes) return(names(palettes_reach))

  pal <- palettes_reach[[palette]]

  if (reverse) pal <- rev(pal)

  if (color_ramp_palette) {
    rlang::check_installed("grDevices", reason = "Package \"grDevices\" needed for `pal_reach()` with 'color_ramp_palette' set to `TRUE` to work. Please install it.")

    pal <- grDevices::colorRampPalette(pal, ...)
  }

  return(pal)
}
