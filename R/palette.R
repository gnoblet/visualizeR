#' @title Interpolate a color palette
#'
#' @param palette Character name of a palette in palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param show_palettes Should the ouput be the set of palettes names to pick from? Default to `FALSE`
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @return A color palette
#'
#' @export
palette <- function(
  palette = "cat_5_main",
  reverse = FALSE,
  show_palettes = FALSE,
  ...
) {
  #------ Checks

  # Check that palette is a character scalar
  checkmate::assert_character(palette, len = 1)

  # Check that reverse is a logical scalar
  checkmate::assert_logical(reverse, len = 1)

  # Check that show_palettes is a logical scalar
  checkmate::assert_logical(show_palettes, len = 1)

  #------ Get colors

  # Define palettes
  pals <- list(
    cat_2_yellow = color_pattern("cat_2_yellow"),
    cat_2_light = color_pattern("cat_2_light"),
    cat_2_green = color_pattern("cat_2_green"),
    cat_2_blue = color_pattern("cat_2_blue"),
    cat_5_main = color_pattern("cat_5_main"),
    cat_5_ibm = color_pattern("cat_5_ibm"),
    cat_3_aquamarine = color_pattern("cat_3_aquamarine"),
    cat_3_tol_high_contrast = color_pattern("cat_3_tol_high_contrast"),
    cat_8_tol_adapted = color_pattern("cat_8_tol_adapted"),
    cat_3_custom_1 = c("#003F5C", "#58508D", "#FFA600"),
    cat_4_custom_1 = c("#003F5C", "#7a5195", "#ef5675", "#ffa600"),
    cat_5_custom_1 = c("#003F5C", "#58508d", "#bc5090", "#ff6361", "#ffa600"),
    cat_6_custom_1 = c(
      "#003F5C",
      "#444e86",
      "#955196",
      "#dd5182",
      "#ff6e54",
      "#ffa600"
    ),
    div_5_orange_blue = color_pattern("div_5_orange_blue"),
    div_5_green_purple = color_pattern("div_5_green_purple")
  )

  # Return if show palettes
  if (show_palettes) {
    return(names(pals))
  }

  # palette is in pals
  if (palette %notin% names(pals)) {
    rlang::abort(c(
      "Palette not defined",
      "*" = glue::glue(
        "Palette `{palette}` is not defined in the `palettes` list."
      ),
      "i" = "Use `palette(show_palettes = TRUE)` to see all available palettes."
    ))
  }

  #------ Get palette

  pal <- pals[[palette]]

  if (reverse) {
    pal <- rev(pal)
  }

  return(pal)
}
