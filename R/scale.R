#' Color scale constructor for REACH or AGORA colors
#'
#' @param initiative Either "reach" or "agora.
#' @param palette Palette name from `pal_reach()` or `pal_agora()`.
#' @param discrete Boolean indicating whether color aesthetic is discrete or not.
#' @param reverse Boolean indicating whether the palette should be reversed.
#' @param reverse_guide Boolean indicating whether the guide should be reversed.
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradient(), used respectively when discrete is TRUE or FALSE.
#'
#' @return A color scale for ggplot
#'
#' @export
scale_color  <- function(initiative = "reach", palette = "main", discrete = TRUE, reverse = FALSE, reverse_guide = TRUE, ...) {

  if (initiative == "reach") {
    pal <- pal_reach(
      palette = palette,
      reverse = reverse,
      color_ramp_palette = TRUE,
      show_palettes = FALSE
      )
  } else if (initiative == "agora") {
    pal <- pal_agora(
      palette = palette,
      reverse = reverse,
      color_ramp_palette = TRUE,
      show_palettes = FALSE
    )
  } else {
    rlang::abort(c("Wrong initiative parameter input", "*" = paste0(initiative, "is not an option"), "i" = "Parameter 'initiative' should be one of 'reach' or 'agora'"))
  }

  if (discrete) {
    ggplot2::discrete_scale(
      "colour",
      paste0(initiative, "_", palette),
      palette = pal,
      guide = ggplot2::guide_legend(
        title.position = "top",
        draw.ulim = TRUE,
        draw.llim = TRUE,
        ticks.colour = "#F1F3F5",
        reverse = reverse_guide),
      ...)
  } else {
    ggplot2::scale_color_gradientn(
      colours = pal(256),
      guide = ggplot2::guide_colorbar(
        title.position = "top",
        draw.ulim = TRUE,
        draw.llim = TRUE,
        ticks.colour = "#F1F3F5",
        reverse = reverse_guide
      ),
      ...)
  }
}



#' Fill scale constructor for REACH or AGORA colors
#'
#' @param initiative Either "reach" or "agora.
#' @param palette Palette name from `pal_reach()` or `pal_agora()`.
#' @param discrete Boolean indicating whether color aesthetic is discrete or not.
#' @param reverse Boolean indicating whether the palette should be reversed.
#' @param reverse_guide Boolean indicating whether the guide should be reversed.
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradient(), used respectively when discrete is TRUE or FALSE.
#'
#' @return A fill scale for ggplot.
#'
#' @export
scale_fill  <- function(initiative = "reach", palette = "main", discrete = TRUE, reverse = FALSE, reverse_guide = TRUE, ...) {

  if (initiative == "reach") {
    pal <- pal_reach(
      palette = palette,
      reverse = reverse,
      color_ramp_palette = TRUE,
      show_palettes = FALSE
    )
  } else if (initiative == "agora") {
    pal <- pal_agora(
      palette = palette,
      reverse = reverse,
      color_ramp_palette = TRUE,
      show_palettes = FALSE
    )
  } else {
    rlang::abort(c("Wrong initiative parameter input", "*" = paste0(initiative, "is not an option"), "i" = "Parameter 'initiative' should be one of 'reach' or 'agora'"))
  }

  if (discrete) {
    ggplot2::discrete_scale(
      "fill",
      paste0(initiative, "_", palette),
      palette = pal,
      guide = ggplot2::guide_legend(
        title.position = "top",
        draw.ulim = TRUE,
        draw.llim = TRUE,
        ticks.colour = "#F1F3F5",
        reverse = reverse_guide),
      ...)
  } else {
    ggplot2::scale_fill_gradientn(
      colours = pal(256),
      guide = ggplot2::guide_colorbar(
        title.position = "top",
        draw.ulim = TRUE,
        draw.llim = TRUE,
        ticks.colour = "#F1F3F5",
        reverse = reverse_guide),
        ...)
  }
}
