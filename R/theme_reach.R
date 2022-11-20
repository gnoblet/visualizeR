#' @title ggplot2 theme with REACH color palettes
#'
#' @param palette Palette name from 'pal_reach()'.
#' @param discrete 	Boolean indicating whether color aesthetic is discrete or not.
#' @param reverse Boolean indicating whether the palette should be reversed.
#' @param family The font family for all plot's texts. Default to "Leelawadee".
#' @param text_size The size of all text other than the title, subtitle and caption. Defaults to 10.
#' @param title_size The size of the title text_family. Defaults to 14.
#' @param plot_background_pal The color for the plot background color. Default to white.
#' @param panel_background_pal The color for the panel background color. Default to white.
#' @param legend_position Position of the legend; Default to "right". Can take "right", "left", "top", "bottom" or "none".
#' @param legend_direction Direction of the legend. Default to "vertical". Can take "vertical" or "horizontal".
#' @param legend_reverse Reverse the color in the guide? Default to TRUE.
#' @param void Boolean to remove all elements from the plot. Default to FALSE.
#' @param ...	Additional arguments passed to `ggblanket::gg_theme()`.
#'
#'
#' @description Give some reach colors and fonts to a ggplot.
#'
#' @return The base REACH theme
#'
#' @export
theme_reach <- function(
    palette = "main",
    discrete = TRUE,
    reverse = FALSE,
    family = "Leelawadee",
    text_size = 10,
    title_size = 14,
    plot_background_pal = "#FFFFFF",
    panel_background_pal = "#FFFFFF",
    void = FALSE,
    legend_position = "right",
    legend_direction = "vertical",
    legend_reverse = TRUE,
    ...
    ) {

  # Basic simple theme
  theme_reach <- ggblanket::gg_theme(
    text_family = family,
    text_size = text_size,
    title_size = title_size,
    plot_background_pal = plot_background_pal,
    panel_background_pal = panel_background_pal,
    void = void
  )


  # Default legend to right position
  theme_reach <- theme_reach +
    ggplot2::theme(legend.position = legend_position)

  # Defaut legend to vertical direction
  theme_reach <- theme_reach +
    ggplot2::theme(legend.direction = legend_direction)

  # Add reach color palettes by default
  # (reversed guide is defaulted to TRUE for natural reading)
  theme_reach <- list(
    theme_reach,
    scale_color(palette = palette, discrete = discrete, reverse = reverse, reverse_guide = legend_reverse),
    scale_fill(palette = palette, discrete = discrete, reverse = reverse, reverse_guide = legend_reverse)
    )


  return(theme_reach)

}
