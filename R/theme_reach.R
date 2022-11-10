#' @title ggplot2 theme with REACH color palettes
#'
#' @param palette Palette name from 'pal_reach()'.
#' @param discrete 	Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param family The font family for all plot's texts. Default to "Leelawadee".
#' @param text_size The size of all text other than the title, subtitle and caption. Defaults to 10.
#' @param title_size The size of the title text_family. Defaults to 14.
#' @param plot_background_pal The color for the plot background color. Default to white.
#' @param panel_background_pal The color for the panel background color. Default to white.
#' @param legend_position Position of the legend; Default to "right". Can take "right", "left", "top", "bottom" or "none".
#' @param legend_direction Direction of the legend. Default to "vertical". Can take "vertical" or "horizontal".
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
  theme_reach <- list(
    theme_reach,
    scale_color(palette = palette, discrete = discrete, reverse = reverse),
    scale_fill(palette = palette, discrete = discrete, reverse = reverse)
    )


  return(theme_reach)

}

#' @title Some REACH theme for ggplot
#'
#' @param family The font family. Default to "Leelawadee"
#'
#' @return A theme to be added to the "+" ggplot grammar
#'
#' @export
theme_reach_borders <- function(family = "Leelawadee") {

  theme_reach() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(colour = "white", fill = "white", size = 0.5),
      strip.background = ggplot2::element_rect(linetype = "solid", colour = "#58585A", fill = "white")
    )
}



#' @title Some reach more minimal theme for a ggplot histogram
#' @param family The font family. Default to "Leelawadee"
#'
#' @description Give some REACH colors and fonts to a ggplot. Based on theme_bw(). To be used for vertical bar charts.
#'
#' @return A theme to be added to the "+" ggplot grammar
#'
#' @export
theme_reach_hist <- function(family = "Leelawadee") {

  theme_reach() +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank()
    )
}


#' @title Some reach more minimal theme for a ggplot flipped histogram
#'
#' @param family The font family. Default to "Leelawadee"
#'
#' @description Give some REACH colors and fonts to a ggplot. Based on theme_bw(). To be used for horizontal bar charts.
#'
#' @return A theme to be added to the "+" ggplot grammar
#'
#' @export
theme_reach_flip_hist <- function(family = "Leelawadee") {

  theme_reach() +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
}



