#' @title ggplot2 theme with REACH color palettes
#'
#' @param palette Palette name from 'pal_reach()'.
#' @param discrete 	Boolean indicating whether color aesthetic is discrete or not.
#' @param reverse Boolean indicating whether the palette should be reversed.
#' @param font_family The font family for all plot's texts. Default to "Leelawadee".
#' @param title_size The size of the title. Defaults to 12.
#' @param title_color Title color.
#' @param title_font_face Title font face. Default to "bold". Font face ("plain", "italic", "bold", "bold.italic").
#' @param text_size The size of all text other than the title, subtitle and caption. Defaults to 10.
#' @param text_color Text color.
#' @param text_font_face Text font face. Default to "bold". Font face ("plain", "italic", "bold", "bold.italic").
#' @param panel_background_color The color for the panel background color. Default to white.
#' @param legend_position Position of the legend; Default to "right". Can take "right", "left", "top", "bottom" or "none".
#' @param legend_direction Direction of the legend. Default to "vertical". Can take "vertical" or "horizontal".
#' @param legend_reverse Reverse the color in the guide? Default to TRUE.
#' @param axis_x Boolean. Do you need x-axis?
#' @param axis_y Boolean. Do you need y-axis?
#' @param grid_x Boolean. Do you need major grid lines for x-axis?
#' @param grid_y Boolean. Do you need major grid lines for y-axis?
#' @param grid_x_size X line size.
#' @param grid_y_size Y line size.
#' @param grid_color Grid lines color.
#' @param ...	Additional arguments passed to `ggplot2::gg_theme()`.
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
    font_family = "Leelawadee",
    title_size = 12,
    title_color = cols_reach("main_grey"),
    title_font_face = "bold",
    text_size = 10,
    text_color = cols_reach("main_grey"),
    text_font_face = "bold",
    panel_background_color = "#FFFFFF",
    legend_position = "right",
    legend_direction = "vertical",
    legend_reverse = TRUE,
    axis_x = TRUE,
    axis_y = TRUE,
    grid_x = FALSE,
    grid_y = FALSE,
    grid_color = cols_reach("main_lt_grey"),
    grid_x_size = 0.1,
    grid_y_size = 0.1,
    ...
    ) {

  # To do :
  # - add facet theming

  # Basic simple theme
  # theme_reach <- ggplot2::theme_bw()

  theme_reach <-  ggplot2::theme(
    # Title - design
    title = ggplot2::element_text(
      family = font_family,
      color = title_color,
      size = title_size,
      face = title_font_face
    ),
    # Text - design
    text = ggplot2::element_text(
      family = font_family,
      color = text_color,
      size = text_size,
      face = text_font_face
    ),
    # Default legend to right position
    legend.position = legend_position,
    # Defaut legend to vertical direction
    legend.direction = legend_direction,
    # set panel background color
    panel.background = ggplot2::element_rect(
      fill = panel_background_color
    ),
    # Remove minor grid lines
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    # Remove background for legend key
    legend.key = ggplot2::element_blank(),
    plot.title.position = "plot",
    plot.caption.position =  "plot"
  )

  # Axis lines ?
  if (axis_x & axis_y) {
    theme_reach <- theme_reach +
      ggplot2::theme(
        axis.line = ggplot2::element_line(color = text_color))
  }

  if (!axis_x) {
    theme_reach <- theme_reach +
      ggplot2::theme(
        axis.line.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank())
  }

  if (!axis_y) {
    theme_reach <- theme_reach +
      ggplot2::theme(
        axis.line.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank())
  }

  # X - major grid lines
  if (!grid_x) theme_reach <- theme_reach +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank()
  ) else theme_reach <- theme_reach +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(
        color = grid_color,
        linewidth = grid_y_size)
    )

  # Y - major grid lines
  if (!grid_y) theme_reach <- theme_reach +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank()
  ) else theme_reach <- theme_reach +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(
        color = grid_color,
        linewidth = grid_y_size)
    )

  # Other parameters
  theme_reach <- theme_reach + ggplot2::theme(...)


  # Check if palette is an actual existing palette
  pal <- pal_reach(palette)

  if(is.null(pal)) {
    rlang::warn(
      c(
        paste0("There is no palette '", palette, "' for initiative 'reach'. Fallback to REACH main palette."),
        "i" = paste0("Use `pal_reach(show_palettes = TRUE)` to see the list of availabale palettes.")
        )
      )

    palette <- "main"

  }

  # Add reach color palettes by default
  # (reversed guide is defaulted to TRUE for natural reading)
  theme_reach <- list(
    theme_reach,
    scale_color(palette = palette, discrete = discrete, reverse = reverse, reverse_guide = legend_reverse),
    scale_fill(palette = palette, discrete = discrete, reverse = reverse, reverse_guide = legend_reverse)
    )


  return(theme_reach)

}
