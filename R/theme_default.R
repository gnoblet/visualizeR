#' ggplot2 theme wrapper with fonts and colors
#'
#' @param title_size The size of the title. Defaults to 12.
#' @param title_color Title color.
#' @param title_font_face Title font face. Default to "bold". Font face ("plain", "italic", "bold", "bold.italic").
#' @param title_hjust Title horizontal justification. Default to NULL. Use 0.5 to center the title.
#' @param title_font_family Title font family. Default to "Carlito".
#' @param title_position_to_plot TRUE or FALSE. Positioning to plot or to panel?
#' @param subtitle_font_family Subtitle font family. Default to "Carlito".
#' @param subtitle_size The size of the subtitle. Defaults to 10.
#' @param subtitle_color Subtitle color.
#' @param subtitle_font_face Subtitle font face. Default to "plain". Font face ("plain", "italic", "bold", "bold.italic").
#' @param subtitle_hjust Subtitle horizontal justification. Default to NULL. Use 0.5 to center the subtitle.
#' @param text_font_family Text font family. Default to "Carlito".
#' @param text_size The size of all text other than the title, subtitle and caption. Defaults to 10.
#' @param text_color Text color.
#' @param text_font_face Text font face. Default to "bold". Font face ("plain", "italic", "bold", "bold.italic").
#' @param panel_background_color The color for the panel background color. Default to white.
#' @param panel_border Boolean. Plot a panel border? Default to FALSE.
#' @param panel_border_color A color. Default to REACH main grey.
#' @param legend_position Position of the legend; Default to "right". Can take "right", "left", "top", "bottom" or "none".
#' @param legend_direction Direction of the legend. Default to "vertical". Can take "vertical" or "horizontal".
#' @param legend_justification In addition to legend_direction, place the legend. Can take "left", "bottom", "center", "right", "top".
#' @param legend_title_size Legend title size.
#' @param legend_title_color Legend title color.
#' @param legend_title_font_face Legend title font face. Default to "plain". Font face ("plain", "italic", "bold", "bold.italic").
#' @param legend_title_font_family Legend title font family. Default to "Carlito".
#' @param legend_text_size Legend text size.
#' @param legend_text_color Legend text color.
#' @param legend_text_font_face Legend text font face. Default to "plain". Font face ("plain", "italic", "bold", "bold.italic").
#' @param legend_text_font_family Legend text font family. Default to "Carlito".
#'
#' @param legend_reverse Reverse the color in the guide? Default to TRUE.
#' @param facet_size Facet font size.
#' @param facet_color Facet font color.
#' @param facet_font_face Facet font face. Default to "plain". Font face ("plain", "italic", "bold", "bold.italic").
#' @param facet_font_family Facet font family. Default to "Carlito".
#' @param facet_bg_color Facet background color.
#' @param axis_x Boolean. Do you need x-axis?
#' @param axis_y Boolean. Do you need y-axis?
#' @param axis_text_font_family Axis text font family. Default to "Carlito".
#' @param axis_text_size Axis text size.
#' @param axis_text_color Axis text color.
#' @param axis_text_font_face Axis text font face. Default to "plain". Font face ("plain", "italic", "bold", "bold.italic").
#' @param axis_text_x Boolean. Do you need the text for the x-axis?
#' @param axis_line_x Boolean. Do you need the line for the x-axis?
#' @param axis_ticks_x Boolean. Do you need the line for the x-axis?
#' @param axis_text_x_angle Angle for the x-axis text.
#' @param axis_text_x_vjust Vertical adjustment for the x-axis text.
#' @param axis_text_x_hjust Vertical adjustment for the x-axis text.
#' @param axis_text_y Boolean. Do you need the text for the y-axis?
#' @param axis_line_y Boolean. Do you need the line for the y-axis?
#' @param axis_ticks_y Boolean. Do you need the line for the y-axis?
#' @param axis_title_size Axis title size.
#' @param axis_title_color Axis title color.
#' @param axis_title_font_face Axis title font face. Default to "plain". Font face ("plain", "italic", "bold", "bold.italic").
#' @param grid_major_x Boolean. Do you need major grid lines for x-axis?
#' @param grid_major_y Boolean. Do you need major grid lines for y-axis?
#' @param grid_major_x_size Major X line size.
#' @param grid_major_y_size Major Y line size.
#' @param grid_major_color Major grid lines color.
#' @param grid_minor_x Boolean. Do you need minor grid lines for x-axis?
#' @param grid_minor_y Boolean. Do you need minor grid lines for y-axis?
#' @param grid_minor_x_size Minor X line size.
#' @param grid_minor_y_size Minor Y line size.
#' @param grid_minor_color Minor grid lines color.
#' @param caption_font_family Caption font family. Default to "Carlito".
#' @param caption_font_face Caption font face. Default to "plain". Font face ("plain", "italic", "bold", "bold.italic").
#' @param caption_position_to_plot TRUE or FALSE. Positioning to plot or to panel?
#' @param caption_size The size of the caption. Defaults to 10.
#' @param caption_color Caption color.
#' @param ... Additional arguments passed to [ggplot2::theme()].
#'
#'
#' @description Give some reach colors and fonts to a ggplot.
#'
#' @export
theme_default <- function(
    title_font_family = "Carlito",
    title_size = 20,
    title_color = color("dark_grey"),
    title_font_face = "bold",
    title_hjust = NULL,
    title_position_to_plot = TRUE,
    subtitle_font_family = "Carlito",
    subtitle_size = 16,
    subtitle_color = color("dark_grey"),
    subtitle_font_face = "plain",
    subtitle_hjust = NULL,
    text_font_family = "Carlito",
    text_size = 14,
    text_color = color("dark_grey"),
    text_font_face = "plain",
    panel_background_color = "#FFFFFF",
    panel_border = FALSE,
    panel_border_color = color("dark_grey"),
    legend_position = "top",
    legend_direction = "horizontal",
    legend_justification = "center",
    legend_reverse = TRUE,
    legend_title_size = 14,
    legend_title_color = color("dark_grey"),
    legend_title_font_face = "plain",
    legend_title_font_family = "Carlito",
    legend_text_size = 14,
    legend_text_color = color("dark_grey"),
    legend_text_font_face = "plain",
    legend_text_font_family = "Carlito",
    facet_size = 15,
    facet_color = color("dark_grey"),
    facet_font_face = "bold",
    facet_font_family = "Carlito",
    facet_bg_color = color("lighter_grey"),
    axis_x = TRUE,
    axis_y = TRUE,
    axis_text_x = TRUE,
    axis_line_x = FALSE,
    axis_ticks_x = FALSE,
    axis_text_y = TRUE,
    axis_line_y = TRUE,
    axis_ticks_y = TRUE,
    axis_text_font_family = "Carlito",
    axis_text_size = 14,
    axis_text_color = color("dark_grey"),
    axis_text_font_face = "plain",
    axis_title_size = 15,
    axis_title_color = color("dark_grey"),
    axis_title_font_face = "plain",
    axis_text_x_angle = 0,
    axis_text_x_vjust = 0.5,
    axis_text_x_hjust = 0.5,
    grid_major_x = TRUE,
    grid_major_y = FALSE,
    grid_major_color = color("dark_grey"),
    grid_major_x_size = 0.1,
    grid_major_y_size = 0.1,
    grid_minor_x = TRUE,
    grid_minor_y = FALSE,
    grid_minor_color = color("dark_grey"),
    grid_minor_x_size = 0.05,
    grid_minor_y_size = 0.05,
    caption_font_family = "Carlito",
    caption_font_face = "plain",
    caption_position_to_plot = TRUE,
    caption_size = 12,
    caption_color = color("dark_grey"),
    ...) {
  # Basic simple theme
  theme <- ggplot2::theme_minimal()

  theme <- theme +
    ggplot2::theme(
      # # Text - design
      text = ggplot2::element_text(
        family = text_font_family,
        color = text_color,
        size = text_size,
        face = text_font_face
      ),
      # Default legend to right position
      legend.position = legend_position,
      # Defaut legend to vertical direction
      legend.direction = legend_direction,
      # Text sizes
      axis.text = ggplot2::element_text(
        size = axis_text_size,
        family = axis_text_font_family,
        face = axis_text_font_face,
        color = axis_text_color
      ),
      axis.title = ggplot2::element_text(
        size = axis_title_size,
        family = axis_text_font_family,
        face = axis_title_font_face,
        color = axis_title_color
      ),
      # # Wrap title
      plot.title = ggtext::element_textbox_simple(
        hjust = title_hjust,
        family = title_font_family,
        color = title_color,
        size = title_size,
        face = title_font_face,
        width = grid::unit(0.9, "npc"),
        margin = ggplot2::margin(b = 10)
      ),
      plot.subtitle = ggtext::element_textbox_simple(
        hjust = title_hjust,
        family = subtitle_font_family,
        color = subtitle_color,
        size = subtitle_size,
        face = subtitle_font_face,
        margin = ggplot2::margin(t = 5, b = 10)
      ),
      plot.caption = ggtext::element_textbox_simple(
        size = caption_size,
        face = caption_font_face,
        family = caption_font_family,
        color = caption_color,
        margin = ggplot2::margin(t = 10)
      ),
      legend.title = ggplot2::element_text(
        size = legend_title_size,
        face = legend_title_font_face,
        family = legend_title_font_family,
        color = legend_title_color
      ),
      legend.text = ggplot2::element_text(
        size = legend_text_size,
        face = legend_text_font_face,
        family = legend_text_font_family,
        color = legend_text_color
      ),
      axis.text.x = ggplot2::element_text(
        angle = axis_text_x_angle,
        vjust = axis_text_x_vjust,
        hjust = axis_text_x_hjust
      )
    )

  # Position of title
  if (title_position_to_plot) {
    theme <- theme +
      ggplot2::theme(
        plot.title.position = "plot"
      )
  }

  if (caption_position_to_plot) {
    theme <- theme +
      ggplot2::theme(
        plot.caption.position = "plot"
      )
  }
  # Position of caption

  # Axis lines ?
  if (axis_x & axis_y) {
    theme <- theme +
      ggplot2::theme(
        axis.line = ggplot2::element_line(color = text_color)
      )
  }

  if (!axis_x) {
    theme <- theme +
      ggplot2::theme(
        axis.line.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank()
      )
  }

  if (!axis_y) {
    theme <- theme +
      ggplot2::theme(
        axis.line.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank()
      )
  }

  if (!axis_line_x) {
    theme <- theme +
      ggplot2::theme(
        axis.line.x = ggplot2::element_blank()
      )
  }

  if (!axis_ticks_x) {
    theme <- theme +
      ggplot2::theme(
        axis.ticks.x = ggplot2::element_blank()
      )
  }

  if (!axis_text_x) {
    theme <- theme +
      ggplot2::theme(
        axis.text.x = ggplot2::element_blank()
      )
  }

  if (!axis_line_y) {
    theme <- theme +
      ggplot2::theme(
        axis.line.y = ggplot2::element_blank()
      )
  }

  if (!axis_ticks_y) {
    theme <- theme +
      ggplot2::theme(
        axis.ticks.y = ggplot2::element_blank()
      )
  }

  if (!axis_text_y) {
    theme <- theme +
      ggplot2::theme(
        axis.text.y = ggplot2::element_blank()
      )
  }

  # X - major grid lines
  if (!grid_major_x) {
    theme <- theme +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank()
      )
  } else {
    theme <- theme +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_line(
          color = grid_major_color,
          linewidth = grid_major_x_size
        )
      )
  }

  # Y - major grid lines
  if (!grid_major_y) {
    theme <- theme +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank()
      )
  } else {
    theme <- theme +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_line(
          color = grid_major_color,
          linewidth = grid_major_y_size
        )
      )
  }

  # X - minor grid lines
  if (!grid_minor_x) {
    theme <- theme +
      ggplot2::theme(
        panel.grid.minor.x = ggplot2::element_blank()
      )
  } else {
    theme <- theme +
      ggplot2::theme(
        panel.grid.minor.x = ggplot2::element_line(
          color = grid_minor_color,
          linewidth = grid_minor_x_size
        )
      )
  }

  # Y - minor grid lines
  if (!grid_minor_y) {
    theme <- theme +
      ggplot2::theme(
        panel.grid.minor.y = ggplot2::element_blank()
      )
  } else {
    theme <- theme +
      ggplot2::theme(
        panel.grid.minor.y = ggplot2::element_line(
          color = grid_minor_color,
          linewidth = grid_minor_y_size
        )
      )
  }
  if (!panel_border) {
    theme <- theme +
      ggplot2::theme(
        panel.border = ggplot2::element_blank()
      )
  } else {
    theme <- theme +
      ggplot2::theme(
        panel.border = ggplot2::element_rect(color = panel_background_color)
      )
  }

  # Add facet title text size
  theme <- theme +
    ggplot2::theme(
      strip.text = ggplot2::element_text(
        size = facet_size,
        family = facet_font_family,
        face = facet_font_face,
        color = facet_color
      ),
      strip.background = ggplot2::element_rect(
        fill = facet_bg_color,
        linewidth = 0
      )
    )

  # Other parameters
  theme <- theme + ggplot2::theme(...)

  return(theme)
}
