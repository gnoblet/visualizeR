#' Custom Theme for Lollipop Charts
#'
#' @description
#' A custom theme specifically designed for lollipop charts with appropriate grid lines and axis styling
#' based on whether the chart is flipped (horizontal) or not.
#'
#' @param flip Logical indicating whether the lollipop chart is flipped (horizontal). Default is TRUE.
#' @param axis_text_x_angle Angle for x-axis text labels. Default is 0.
#' @param axis_text_x_vjust Vertical justification for x-axis text labels. Default is 0.5.
#' @param axis_text_x_hjust Horizontal justification for x-axis text labels. Default is 0.5.
#'
#' @return A ggplot2 theme object
#'
#' @rdname theme_default
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' df <- data.frame(x = letters[1:5], y = c(10, 5, 7, 12, 8))
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   theme_lollipop()
#' }
theme_lollipop <- function(
    flip = TRUE,
    axis_text_x_angle = 0,
    axis_text_x_vjust = 0.5,
    axis_text_x_hjust = 0.5) {
  # Set parameters based on flip
  if (!flip) {
    par_axis_text_font_face <- "plain"
    par_axis_x <- TRUE
    par_axis_y <- TRUE
    par_axis_line_y <- FALSE
    par_axis_ticks_y <- TRUE
    par_axis_text_y <- TRUE
    par_axis_line_x <- TRUE
    par_axis_ticks_x <- TRUE
    par_axis_text_x <- TRUE
    par_grid_major_y <- TRUE
    par_grid_major_x <- FALSE
    par_grid_minor_y <- TRUE
    par_grid_minor_x <- FALSE
  } else if (flip) {
    par_axis_text_font_face <- "plain"
    par_axis_x <- TRUE
    par_axis_y <- TRUE
    par_axis_line_y <- TRUE
    par_axis_ticks_y <- TRUE
    par_axis_text_y <- TRUE
    par_axis_line_x <- FALSE
    par_axis_ticks_x <- TRUE
    par_axis_text_x <- TRUE
    par_grid_major_y <- FALSE
    par_grid_major_x <- TRUE
    par_grid_minor_y <- FALSE
    par_grid_minor_x <- TRUE
  }

  # Theme
  t <- theme_default(
    axis_text_font_face = par_axis_text_font_face,
    axis_x = par_axis_x,
    axis_y = par_axis_y,
    grid_major_y = par_grid_major_y,
    grid_major_x = par_grid_major_x,
    grid_minor_y = par_grid_minor_y,
    grid_minor_x = par_grid_minor_x,
    axis_text_y = par_axis_text_y,
    axis_line_y = par_axis_line_y,
    axis_ticks_y = par_axis_ticks_y,
    axis_text_x = par_axis_text_x,
    axis_line_x = par_axis_line_x,
    axis_ticks_x = par_axis_ticks_x,
    axis_text_x_angle = axis_text_x_angle,
    axis_text_x_vjust = axis_text_x_vjust,
    axis_text_x_hjust = axis_text_x_hjust
  )

  return(t)
}
