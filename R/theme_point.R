#' Custom Theme for Point Charts
#'
#' @param flip Logical. Whether the plot is flipped (horizontal).
#' @param axis_text_x_angle Angle for x-axis text.
#' @param axis_text_x_vjust Vertical justification for x-axis text.
#' @param axis_text_x_hjust Horizontal justification for x-axis text.
#'
#' @rdname theme_default
#'
#' @return A custom theme object.
#'
#' @export
theme_point <- function() {
  t <- theme_default(
    axis_text_font_face = "plain",
    axis_x = TRUE,
    axis_y = TRUE,
    grid_major_y = TRUE,
    grid_major_x = TRUE,
    grid_minor_y = FALSE,
    grid_minor_x = FALSE,
    axis_text_x = TRUE,
    axis_line_x = TRUE,
    axis_ticks_x = TRUE,
    axis_text_x_angle = 0,
    axis_text_x_vjust = 0.5,
    axis_text_x_hjust = 0
  )

  return(t)
}
