#' Custom Theme for Bar Charts
#'
#' @return A custom theme object.
#'
#' @rdname theme_default
#'
#' @export
theme_bar <- function(flip = TRUE, add_text = FALSE, axis_text_x_angle = 0, axis_text_x_vjust = 0.5, axis_text_x_hjust = 0.5) {

  # If add_text is TRUE, flip is FALSE
  if (!flip && !add_text){
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
  } else if (flip && !add_text){
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
  } else if (!flip && add_text){
    par_axis_text_font_face <- "bold"
    par_axis_x <- TRUE
    par_axis_y <- TRUE
    par_axis_line_y <- FALSE
    par_axis_ticks_y <- FALSE
    par_axis_text_y <- FALSE
    par_axis_line_x <- FALSE
    par_axis_ticks_x <- TRUE
    par_axis_text_x <- TRUE
    par_grid_major_y <- FALSE
    par_grid_major_x <- FALSE
    par_grid_minor_y <- FALSE
    par_grid_minor_x <- FALSE
  } else if (flip && add_text){
    par_axis_text_font_face <- "bold"
    par_axis_x <- TRUE
    par_axis_y <- TRUE
    par_axis_line_y <- FALSE
    par_axis_ticks_y <- TRUE
    par_axis_text_y <- TRUE
    par_axis_line_x <- FALSE
    par_axis_ticks_x <- FALSE
    par_axis_text_x <- FALSE
    par_grid_major_y <- FALSE
    par_grid_major_x <- FALSE
    par_grid_minor_y <- FALSE
    par_grid_minor_x <- FALSE
  }

  # Theme
  t <- theme_default(
    axis_text_font_face = par_axis_text_font_face
    , axis_x = par_axis_x
    , axis_y = par_axis_y
    , grid_major_y = par_grid_major_y
    , grid_major_x = par_grid_major_x
    , grid_minor_y = par_grid_minor_y
    , grid_minor_x = par_grid_minor_x
    , axis_text_y = par_axis_text_y
    , axis_line_y = par_axis_line_y
    , axis_ticks_y = par_axis_ticks_y
    , axis_text_x = par_axis_text_x
    , axis_line_x = par_axis_line_x
    , axis_ticks_x = par_axis_ticks_x
    , axis_text_x_angle = axis_text_x_angle
    , axis_text_x_vjust = axis_text_x_vjust
    , axis_text_x_hjust = axis_text_x_hjust
  )

  return(t)
}
