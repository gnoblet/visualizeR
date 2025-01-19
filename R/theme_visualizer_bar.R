#' Dynamic Theme for ggplot2
#'
#' A dynamic theme that adjusts axis text styles based on whether the plot is flipped.
#'
#' This function dynamically applies different axis text styles depending on
#' the coordinate system of the plot. If the plot is flipped (e.g., using
#' `coord_flip()`), the x-axis and y-axis text styles are adjusted accordingly.
#'
#' @return A ggproto object that applies a dynamic theme to a ggplot2 plot.
#' @examples
#' library(ggplot2)
#'
#' # Example with a regular plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_col()
#'
#' # Add the dynamic theme
#' p + theme_visualizer_bar()
#'
#' # Add the dynamic theme with a flipped coordinate system
#' p + theme_visualizer_bar() + coord_flip()
#'
#' @export
theme_visualizer_bar <- function() {
  out <- theme_grey()
  class(out) <- c("ThemeVisualizerBar", class(out))

  #structure(list(), class = c("ThemeVisualizerBar", "theme", "gg"))
  return(out)
}



ggplot_add.theme_visualizer_bar <- function(object, p, object_name) {
   # Check if the plot is flipped
   is_flipped <- inherits(p$coordinates, "CoordFlip")

   if (!is_flipped) {
    object <- object +
      theme_minimal()
  } else {
      object <- object +
      theme(
        panel.grid.major = ggplot2::element_line(color = "blue")
      )
  }

  return(object)
}
