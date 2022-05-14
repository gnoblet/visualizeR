#' @title Base REACH ggplot2 theme
#'
#' @param family The font family. Default to "Leelawadee"
#'
#' @description Give some reach colors and fonts to a ggplot. Based on theme_bw()
#'
#' @return The base REACH theme
#'
#' @export
theme_reach <- function(family = "Leelawadee") {

  rlang::check_installed("ggplot2", reason = "Package \"ggplot2\" needed for `theme_reach_*()` to work. Please install it.")

  ggplot2::theme_bw() +
    ggplot2::theme(
      title = ggplot2::element_text(family = family,
                                    size = 12,
                                    colour = "#58585A",
                                    hjust = 0.5,
                                    vjust = 0.5),
      text = ggplot2::element_text(family = family,
                                   colour = "#58585A"),
      axis.title = ggplot2::element_text(size = 11),
      axis.text = ggplot2::element_text(size = 10),
      legend.text = ggplot2::element_text(size = 11),
      strip.text = ggplot2::element_text(size = 11),
      legend.title = ggplot2::element_text(size = 11)
    )
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
#'
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



