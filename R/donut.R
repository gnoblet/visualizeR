#' @title Simple donut chart (to be used parsimoniously), can be a pie chart
#'
#' @param df A data frame.
#' @param x A character column or coercible as a character column. Will give the donut's fill color.
#' @param y A numeric column.
#' @param alpha Fill transparency.
#' @param x_title The x scale title. Default to NULL.
#' @param title Plot title. Default to NULL.
#' @param subtitle Plot subtitle. Default to NULL.
#' @param caption Plot caption. Default to NULL.
#' @param arrange TRUE or FALSE. Arrange by highest percentage first.
#' @param hole_size Hole size. Default to 3. If less than 2, back to a pie chart.
#' @param add_text TRUE or FALSE. Add the value as text.
#' @param add_text_threshold_display Minimum value to add the text label.
#' @param add_text_color Text color.
#' @param add_text_suffix If percent is FALSE, should we add a suffix to the text label?
#' @param theme Whatever theme. Default to theme_reach().
#'
#' @return A donut chart to be used parsimoniously
#'
#' @export
donut <- function(df,
                  x,
                  y,
                  alpha = 1,
                  x_title = NULL,
                  title = NULL,
                  subtitle = NULL,
                  caption = NULL,
                  arrange = TRUE,
                  hole_size = 3,
                  add_text = TRUE,
                  add_text_treshold_display = 5, add_text_color = "white", add_text_suffix = "", theme = theme_reach(legend_reverse = TRUE, axis_x = FALSE)){

  # Arrange by biggest prop first ?
  if (arrange) df <- dplyr::arrange(
    df,
    {{ y }}
  )

  # Get levels for scaling
  lev <- dplyr::pull(df, {{ x }})
  df <- dplyr::mutate(df, "{{x}}" := factor({{ x }}, levels = lev))

  # Mapping
  g <- ggplot2::ggplot(
    df,
    mapping = ggplot2::aes(
      x = hole_size,
      y = {{ y }},
      fill = {{ x }},
      color = {{ x }}
  )
  )

  # Add rect
  g <- g + ggplot2::geom_col(alpha = alpha)


  # Add text labels
  if (add_text) {

    df <- dplyr::mutate(df, y_treshold = ifelse({{ y }} >= add_text_treshold_display, {{ y }}, NA ))

    g <- g +
      ggplot2::geom_text(
        data = df,
        ggplot2::aes(
          x = hole_size,
          y = !!rlang::sym("y_treshold"),
          label = paste0({{ y }}, add_text_suffix)),
        color = add_text_color,
       position = ggplot2::position_stack(vjust = 0.5))
  }

  # Add title, subtitle, caption, x_title, y_title
  g <- g + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    fill = x_title,
    color = x_title
  )

  # Transform to polar coordinates and adjust hole
  g <- g +
    ggplot2::coord_polar(
      theta = "y"
    )

  if (hole_size >= 2) g <- g + ggplot2::xlim(c(1, hole_size + 0.5)) # Try to remove that to see how to make a pie chart

  # Add theme
  g <- g + theme

  # No axis
  g <- g + ggplot2::theme(
    axis.text = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank()
  )


  return(g)

}
