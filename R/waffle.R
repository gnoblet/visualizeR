#' @title Simple waffle chart
#'
#' @param df A data frame.
#' @param x A character column or coercible as a character column. Will give the waffle's fill color.
#' @param y A numeric column.
#' @param alpha Fill transparency.
#' @param title Plot title. Default to NULL.
#' @param subtitle Plot subtitle. Default to NULL.
#' @param caption Plot caption. Default to NULL.
#' @param arrange TRUE or FALSE. Arrange by highest percentage first.
#' @param theme Whatever theme. Default to theme_reach().
#'
#' @return A waffle chart to be used parsimoniously
#'
#' @export
waffle <- function(df,
                  x,
                  y,
                  alpha = 1,
                  x_title = NULL,
                  title = NULL,
                  subtitle = NULL,
                  caption = NULL,
                  arrange = TRUE,
                  n_rows = 10,
                  size = 0.33,
                  colour = "white",
                  flip = FALSE,
                  theme = theme_reach(

                    axis_x = FALSE,
                    axis_y = FALSE)){

  # Arrange by biggest prop first ?
  if (arrange) df <- dplyr::arrange(
    df,
    dplyr::desc({{ y }})
  )

  # Get levels for scaling
  lev <- dplyr::pull(df, {{ x }})
  df <- dplyr::mutate(df, "{{x}}" := factor({{ x }}, levels = lev))

  # Mapping
  g <- ggplot2::ggplot(
    df,
    mapping = ggplot2::aes(
      fill = {{ x }},
      color = {{ x }},
      values = {{ y }}
    )
  )

  # Add waffle geom
  g <- g +
    waffle::geom_waffle(
      n_rows = n_rows,
      size = size,
      color = "white",
      flip = flip,
      alpha = alpha) +
    ggplot2::coord_equal()

  # Add title, subtitle, caption, x_title, y_title
  g <- g + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    fill = x_title,
    color = x_title
  )

  # # No axis
  # g <- g + ggplot2::theme(
  #   axis.line.x = ggplot2::element_blank(),
  #   axis.ticks.x = ggplot2::element_blank(),
  #   axis.text.x = ggplot2::element_blank(),
  #   axis.title.x = ggplot2::element_blank(),
  #   axis.line.y = ggplot2::element_blank(),
  #   axis.ticks.y = ggplot2::element_blank(),
  #   axis.text.y = ggplot2::element_blank(),
  #   axis.title.y = ggplot2::element_blank()
  # )

  # Basic theme
  g <- g +
    hrbrthemes::theme_ipsum() +
    waffle::theme_enhance_waffle()

  # Add theme
  g <- g + theme

  return(g)

}
