#' @title Simple bar chart
#'
#' @param df A data frame.
#' @param x A numeric column.
#' @param y A character column or coercible as a character column.
#' @param group Some grouping categorical column, e.g. administrative areas or population groups.
#' @param flip TRUE or FALSE. Default to TRUE or horizontal bar plot.
#' @param alpha Fill transparency.
#' @param size Point size.
#' @param x_title The x scale title. Default to NULL.
#' @param y_title The y scale title. Default to NULL.
#' @param group_title The group legend title. Default to NULL.
#' @param title Plot title. Default to NULL.
#' @param subtitle Plot subtitle. Default to NULL.
#' @param caption Plot caption. Default to NULL.
#' @param theme Whatever theme. Default to theme_reach().
#'
#' @return A bar chart
#'
#' @export
point <- function(df, x, y, group = NULL, flip = TRUE, alpha = 1, size = 1, x_title = NULL, y_title = NULL, group_title = NULL, title = NULL, subtitle = NULL, caption = NULL, theme = theme_reach()){

  # To do :
  # - automate bar width and text size, or at least give the flexibility and still center text
  # - add facet possibility

  # Prepare group, x and y names
  # if (is.null(x_title)) x_title <- rlang::as_name(rlang::enquo(x))
  # if (is.null(y_title)) y_title <- rlang::as_name(rlang::enquo(y))
  # if (is.null(group_title)) group_title <- rlang::as_name(rlang::enquo(group))

  # Mapping
  g <- ggplot2::ggplot(
    df,
    mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}, fill = {{ group }}, color = {{ group }}
    )
  )

  # Add title, subtitle, caption, x_title, y_title
  g <- g + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = x_title,
    y = y_title,
    color = group_title,
    fill = group_title
  )

  width <- 0.5
  dodge_width <- 0.5

  # Should the graph use position_fill?
  g <- g + ggplot2::geom_point(
      alpha = alpha,
      size = size
    )

  # Labels to percent and expand scale
  # if (percent) {
  #   g <- g + ggplot2::scale_y_continuous(
  #     labels         = scales::label_percent(
  #       accuracy     = 1,
  #       decimal.mark = ",",
  #       suffix       = " %"),
  #     expand = c(0.01, 0.1)
  #   )
  # } else {
  #   g <- g + ggplot2::scale_y_continuous(expand = c(0.01, 0.1))
  # }

  # # Because a text legend should always be horizontal, especially for an horizontal bar graph
  if (flip){
    g <- g + ggplot2::coord_flip()
  }

  # Add theme
  g <- g + theme

  return(g)
}
