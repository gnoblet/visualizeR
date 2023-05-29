#' @title Simple bar chart
#'
#' @param df A data frame.
#' @param x A numeric column.
#' @param y A character column or coercible as a character column.
#' @param flip TRUE or FALSE. Default to TRUE or horizontal lollipop plot.
#' @param wrap Should x-labels be wrapped? Number of characters.
#' @param arrange TRUE or FALSE. Arrange by highest percentage first.
#' @param point_size Point size.
#' @param point_color Point color.
#' @param point_alpha Point alpha.
#' @param segment_size Segment size.
#' @param segment_color Segment color.
#' @param segment_alpha Segment alpha.
#' @param alpha Fill transparency.
#' @param x_title The x scale title. Default to NULL.
#' @param y_title The y scale title. Default to NULL.
#' @param title Plot title. Default to NULL.
#' @param subtitle Plot subtitle. Default to NULL.
#' @param caption Plot caption. Default to NULL.
#' @param add_text TRUE or FALSE. Add the y value as text within the bubble.
#' @param add_text_size Text size.
#' @param add_text_suffix If percent is FALSE, should we add a suffix to the text label?
#' @param add_text_color Added text color. Default to white.
#' @param add_text_fontface Added text font face. Default to "bold".
#' @param theme Whatever theme. Default to theme_reach().
#'
#' @return A bar chart
#'
#' @export
lollipop <- function(df,
                     x,
                     y,
                     flip = TRUE,
                     wrap = NULL,
                     arrange = TRUE,
                     point_size = 3,
                     point_color = cols_reach("main_red"),
                     point_alpha = 1,
                     segment_size = 1,
                     segment_color = cols_reach("main_grey"),
                     segment_alpha = 1,
                     alpha = 1,
                     x_title = NULL,
                     y_title = NULL,
                     title = NULL,
                     subtitle = NULL,
                     caption = NULL,
                     add_text = FALSE,
                     add_text_size = 3,
                     add_text_suffix = "",
                     add_text_color = "white",
                     add_text_fontface = "bold",
                     theme = theme_reach()){


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
    mapping = ggplot2::aes(x = {{ x }}, y = {{ y }}, xend = {{ x }}, yend = 0)
  )

  #  Add segment
  g <- g + ggplot2::geom_segment(
    linewidth = segment_size,
    alpha = segment_alpha,
    color = segment_color
  )

  g <- g + ggplot2::geom_point(
    size = point_size,
    alpha = point_alpha,
    color = point_color
  )

  if (!is.null(wrap)) {
    g <- g + ggplot2::scale_x_discrete(labels = scales::label_wrap(wrap))
  }

  # Because a text legend should always be horizontal, especially for an horizontal bar graph
  if (flip){
    g <- g + ggplot2::coord_flip()
  }

  # Add text labels
  if (add_text) {
      g <- g + ggplot2::geom_text(
        ggplot2::aes(
          label = paste0({{ y }}, add_text_suffix)),
        size = add_text_size,
        color = add_text_color,
        fontface = add_text_fontface)
    }

  # Add title, subtitle, caption, x_title, y_title
  g <- g + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = x_title,
    y = y_title,
  )


  # Add theme
  g <- g + theme

  return(g)

}

