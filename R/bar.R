#' @title Simple bar chart
#'
#' @param df A data frame.
#' @param x A numeric column.
#' @param y A character column or coercible as a character column.
#' @param group Some grouping categorical column, e.g. administrative areas or population groups.
#' @param flip TRUE or FALSE. Default to TRUE or horizontal bar plot.
#' @param percent TRUE or FALSE. Should the x-labels (and text labels if present) be displayed as percentages? Default to TRUE.
#' @param position Should the chart be stacked? Default to "dodge". Can take "dodge" and "stack".
#' @param alpha Fill transparency.
#' @param x_title The x scale title. Default to NULL.
#' @param y_title The y scale title. Default to NULL.
#' @param group_title The group legend title. Default to NULL.
#' @param title Plot title. Default to NULL.
#' @param subtitle Plot subtitle. Default to NULL.
#' @param caption Plot caption. Default to NULL.
#' @param add_text TRUE or FALSE. Add the value as text.
#' @param add_text_suffix If percent is FALSE, should we add a suffix to the text label?
#' @param title_wrap Wrap title, number of characters.
#' @param subtitle_wrap Wrap subtitle, number of characters.
#' @param caption_wrap Wrap caption, number of characters.
#' @param theme Whatever theme. Default to theme_reach().
#'
#' @return A bar chart
#'
#' @export
bar <- function(df, x, y, group = NULL, flip = TRUE, percent = TRUE, position = "dodge", alpha = 1,  x_title = NULL, y_title = NULL, group_title = NULL, title = NULL, subtitle = NULL, caption = NULL, add_text = FALSE, add_text_suffix = "", title_wrap = 60, subtitle_wrap = 60, caption_wrap = 120, theme = theme_reach()){

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
    title = stringr::str_wrap(title, title_wrap),
    subtitle = stringr::str_wrap(subtitle, subtitle_wrap),
    caption = stringr::str_wrap(caption, caption_wrap),
    x = x_title,
    y = y_title,
    color = group_title,
    fill = group_title
  )

  width <- 0.5
  dodge_width <- 0.5

  # Should the graph use position_fill?
  if (position == "stack"){
    g <- g + ggplot2::geom_col(
      alpha    = alpha,
      width    = width,
      position = ggplot2::position_stack()
    )
  } else if (position == "dodge"){
    g <- g + ggplot2::geom_col(
      alpha    = alpha,
      width    = width,
      position = ggplot2::position_dodge2(
        width = dodge_width,
        preserve = "single")
    )
  } else{
    g <- g + ggplot2::geom_col(
      alpha = alpha,
      width = width
    )
  }
  #
  # Labels to percent and expand scale
  if (percent) {
    g <- g + ggplot2::scale_y_continuous(
      labels         = scales::label_percent(
        accuracy     = 1,
        decimal.mark = ",",
        suffix       = " %"),
      expand = c(0.01, 0.1)
    )
  } else {
    g <- g + ggplot2::scale_y_continuous(expand = c(0.01, 0.1))
  }

  # Because a text legend should always be horizontal, especially for an horizontal bar graph
  if (flip){
      g <- g + ggplot2::coord_flip()
    }

  # Add text to bars
  if (flip) hjust_flip <- 1.5 else hjust_flip <- 0.5
  if (flip) vjust_flip <- 0.5 else vjust_flip <- 1.5

  if (add_text & position != "dodge") {
    rlang::abort("Adding text labels and positions different than dodges as not been implemented yet")
  }

  # Add text labels
  if (add_text) {
    if (percent) {
      g <- g + ggplot2::geom_text(
        ggplot2::aes(
          label = scales::label_percent(
            accuracy     = 1,
            decimal.mark = ",",
            suffix       = " %")({{ y }}),
          group = {{ group }}),
          hjust = hjust_flip,
          vjust = vjust_flip,
        color = "white",
        fontface = "bold",
        position = ggplot2::position_dodge(width = dodge_width))
    } else {
      g <- g + ggplot2::geom_text(
        ggplot2::aes(
          label = paste0(round({{ y }}), add_text_suffix),
          group = {{ group }}),
        hjust = hjust_flip,
        vjust = vjust_flip,
        color = "white",
        fontface = "bold",
        position = ggplot2::position_dodge(width = dodge_width))
    }
  }

  # Add theme
  g <- g + theme

  return(g)
}
