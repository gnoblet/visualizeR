#' @title Simple alluvial chart
#'
#' @param df A data frame.
#' @param from A character column of upstream stratum.
#' @param to A character column of downstream stratum.
#' @param value A numeric column of values.
#' @param group The grouping column to fill the alluvium with.
#' @param alpha Fill transparency. Default to 0.5.
#' @param from_levels Order by given from levels?
#' @param value_title The value/y scale title. Default to NULL.
#' @param group_title The group title. Default to NULL.
#' @param title Plot title. Default to NULL.
#' @param subtitle Plot subtitle. Default to NULL.
#' @param caption Plot caption. Default to NULL.
#' @param rect_color Stratum rectangles' fill color.
#' @param rect_border_color Stratum rectangles' border color.
#' @param rect_text_color Stratum rectangles' text color.
#' @param theme Whatever theme. Default to theme_reach().
#'
#' @return A donut chart to be used parsimoniously
#'
#' @export
alluvial <- function(
    df,
    from,
    to,
    value,
    group = NULL,
    alpha = 0.5,
    from_levels = NULL,
    value_title = NULL,
    group_title = NULL,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    rect_color = cols_reach("white"),
    rect_border_color = cols_reach("main_grey"),
    rect_text_color = cols_reach("main_grey"),
    theme = theme_reach(axis_y = FALSE,
                        legend_position = "none")
){

  if(!is.null(from_levels)) df <- dplyr::mutate(df, "{{from}}" := factor({{ from }}, levels = from_levels))

  # General mapping
  g <- ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes(
      y = {{ value }},
      axis1 = {{ from }},
      axis3 = {{ to }}
      )
    )

  # Add alluvium
  g <- g +
    ggalluvial::geom_alluvium(
      ggplot2::aes(
        fill = {{ group }},
        color = {{ group }}
      ),
      alpha = alpha)

  # Add stratum
  g <- g +
    ggalluvial::geom_stratum(
      fill = rect_color,
      color = rect_border_color
    )

  # Add stratum text

  stratum <- ggalluvial::StatStratum

  g <- g +
    ggplot2::geom_text(
      stat = stratum,
      ggplot2::aes(label = ggplot2::after_stat(!!rlang::sym("stratum"))),
      color = cols_reach("main_grey")
  )


  # Add title, subtitle, caption, x_title, y_title
  g <- g + ggplot2::labs(
    y = value_title,
    title = title,
    subtitle = subtitle,
    caption = caption,
    fill = group_title,
    color = group_title
  )

  # Remove x-axis
  g <- g + ggplot2::theme(
    axis.line.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank()
  )

  g <- g + theme

  return(g)
}
