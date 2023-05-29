#' Make dumbbell chart.
#'
#' @param df A data frame.
#' @param col A numeric column.
#' @param group_x The grouping column on the x-axis; only two groups.
#' @param group_y The grouping column on the y-axis.
#' @param point_size Point size.
#' @param point_alpha Point alpha.
#' @param segment_size Segment size.
#' @param segment_color Segment color.
#' @param group_x_title X-group and legend title.
#' @param group_y_title Y-axis and group title.
#' @param x_title X-axis title.
#' @param title Title.
#' @param subtitle Subtitle.
#' @param caption Caption.
#' @param line_to_y_axis TRUE or FALSE; add a line connected points and Y-axis.
#' @param line_to_y_axis_type Line to Y-axis type.
#' @param line_to_y_axis_width Line to Y-axis width.
#' @param line_to_y_axis_color Line to Y-axis color.
#' @param add_text TRUE or FALSE; add text at the points.
#' @param add_text_vjust Vertical adjustment.
#' @param add_text_size Text size.
#' @param add_text_color Text color.
#' @param theme A ggplot2 theme, default to `theme_reach()`
#'
#' @return A dumbbell chart.
#' @export
#'
dumbbell <- function(df,
                     col,
                     group_x,
                     group_y,
                     point_size = 5,
                     point_alpha = 1,
                     segment_size = 2.5,
                     segment_color = cols_reach("main_lt_grey"),
                     group_x_title = NULL,
                     group_y_title = NULL,
                     x_title = NULL,
                     title = NULL,
                     subtitle = NULL,
                     caption = NULL,
                     line_to_y_axis = TRUE,
                     line_to_y_axis_type = 3,
                     line_to_y_axis_width = 0.5,
                     line_to_y_axis_color = cols_reach("main_grey"),
                     add_text = TRUE,
                     add_text_vjust = 2,
                     add_text_size = 3.5,
                     add_text_color = cols_reach("main_grey"),
                     theme = theme_reach(palette = "primary")){

  # Get group keys
  group_x_keys <- df |>
    dplyr::group_by({{ group_x }}) |>
    dplyr::group_keys() |>
    dplyr::pull()

  # Check if only two groups
  if (length(group_x_keys) > 2) rlang::abort("Cannot draw a dumbbell plot for `group_x` with more than 2 groups")

  # Pivot long data
  df_pivot <- df |>
    tidyr::pivot_wider(
      id_cols = c({{ group_y}}),
      values_from = {{ col }},
      names_from = {{ group_x }}
    )

  df_pivot <- df_pivot |>
    dplyr::rowwise() |>
    dplyr::mutate(
      min = min(!!rlang::sym(group_x_keys[[1]]), !!rlang::sym(group_x_keys[[2]]), na.rm = T),
      max = max(!!rlang::sym(group_x_keys[[1]]), !!rlang::sym(group_x_keys[[2]]), na.rm = T)) |>
    dplyr::ungroup() |>
    dplyr::mutate(diff = max - min)

  g <- ggplot2::ggplot(df_pivot)

  # Add line
  if(line_to_y_axis) {

    xend <- min(dplyr::pull(df, {{ col }}))

    g <- g +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = min,
          y = {{ group_y }},
          yend = {{ group_y }}),
        xend = xend,
        linetype = line_to_y_axis_type,
        size = line_to_y_axis_width,
        color = line_to_y_axis_color)
  }

  # Add segment
  g <-  g +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = !!rlang::sym(group_x_keys[[1]]),
        y = {{ group_y }},
        xend = !!rlang::sym(group_x_keys[[2]]),
        yend = {{ group_y }}),
      size = segment_size,
      color = segment_color
    )

  # Add points
  g <- g +
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(
        x = {{ col }},
        y = {{ group_y }},
        color = {{ group_x }},
        fill = {{ group_x }}
      ),
      size = point_size,
      alpha = point_alpha
    )

  # Add title, subtitle, caption, x_title, y_title
  g <- g + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    x = x_title,
    y = group_y_title,
    color = group_x_title,
    fill = group_x_title
  )

  # Add stat labels to points
  if(add_text) g <- g +
    ggrepel::geom_text_repel(
      data = df,
      ggplot2::aes(
        x = {{ col }},
        y = {{ group_y}},
        label = {{ col }}
      ),
      vjust = add_text_vjust,
      size = add_text_size,
      color = add_text_color
    )

  # Expan y axis
  # g <- g +
    # ggplot2::scale_y_discrete(
    #   group_y_title,
    #   expand = c(0, 0))


  # Add theme
  g <- g + theme

  return(g)

}
