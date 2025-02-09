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
#' @param theme_fun A ggplot2 theme, default to `theme_dumbbell()`
#' @param scale_fill_fun A ggplot2 scale_fill function, default to `scale_fill_visualizer_discrete()`
#' @param scale_color_fun A ggplot2 scale_color function, default to `scale_color_visualizer_discrete()`
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
                     segment_color = color("light_blue_grey"),
                     group_x_title = NULL,
                     group_y_title = NULL,
                     x_title = NULL,
                     title = NULL,
                     subtitle = NULL,
                     caption = NULL,
                     line_to_y_axis = FALSE,
                     line_to_y_axis_type = 3,
                     line_to_y_axis_width = 0.5,
                     line_to_y_axis_color = color("dark_grey"),
                     add_text = FALSE,
                     add_text_vjust = 2,
                     add_text_size = 3.5,
                     add_text_color = color("dark_grey"),
                     theme_fun = theme_dumbbell(),
                     scale_fill_fun = scale_fill_visualizer_discrete(),
                     scale_color_fun = scale_color_visualizer_discrete()){

  #------ Checks

  # df is a data frame
  checkmate::assert_data_frame(df)

  # col, group_x, group_y are character
  checkmate::assert_character(col, len = 1)
  checkmate::assert_character(group_x, len = 1)
  checkmate::assert_character(group_y, len = 1)

  # col, group_x, group_y are columns in df
  checkmate::assert_choice(col, colnames(df))
  checkmate::assert_choice(group_x, colnames(df))
  checkmate::assert_choice(group_y, colnames(df))

  # Check numeric/logical values
  checkmate::assert_numeric(point_size, len = 1)
  checkmate::assert_numeric(point_alpha, lower = 0, upper = 1, len = 1)
  checkmate::assert_numeric(segment_size, len = 1)
  checkmate::assert_logical(line_to_y_axis, len = 1)
  checkmate::assert_numeric(line_to_y_axis_type, len = 1)
  checkmate::assert_numeric(line_to_y_axis_width, len = 1)
  checkmate::assert_logical(add_text, len = 1)
  checkmate::assert_numeric(add_text_vjust, len = 1)
  checkmate::assert_numeric(add_text_size, len = 1)

  # Get group keys
  group_x_keys <- df |>
    dplyr::group_by(!!rlang::sym(group_x)) |>
    dplyr::group_keys() |>
    dplyr::pull()

  # Check if only two groups
  if (length(group_x_keys) > 2) rlang::abort("Cannot draw a dumbbell plot for `group_x` with more than 2 groups")

  # Pivot long data
  df_pivot <- df |>
    tidyr::pivot_wider(
      id_cols = c(!!rlang::sym(group_y)),
      values_from = !!rlang::sym(col),
      names_from = !!rlang::sym(group_x)
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

    xend <- min(dplyr::pull(df, !!rlang::sym(col)))

    g <- g +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = min,
          y = !!rlang::sym(group_y),
          yend = !!rlang::sym(group_y)),
        xend = xend,
        linetype = line_to_y_axis_type,
        linewidth = line_to_y_axis_width,
        color = line_to_y_axis_color)
  }

  # Add segment
  g <-  g +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = !!rlang::sym(group_x_keys[[1]]),
        y = !!rlang::sym(group_y),
        xend = !!rlang::sym(group_x_keys[[2]]),
        yend = !!rlang::sym(group_y)),
      linewidth = segment_size,
      color = segment_color
    )

  # Add points
  g <- g +
    ggplot2::geom_point(
      data = df,
      ggplot2::aes(
        x = !!rlang::sym(col),
        y = !!rlang::sym(group_y),
        color = !!rlang::sym(group_x),
        fill = !!rlang::sym(group_x)
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
        x = !!rlang::sym(col),
        y = !!rlang::sym(group_y),
        label = !!rlang::sym(col)
      ),
      vjust = add_text_vjust,
      size = add_text_size,
      color = add_text_color
    )

  # Add theme
  g <- g + theme_fun

  # Add scale fun
  if (!is.null(scale_fill_fun)) g <- g + scale_fill_fun

  if (!is.null(scale_color_fun)) g <- g + scale_color_fun

  return(g)

}
