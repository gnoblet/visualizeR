#' @rdname lollipop
#'
#' @inheritParams lollipop
#'
#' @export
hlollipop <- function(
    ...,
    flip = TRUE,
    theme_fun = theme_lollipop(flip = flip)) {
  lollipop(flip = flip, theme_fun = theme_fun, ...)
}

#' Simple lollipop chart
#'
#' `lollipop()` is a simple lollipop chart (dots connected to the baseline by a segment) with some customization allowed.
#' `hlollipop()` uses `lollipop()` with sane defaults for a horizontal lollipop chart.
#'
#' @param df A data frame.
#' @param x A quoted character column or coercible as a character column.
#' @param y A quoted numeric column.
#' @param group Some quoted grouping categorical column, e.g. administrative areas or population groups.
#' @param facet Some quoted grouping categorical column, e.g. administrative areas or population groups.
#' @param x_rm_na Remove NAs in x?
#' @param y_rm_na Remove NAs in y?
#' @param group_rm_na Remove NAs in group?
#' @param facet_rm_na Remove NAs in facet?
#' @param y_expand Multiplier to expand the y axis.
#' @param add_color Add a color to dots (if no grouping).
#' @param add_color_guide Should a legend be added?
#' @param flip TRUE or FALSE (default). Default to TRUE or horizontal lollipop plot.
#' @param wrap Should x-labels be wrapped? Number of characters.
#' @param alpha Fill transparency for dots.
#' @param x_title The x scale title. Default to NULL.
#' @param y_title The y scale title. Default to NULL.
#' @param group_title The group legend title. Default to NULL.
#' @param title Plot title. Default to NULL.
#' @param subtitle Plot subtitle. Default to NULL.
#' @param caption Plot caption. Default to NULL.
#' @param dot_size The size of the dots.
#' @param line_size The size/width of the line connecting dots to the baseline.
#' @param line_color The color of the line connecting dots to the baseline.
#' @param dodge_width Width for position dodge when using groups (controls space between grouped lollipops).
#' @param theme_fun Whatever theme function. For no custom theme, use theme_fun = NULL.
#' @param scale_fill_fun Scale fill function.
#' @param scale_color_fun Scale color function.
#'
#' @inheritParams reorder_by
#'
#' @importFrom rlang `:=`
#'
#' @export
lollipop <- function(
    df,
    x,
    y,
    group = "",
    facet = "",
    order = "y",
    x_rm_na = TRUE,
    y_rm_na = TRUE,
    group_rm_na = TRUE,
    facet_rm_na = TRUE,
    y_expand = 0.1,
    add_color = color("cat_5_main_1"),
    add_color_guide = TRUE,
    flip = FALSE,
    wrap = NULL,
    alpha = 1,
    x_title = NULL,
    y_title = NULL,
    group_title = NULL,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    dot_size = 4,
    line_size = 0.8,
    line_color = color("dark_grey"),
    dodge_width = 0.9,
    theme_fun = theme_lollipop(
      flip = flip,
      axis_text_x_angle = 0,
      axis_text_x_vjust = 0.5,
      axis_text_x_hjust = 0.5
    ),
    scale_fill_fun = scale_fill_visualizer_discrete(),
    scale_color_fun = scale_color_visualizer_discrete()) {
  #------ Checks

  # df is a data frame
  checkmate::assert_data_frame(df)

  # x and y and group are character
  checkmate::assert_character(x, len = 1)
  checkmate::assert_character(y, len = 1)
  checkmate::assert_character(group, len = 1)
  checkmate::assert_character(facet, len = 1)

  # x and y are columns in df
  checkmate::assert_choice(x, colnames(df))
  checkmate::assert_choice(y, colnames(df))
  if (group != "") {
    checkmate::assert_choice(group, colnames(df))
  }
  if (facet != "") {
    checkmate::assert_choice(facet, colnames(df))
  }

  # x_rm_na, y_rm_na and group_rm_na are logical scalar
  checkmate::assert_logical(x_rm_na, len = 1)
  checkmate::assert_logical(y_rm_na, len = 1)
  checkmate::assert_logical(group_rm_na, len = 1)
  checkmate::assert_logical(facet_rm_na, len = 1)

  # flip is a logical scalar
  checkmate::assert_logical(flip, len = 1)

  # dodge_width is a numeric scalar
  checkmate::assert_numeric(dodge_width, len = 1, lower = 0)

  # wrap is a numeric scalar or NULL
  if (!is.null(wrap)) {
    checkmate::assert_numeric(wrap, len = 1, null.ok = TRUE)
  }

  # alpha is a numeric scalar between 0 and 1
  checkmate::assert_numeric(alpha, lower = 0, upper = 1, len = 1)

  # dot_size is a numeric scalar
  checkmate::assert_numeric(dot_size, len = 1)

  # line_size is a numeric scalar
  checkmate::assert_numeric(line_size, len = 1)

  # order is a character scalar in valid choices
  checkmate::assert_choice(order, c("none", "y", "grouped_y", "x", "grouped_x"))

  # x and y are numeric or character
  if (class(df[[y]]) %notin% c("integer", "numeric")) {
    rlang::abort(paste0(y, " must be numeric."))
  }
  if (!any(class(df[[x]]) %in% c("character", "factor"))) {
    rlang::abort(paste0(x, " must be character or factor"))
  }

  #----- Data wrangling

  # facets over group
  if (group != "" && facet != "" && group == facet) {
    rlang::warn("'group' and 'facet' are the same identical.")
  }

  # remove NAs using base R
  if (x_rm_na) {
    df <- df[!(is.na(df[[x]])), ]
  }
  if (y_rm_na) {
    df <- df[!(is.na(df[[y]])), ]
  }
  if (group != "" && group_rm_na) {
    df <- df[!(is.na(df[[group]])), ]
  }
  if (facet != "" && facet_rm_na) {
    df <- df[!(is.na(df[[facet]])), ]
  }

  # reorder
  dir_order <- if (flip && order %in% c("x", "grouped_x")) {
    -1
  } else if (!flip && order %in% c("x", "grouped_x")) {
    1
  } else if (flip) {
    1
  } else {
    -1
  }
  group_order <- if (group != "" || (group == "" && facet == "")) {
    group
  } else if (group == "" && facet != "") {
    facet
  }
  df <- reorder_by(
    df = df,
    x = x,
    y = y,
    group = group_order,
    order = order,
    dir_order = dir_order
  )

  # prepare aes
  if (group != "") {
    g <- ggplot2::ggplot(
      df,
      mapping = ggplot2::aes(
        x = !!rlang::sym(x),
        y = !!rlang::sym(y),
        fill = !!rlang::sym(group),
        color = !!rlang::sym(group)
      )
    )
  } else {
    g <- ggplot2::ggplot(
      df,
      mapping = ggplot2::aes(
        x = !!rlang::sym(x),
        y = !!rlang::sym(y)
      )
    )
  }

  # add title, subtitle, caption, x_title, y_title
  g <- g +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      x = y_title,
      y = x_title,
      color = group_title,
      fill = group_title
    )

  # facets
  if (facet != "") {
    if (flip) {
      g <- g +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!rlang::sym(facet)),
          scales = "free",
          space = "free_y"
        )
    } else {
      g <- g +
        ggplot2::facet_grid(
          cols = ggplot2::vars(!!rlang::sym(facet)),
          scales = "free",
          space = "free_x"
        )
    }
  }

  # Add segments and points
  if (group != "") {
    # With grouping - use position_dodge for side-by-side display
    position_dodge_obj <- ggplot2::position_dodge(width = dodge_width)

    g <- g +
      ggplot2::geom_linerange(
        mapping = ggplot2::aes(
          ymin = 0,
          ymax = !!rlang::sym(y),
          group = !!rlang::sym(group)
        ),
        position = position_dodge_obj,
        color = line_color,
        linewidth = line_size
      ) +
      ggplot2::geom_point(
        position = position_dodge_obj,
        size = dot_size,
        alpha = alpha
      )
  } else {
    # Without grouping
    g <- g +
      ggplot2::geom_linerange(
        mapping = ggplot2::aes(
          ymin = 0,
          ymax = !!rlang::sym(y)
        ),
        color = line_color,
        linewidth = line_size
      ) +
      ggplot2::geom_point(
        size = dot_size,
        alpha = alpha,
        color = add_color,
        fill = add_color
      )
  }

  # wrap labels on the x scale?
  if (!is.null(wrap)) {
    g <- g + ggplot2::scale_x_discrete(labels = scales::label_wrap(wrap))
  }

  # flip coordinates if needed
  if (flip) {
    g <- g + ggplot2::coord_flip()
  }

  # y scale tweaks
  g <- g +
    ggplot2::scale_y_continuous(
      # start at 0
      expand = ggplot2::expansion(mult = c(0, y_expand)),
      # remove trailing 0 and choose accuracy of y labels
      labels = scales::label_number(
        accuracy = 0.1,
        drop0trailing = TRUE,
        big.mark = "",
        decimal.mark = "."
      ),
    )

  # remove guides for legend if !add_color_guide
  if (!add_color_guide) {
    g <- g + ggplot2::guides(fill = "none", color = "none")
  }

  # add theme fun
  if (!is.null(theme_fun)) {
    g <- g + theme_fun
  }

  # add scale fun
  if (!is.null(scale_fill_fun)) {
    g <- g + scale_fill_fun
  }

  if (!is.null(scale_color_fun)) {
    g <- g + scale_color_fun
  }

  return(g)
}
