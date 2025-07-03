#' @title Simple scatterplot
#'
#' @param df A data frame.
#' @param x A quoted numeric column.
#' @param y A quoted numeric column.
#' @param group Some quoted grouping categorical column, e.g. administrative areas or population groups.
#' @param facet Some quoted grouping categorical column.
#' @param facet_scales Character. Either "free" (default) or "fixed" for facet scales.
#' @param x_rm_na Remove NAs in x?
#' @param y_rm_na Remove NAs in y?
#' @param group_rm_na Remove NAs in group?
#' @param facet_rm_na Remove NAs in facet?
#' @param add_color Add a color to points (if no grouping).
#' @param add_color_guide Should a legend be added?
#' @param flip TRUE or FALSE.
#' @param alpha Fill transparency.
#' @param size Point size.
#' @param x_title The x scale title. Default to NULL.
#' @param y_title The y scale title. Default to NULL.
#' @param group_title The group legend title. Default to NULL.
#' @param title Plot title. Default to NULL.
#' @param subtitle Plot subtitle. Default to NULL.
#' @param caption Plot caption. Default to NULL.
#' @param theme_fun Whatever theme. Default to theme_point(). NULL if no theming needed.
#' @param scale_fill_fun Scale fill function. Default to scale_fill_visualizer_discrete().
#' @param scale_color_fun Scale color function. Default to scale_color_visualizer_discrete().
#'
#' @export
point <- function(
    df,
    x,
    y,
    group = "",
    facet = "",
    facet_scales = "free",
    x_rm_na = TRUE,
    y_rm_na = TRUE,
    group_rm_na = TRUE,
    facet_rm_na = TRUE,
    add_color = color("cat_5_main_1"),
    add_color_guide = TRUE,
    flip = TRUE,
    alpha = 1,
    size = 2,
    x_title = NULL,
    y_title = NULL,
    group_title = NULL,
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    theme_fun = theme_point(),
    scale_fill_fun = scale_fill_visualizer_discrete(),
    scale_color_fun = scale_color_visualizer_discrete()) {
  #------ Checks

  # df is a data frame
  checkmate::assert_data_frame(df)

  # x and y and group are character
  checkmate::assert_character(x, len = 1)
  checkmate::assert_character(y, len = 1)
  checkmate::assert_character(group, len = 1)

  # x and y are columns in df
  checkmate::assert_choice(x, colnames(df))
  checkmate::assert_choice(y, colnames(df))
  if (group != "") {
    checkmate::assert_choice(group, colnames(df))
  }

  # x_rm_na, y_rm_na and group_rm_na are logical scalar
  checkmate::assert_logical(x_rm_na, len = 1)
  checkmate::assert_logical(y_rm_na, len = 1)
  checkmate::assert_logical(group_rm_na, len = 1)
  checkmate::assert_logical(facet_rm_na, len = 1)

  # facet_scales is a character scalar in c("free", "fixed")
  checkmate::assert_choice(facet_scales, c("free", "fixed"))

  # flip is a logical scalar
  checkmate::assert_logical(flip, len = 1)

  # alpha is a numeric scalar between 0 and 1
  checkmate::assert_numeric(alpha, lower = 0, upper = 1, len = 1)

  # size is a numeric scalar
  checkmate::assert_numeric(size, len = 1)

  # x and y are numeric
  if (!any(c("numeric", "integer") %in% class(df[[x]]))) {
    rlang::abort(paste0(x, " must be numeric."))
  }
  if (!any(c("numeric", "integer") %in% class(df[[y]]))) {
    rlang::abort(paste0(y, " must be numeric."))
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
      x = x_title,
      y = y_title,
      color = group_title,
      fill = group_title
    )

  # facets
  # facets
  if (facet != "") {
    if (flip) {
      g <- g +
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!rlang::sym(facet)),
          scales = facet_scales,
          space = if (facet_scales == "free") "free_y" else "fixed"
        )
    } else {
      g <- g +
        ggplot2::facet_grid(
          cols = ggplot2::vars(!!rlang::sym(facet)),
          scales = facet_scales,
          space = if (facet_scales == "free") "free_x" else "fixed"
        )
    }
  }

  # Should the graph use position_fill?
  if (group != "") {
    g <- g +
      ggplot2::geom_point(
        alpha = alpha,
        size = size
      )
  } else {
    g <- g +
      ggplot2::geom_point(
        alpha = alpha,
        size = size,
        color = add_color
      )
  }

  if (flip) {
    g <- g + ggplot2::coord_flip()
  }

  # Remove guides for legend if !add_color_guide
  if (!add_color_guide) {
    g <- g + ggplot2::guides(fill = "none", color = "none")
  }

  # Add theme
  if (!is.null(theme_fun)) {
    g <- g + theme_fun
  }

  # Add scale fun
  if (!is.null(scale_fill_fun)) {
    g <- g + scale_fill_fun
  }

  if (!is.null(scale_color_fun)) {
    g <- g + scale_color_fun
  }

  return(g)
}
