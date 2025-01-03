#' @title Simple point chart
#'
#' @param df A data frame.
#' @param x A numeric column.
#' @param y Another numeric column.
#' @param group Some grouping categorical column, e.g. administrative areas or population groups.
#' @param add_color Add a color to bars (if no grouping).
#' @param flip TRUE or FALSE. Default to TRUE or horizontal bar plot.
#' @param alpha Fill transparency.
#' @param size Point size.
#' @param x_title The x scale title. Default to NULL.
#' @param y_title The y scale title. Default to NULL.
#' @param group_title The group legend title. Default to NULL.
#' @param title Plot title. Default to NULL.
#' @param subtitle Plot subtitle. Default to NULL.
#' @param caption Plot caption. Default to NULL.
#' @param theme_fun Whatever theme. Default to theme_reach(). NULL if no theming needed.
#' @param scale_impact Use the package custom scales for fill and color.
#'
#' @inheritParams scale_color_impact_discrete
#'
#' @export
point <- function(df, x, y, group = "", add_color = color("branding_reach_red"), flip = TRUE, alpha = 1, size = 2, x_title = NULL, y_title = NULL, group_title = NULL, title = NULL, subtitle = NULL, caption = NULL, theme_fun = theme_reach(grid_major_y = TRUE), palette = "cat_5_ibm", scale_impact = TRUE, direction = 1, reverse_guide = TRUE) {
  # # Check if numeric and character
  if (!any(c("numeric", "integer") %in% class(df[[x]]))) rlang::abort(paste0(x, " must be numeric."))
  if (!any(c("numeric", "integer") %in% class(df[[y]]))) rlang::abort(paste0(x, " must be numeric."))

  # Mapping
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

  # Should the graph use position_fill?
  if (group != "") {
    g <- g + ggplot2::geom_point(
      alpha = alpha,
      size = size
    )
  } else {
    g <- g + ggplot2::geom_point(
      alpha = alpha,
      size = size,
      color = add_color
    )
  }

  if (flip) {
    g <- g + ggplot2::coord_flip()
  }

  # Add theme
  g <- g + theme_fun


  # Add theme
  if (!is.null(theme_fun)) g <- g + theme_fun

  # Add scale
  if (scale_impact) g <- g + scale_fill_impact_discrete(palette, direction, reverse_guide) + scale_color_impact_discrete(palette, direction, reverse_guide)

  return(g)
}
