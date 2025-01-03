#' Simple bar chart
#'
#' @param df A data frame.
#' @param x A quoted numeric column.
#' @param y A quoted character column or coercible as a character column.
#' @param group Some quoted grouping categorical column, e.g. administrative areas or population groups.
#' @param add_color Add a color to bars (if no grouping).
#' @param flip TRUE or FALSE. Default to TRUE or horizontal bar plot.
#' @param wrap Should x-labels be wrapped? Number of characters.
#' @param position Should the chart be stacked? Default to "dodge". Can take "dodge" and "stack".
#' @param alpha Fill transparency.
#' @param x_title The x scale title. Default to NULL.
#' @param y_title The y scale title. Default to NULL.
#' @param group_title The group legend title. Default to NULL.
#' @param title Plot title. Default to NULL.
#' @param subtitle Plot subtitle. Default to NULL.
#' @param caption Plot caption. Default to NULL.
#' @param width Bar width.
#' @param add_text TRUE or FALSE. Add values as text.
#' @param add_text_size Text size.
#' @param add_text_color Text color.
#' @param add_text_font_face Text font_face.
#' @param add_text_threshold_display Minimum value to add the text label.
#' @param add_text_suffix If percent is FALSE, should we add a suffix to the text label?
#' @param add_text_expand_limit Default to adding 10% on top of the bar.
#' @param add_text_round Round the text label.
#' @param theme_fun Whatever theme function. For no custom theme, use theme_fun = NULL.
#' @param scale_impact Use the package custom scales for fill and color.
#'
#' @inheritParams scale_color_impact_discrete
#'
#' @importFrom rlang `%||%`
#'
#' @export
bar <- function(
  df,
  x,
  y,
  group = "",
  add_color = color("dark_grey"),
  flip = TRUE,
  wrap = NULL,
  position = "dodge",
  alpha = 1,
  x_title = NULL,
  y_title = NULL,
  group_title = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  width = 0.5,
  add_text = TRUE,
  add_text_size = 5,
  add_text_color = color("dark_grey"),
  add_text_font_face = "plain",
  add_text_threshold_display = 0.05,
  add_text_suffix = "%",
  add_text_expand_limit = 1.2,
  add_text_round = 1){

# Check if numeric and character
if (class(df[[y]]) %notin% c("integer", "numeric")) rlang::abort(paste0(y, " must be numeric."))
if (!any(class(df[[x]]) %in% c("character", "factor"))) rlang::abort(paste0(x, " must be character or factor"))

# Check if position is stack or dodge
if (position %notin% c("stack", "dodge")) rlang::abort("Position should be either 'stack' or 'dodge'.")

if(group != "") {

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
  x = y_title,
  y = x_title,
  color = group_title,
  fill = group_title
)

width <- width
dodge_width <- width

# Should the graph use position_fill?
if(group != "") {

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

} else {

  if (position == "stack"){
    g <- g + ggplot2::geom_col(
      alpha    = alpha,
      width    = width,
      position = ggplot2::position_stack(),
      fill = add_color,
      color = add_color
    )
  } else if (position == "dodge"){
    g <- g + ggplot2::geom_col(
      alpha    = alpha,
      width    = width,
      position = ggplot2::position_dodge2(
        width = dodge_width,
        preserve = "single"),
      fill = add_color,
      color = add_color
    )
  } else {
    g <- g + ggplot2::geom_col(
      alpha = alpha,
      width = width,
      fill = add_color,
      color = add_color
    )
  }
}

# Expand scale
g <- g + ggplot2::scale_y_continuous(expand = c(0, 0))

if (!is.null(wrap)) {
  g <- g + ggplot2::scale_x_discrete(labels = scales::label_wrap(wrap))
}


# Because a text legend should always be horizontal, especially for an horizontal bar graph
if (flip) g <- g + ggplot2::coord_flip()
# Add text to bars
if (flip) hjust_flip <- -0.5 else hjust_flip <- 0.5
if (flip) vjust_flip <- 0.5 else vjust_flip <- -0.5


# Add text labels
if (add_text & position == "dodge") {

  df <- dplyr::mutate(df, "y_threshold" := ifelse(!!rlang::sym(y) >= add_text_threshold_display, !!rlang::sym(y), NA ))

  # Expand limits
  g <- g + ggplot2::geom_blank(
    data = df,
    ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y) * add_text_expand_limit, group = !!rlang::sym(group))
  )

  g <- g + ggplot2::geom_text(
    data = df,
    ggplot2::aes(
      label = ifelse(is.na(!!rlang::sym("y_threshold")), NA, paste0(round(!!rlang::sym("y_threshold"), add_text_round), add_text_suffix)),
      group = !!rlang::sym(group)),
    hjust = hjust_flip,
    vjust = vjust_flip,
    color = add_text_color,
    fontface = add_text_font_face,
    size = add_text_size,
    position = ggplot2::position_dodge2(width = dodge_width)
  )


} else if (add_text & position == "stack") {

  df <- dplyr::mutate(df, "y_threshold" := ifelse(!!rlang::sym(y) >= add_text_threshold_display, !!rlang::sym(y), NA ))

  g <- g + ggplot2::geom_text(
    data = df,
    ggplot2::aes(
      label = ifelse(is.na(!!rlang::sym("y_threshold")), NA, paste0(round(!!rlang::sym("y_threshold"), add_text_round), add_text_suffix)),
      group = !!rlang::sym(group)),
    color = add_text_color,
    fontface = add_text_font_face,
    size = add_text_size,
    position = ggplot2::position_stack(vjust = 0.5)
  )

}

# Remove trailing 0
  ! no applicable method for 'round_any' applied to an object of class "character"


return(g)
}