#' @rdname bar
#'
#' @inheritParams bar
#'
#' @export
hbar <- function(
  ...,
  flip = TRUE,
  add_text = FALSE,
  theme_fun = theme_bar(flip = flip, add_text = add_text)
) {
  bar(flip = flip, add_text = add_text, theme_fun = theme_fun, ...)
}

#' Simple bar chart
#'
#' `bar()` is a simple bar chart with some customization allowed, in particular the `theme_fun` argument for theming. `hbar()` uses `bar()` with sane defaults for a horizontal bar chart.
#'
#' @param df A data frame.
#' @param x A quoted numeric column.
#' @param y A quoted character column or coercible as a character column.
#' @param group Some quoted grouping categorical column, e.g. administrative areas or population groups.
#' @param facet Some quoted grouping categorical column, e.g. administrative areas or population groups.
#' @param order Should bars be ordered? "none" if no, "y" if yes based on y, "grouped" if yes based on y and group.
#' @param x_rm_na Remove NAs in x?
#' @param y_rm_na Remove NAs in y?
#' @param group_rm_na Remove NAs in group?
#' @param facet_rm_na Remove NAs in facet?
#' @param y_expand Multiplier to expand the y axis.
#' @param add_color Add a color to bars (if no grouping).
#' @param add_color_guide Should a legend be added?
#' @param flip TRUE or FALSE (default). Default to TRUE or horizontal bar plot.
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
#' @param add_text_expand_limit Default to adding 10\% on top of the bar.
#' @param add_text_round Round the text label.
#' @param theme_fun Whatever theme function. For no custom theme, use theme_fun = NULL.
#'
#' @inheritParams reorder_by
#'
#' @importFrom rlang `:=`
#'
#' @export
bar <- function(
  df,
  x,
  y,
  group = "",
  facet = "",
  order = "none",
  x_rm_na = TRUE,
  y_rm_na = TRUE,
  group_rm_na = TRUE,
  facet_rm_na = TRUE,
  y_expand = 0.1,
  add_color = color("cat_5_main_1"),
  add_color_guide = TRUE,
  flip = FALSE,
  wrap = NULL,
  position = "dodge",
  alpha = 1,
  x_title = NULL,
  y_title = NULL,
  group_title = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  width = 0.8,
  add_text = FALSE,
  add_text_size = 4.5,
  add_text_color = color("dark_grey"),
  add_text_font_face = "bold",
  add_text_threshold_display = 0.05,
  add_text_suffix = "%",
  add_text_expand_limit = 1.2,
  add_text_round = 1,
  theme_fun = theme_bar(
    flip = flip,
    add_text = add_text,
    axis_text_x_angle = 0,
    axis_text_x_vjust = 0.5,
    axis_text_x_hjust = 0.5
  ),
  scale_fill_fun = scale_fill_visualizer_discrete(),
  scale_color_fun = scale_color_visualizer_discrete()
) {
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

  # flip is a logical scalar
  checkmate::assert_logical(flip, len = 1)

  # wrap is a numeric scalar or NULL
  if (!is.null(wrap)) {
    checkmate::assert_numeric(wrap, len = 1, null.ok = TRUE)
  }

  # alpha is a numeric scalar between 0 and 1
  checkmate::assert_numeric(alpha, lower = 0, upper = 1, len = 1)

  # add_text is a logical scalar
  checkmate::assert_logical(add_text, len = 1)

  # add_text_size is a numeric scalar
  checkmate::assert_numeric(add_text_size, len = 1)

  # add_text_font_face is a character scalar in bold plain or italic
  checkmate::assert_choice(add_text_font_face, c("bold", "plain", "italic"))

  # add_text_threshold_display is a numeric scalar
  checkmate::assert_numeric(add_text_threshold_display, len = 1)

  # add_text_suffix is a character scalar
  checkmate::assert_character(add_text_suffix, len = 1)

  # add_text_expand_limit is a numeric scalar
  checkmate::assert_numeric(add_text_expand_limit, len = 1)

  # add_text_round is a numeric scalar
  checkmate::assert_numeric(add_text_round, len = 1)

  # x and y are numeric or character
  if (class(df[[y]]) %notin% c("integer", "numeric")) {
    rlang::abort(paste0(y, " must be numeric."))
  }
  if (!any(class(df[[x]]) %in% c("character", "factor"))) {
    rlang::abort(paste0(x, " must be character or factor"))
  }

  # width is a numeric scalar between 0 and 1
  checkmate::assert_numeric(width, lower = 0, upper = 1, len = 1)

  # Check if position is stack or dodge
  if (position %notin% c("stack", "dodge")) {
    rlang::abort("Position should be either 'stack' or 'dodge'.")
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

  # width
  width <- width
  dodge_width <- width

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

  # should the graph use position_fill?
  if (group != "") {
    if (position == "stack") {
      g <- g +
        ggplot2::geom_col(
          alpha = alpha,
          width = width,
          position = ggplot2::position_stack()
        )
    } else if (position == "dodge") {
      g <- g +
        ggplot2::geom_col(
          alpha = alpha,
          width = width,
          position = ggplot2::position_dodge2(
            width = dodge_width,
            preserve = "single"
          )
        )
    } else {
      g <- g +
        ggplot2::geom_col(
          alpha = alpha,
          width = width
        )
    }
  } else {
    if (position == "stack") {
      g <- g +
        ggplot2::geom_col(
          alpha = alpha,
          width = width,
          position = ggplot2::position_stack(),
          fill = add_color,
          color = add_color
        )
    } else if (position == "dodge") {
      g <- g +
        ggplot2::geom_col(
          alpha = alpha,
          width = width,
          position = ggplot2::position_dodge2(
            width = dodge_width,
            preserve = "single"
          ),
          fill = add_color,
          color = add_color
        )
    } else {
      g <- g +
        ggplot2::geom_col(
          alpha = alpha,
          width = width,
          fill = add_color,
          color = add_color
        )
    }
  }

  # wrap labels on the x scale?
  if (!is.null(wrap)) {
    g <- g + ggplot2::scale_x_discrete(labels = scales::label_wrap(wrap))
  }

  # because a text legend should always be horizontal, especially for an horizontal bar graph
  if (flip) {
    g <- g + ggplot2::coord_flip()
  }
  # add text to bars
  if (flip) {
    hjust_flip <- -0.5
  } else {
    hjust_flip <- 0.5
  }
  if (flip) {
    vjust_flip <- 0.5
  } else {
    vjust_flip <- -0.5
  }

  # function for interaction
  interaction_f <- function(group, facet, data) {
    if (group == "" && facet == "") {
      return(NULL)
    } else if (group != "" && facet != "") {
      return(interaction(data[[group]], data[[facet]]))
    } else if (group != "") {
      return(data[[group]])
    } else if (facet != "") {
      return(data[[facet]])
    } else {
      return(NULL)
    }
  }

  # add text labels
  if (add_text & position == "dodge") {
    df <- dplyr::mutate(
      df,
      "y_threshold" := ifelse(
        !!rlang::sym(y) >= add_text_threshold_display,
        !!rlang::sym(y),
        NA
      )
    )

    # expand limits
    g <- g +
      ggplot2::geom_blank(
        data = df,
        ggplot2::aes(
          x = !!rlang::sym(x),
          y = !!rlang::sym(y) * add_text_expand_limit,
          group = interaction_f(group, facet, df)
        )
      )
    g <- g +
      ggplot2::geom_text(
        data = df,
        ggplot2::aes(
          label = ifelse(
            is.na(!!rlang::sym("y_threshold")),
            NA,
            paste0(
              round(!!rlang::sym("y_threshold"), add_text_round),
              add_text_suffix
            )
          ),
          group = interaction_f(group, facet, df)
        ),
        hjust = hjust_flip,
        vjust = vjust_flip,
        color = add_text_color,
        fontface = add_text_font_face,
        size = add_text_size,
        position = ggplot2::position_dodge2(width = dodge_width)
      )
  } else if (add_text & position == "stack") {
    df <- dplyr::mutate(
      df,
      "y_threshold" := ifelse(
        !!rlang::sym(y) >= add_text_threshold_display,
        !!rlang::sym(y),
        NA
      )
    )

    g <- g +
      ggplot2::geom_text(
        data = df,
        ggplot2::aes(
          label = ifelse(
            is.na(!!rlang::sym("y_threshold")),
            NA,
            paste0(
              round(!!rlang::sym("y_threshold"), add_text_round),
              add_text_suffix
            )
          ),
          group = interaction_f(group, facet, df)
        ),
        hjust = hjust_flip,
        vjust = vjust_flip,
        color = add_text_color,
        fontface = add_text_font_face,
        size = add_text_size,
        position = ggplot2::position_dodge2(width = dodge_width)
      )
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

  # # remove guides for legend if !add_color_guide
  if (!add_color_guide) {
    g <- g + ggplot2::guides(fill = "none", color = "none")
  }

  # # add theme fun
  if (!is.null(theme_fun)) {
    g <- g + theme_fun
  }

  # # # add scale fun
  if (!is.null(scale_fill_fun)) {
    g <- g + scale_fill_fun
  }

  if (!is.null(scale_color_fun)) {
    g <- g + scale_color_fun
  }

  return(g)
}
