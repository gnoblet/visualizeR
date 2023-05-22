#' @title Simple waffle chart
#'
#' @param df A data frame.
#' @param x A character column or coercible as a character column. Will give the waffle's fill color.
#' @param y A numeric column.
#' @param alpha Fill transparency.
#' @param title Plot title. Default to NULL.
#' @param subtitle Plot subtitle. Default to NULL.
#' @param caption Plot caption. Default to NULL.
#' @param arrange TRUE or FALSE. Arrange by highest percentage first.
#' @param theme Whatever theme. Default to theme_reach().
#'
#' @return A waffle chart to be used parsimoniously
#'
#' @export
waffle <- function(df,
                  x,
                  y,
                  alpha = 1,
                  x_title = NULL,
                  title = NULL,
                  subtitle = NULL,
                  caption = NULL,
                  arrange = TRUE,
                  n_rows = 10,
                  size = 0.33,
                  colour = "white",
                  flip = FALSE,
                  theme = theme_reach(

                    axis_x = FALSE,
                    axis_y = FALSE)){

  # Arrange by biggest prop first ?
  if (arrange) df <- dplyr::arrange(
    df,
    dplyr::desc({{ y }})
  )

  # Get levels for scaling
  lev <- dplyr::pull(df, {{ x }})
  df <- dplyr::mutate(df, "{{x}}" := factor({{ x }}, levels = lev))

  # Mapping
  g <- ggplot2::ggplot(
    df,
    mapping = ggplot2::aes(
      fill = {{ x }},
      color = {{ x }},
      values = {{ y }}
    )
  )

  # Add waffle geom
  g <- g +
    waffle::geom_waffle(
      n_rows = n_rows,
      size = size,
      color = "white",
      flip = flip,
      alpha = alpha) +
    ggplot2::coord_equal()

  # Add title, subtitle, caption, x_title, y_title
  g <- g + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    fill = x_title,
    color = x_title
  )

  # # No axis
  # g <- g + ggplot2::theme(
  #   axis.line.x = ggplot2::element_blank(),
  #   axis.ticks.x = ggplot2::element_blank(),
  #   axis.text.x = ggplot2::element_blank(),
  #   axis.title.x = ggplot2::element_blank(),
  #   axis.line.y = ggplot2::element_blank(),
  #   axis.ticks.y = ggplot2::element_blank(),
  #   axis.text.y = ggplot2::element_blank(),
  #   axis.title.y = ggplot2::element_blank()
  # )

  # Basic theme
  g <- g +
    hrbrthemes::theme_ipsum() +
    waffle::theme_enhance_waffle()

  # Add theme
  g <- g + theme

  return(g)

}

#
# # Make a waffle plot with ggplot2 that shows 100 squares and the already summarized proportions of the following data frame as input: df <- data.frame(category = c("Category 1", "Category 2", "Category 3"),proportion = c(0.1, 0.8, 0.1))
#
#
# # Make a waffle plot using ggplot2 that shows 100 squares and the already summarized proportions of the following data frame as input: df <- data.frame(category = c("Category 1", "Category 2", "Category 3"),proportion = c(0.1, 0.8, 0.1))
#
# # Load the ggplot2 package
# library(ggplot2)
#
# # Create a vector with the number of squares for each category
# num_squares <- round(df$proportion * 100)
#
# # Create a vector with the category labels (repeated by the number of squares)
# labels <- rep(df$category, times = num_squares)
#
# # Create a data frame with the labels and a numeric variable for the x and y positions
# waffle_df <- data.frame(
#   x = rep(1:10, times = 10),
#   y = rep(1:10, each = 10),
#   label = labels
# )
#
# # Create the plot using ggplot2
# ggplot(waffle_df, aes(x = x, y = y, fill = label)) +
#   geom_tile(color = "white", size = 0.5) +
#   scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF")) +
#   theme_void()
# # Load the ggplot2 package
# library(ggplot2)
#
# # Create a vector of labels for the categories
# labels <- df$category
#
# # Create a vector with the number of squares for each category, based on the proportions
# numsquares <- round(df$proportion * 100)
#
# # Create a data frame with the labels and the number of squares
# df_waffle <- data.frame(
#   category = rep(labels, numsquares),
#   square = rep(1:100)
# )
#
# # Create the plot using ggplot
# ggplot(df_waffle, aes(x = factor(1), fill = category)) +
#   geom_bar(stat = "count") +
#   scale_fill_brewer(palette = "Set2") +
#   coord_polar(theta = "y") +
#   theme_void() +
#   guides(fill = guide_legend(title = "Category"))
#
#
#
#
#
#
# # Load the 'waffle' package for creating waffle charts
# library(waffle)
#
# # Define the input data
# df <- data.frame(
#   category = c("Category 1", "Category 2", "Category 3"),
#   proportion = c(10.2, 80, 9.8)
# )
#
# reach_co
#
# # Create a waffle chart with 100 squares
# waffle::waffle(setNames(df$proportion*100, df$category), rows = 10,
#        legend_pos = "bottom", title = "Waffle Chart") +
#   theme_reach(axis_x = FALSE, axis_y = FALSE, title_position_to_plot = FALSE, grid_major_x = FALSE, grid_major_y = FALSE, legend_position = "bottom", legend_direction = "horizontal")
#
#   ggplot2::theme(plot.title  = ggplot2::element_text(hjust = 0.5))
#
# # Add a legend that shows the labels for each category
# legend(x = "bottom", legend = df$category, fill = c("#F8766D", "#00BA38", "#619CFF"))
#
# # waffle plot that shows 100 squares and the already summarized proportions of the following dataframe as input:
# df <- data.frame(category = c("Category 1", "Category 2", "Category 3", "Category 4"), proportion = c(0.1, 0.5, 0.1, 0.3,))
