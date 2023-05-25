#' @title Simple waffle chart
#'
#' @param df A data frame.
#' @param x A character column or coercible as a character column. Will give the waffle's fill color.
#' @param y A numeric column (if plotting proportion, make sure to have percentages between 0 and 100 and not 0 and 1).
#' @param n_rows Number of rows. Default to 10.
#' @param size Width of the separator between blocks (defaults to 2).
#' @param x_title The x scale title. Default to NULL.
#' @param x_lab The x scale caption. Default to NULL.
#' @param title Plot title. Default to NULL.
#' @param subtitle Plot subtitle. Default to NULL.
#' @param caption Plot caption. Default to NULL.
#' @param arrange TRUE or FALSE. Arrange by highest percentage first.
#' @param theme Whatever theme. Default to theme_reach().
#'
#' @return A waffle chart
#'
#' @export
#'
#' @example
#' df <- data.frame(category = c("Category 1", "Category 2", "Category 3"),proportion = c(15, 55, 30))
#' waffle(df, category, proportion, x_title = "A caption", title = "A title", subtitle = "A subtitle")
waffle <- function(df,
                  x,
                  y,
                  n_rows = 10,
                  size = 2,
                  x_title = NULL,
                  x_lab = NULL,
                  title = NULL,
                  subtitle = NULL,
                  caption = NULL,
                  arrange = TRUE,
                  theme = theme_reach(
                    axis_x = FALSE,
                    axis_y = FALSE,
                    legend_position = "bottom",
                    legend_direction = "horizontal",
                    hjust = 0.5)){

  # A basic and not robust check
  # - add check between 0 and 1

  # Arrange by biggest prop first ?
  if (arrange) df <- dplyr::arrange(
    df,
    dplyr::desc({{ y }})
  )

  # Mutate to 100
  # df <- dplyr::mutate(df, "{{y}}" := {{ y }} * 100)

  # Prepare named vector
  values <- stats::setNames(dplyr::pull(df, {{ y }}), dplyr::pull(df, {{ x }}))

  # Make plot
  g <- waffle::waffle(values, xlab = x_lab, rows = n_rows, size = size)

  # Add title, subtitle, caption, x_title, y_title
  g <- g + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    fill = x_title,
    color = x_title
  )

  # Basic theme
  # g <- g +
    # hrbrthemes::theme_ipsum() #+
    # waffle::theme_enhance_waffle()

  # Add theme
  g <- g + theme

  return(g)

}
