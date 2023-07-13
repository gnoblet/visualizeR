#' @title Simple treemap chart
#'
#' @param df A data frame.
#' @param x A character column or coercible as a character column. Will give the treemap's fill color and text.
#' @param y A numeric column of proportions (0 to 100 or 0 to 1).
#' @param tile_border_size Size of the inter-tile space (default to 2).
#' @param tile_start The corner in which to start placing the tiles. One of 'bottomleft' (the default), 'topleft', 'topright' or 'bottomright'. See `treemapify::geom_treemap()`.
#' @param tile_corner_radius The corner radius (defaults to `grid::unit(0, "pt")`). See `treemapify::geom_treemap()`.
#' @param tile_text Boolean. If true, add a text label to each tile (the default). If false, use a side legend only.
#' @param tile_text_size A size (defaults to 20).
#' @param tile_text_color A color (defaults to "white").
#' @param tile_text_threshold_display Minimum value to add the text label to the tile (defaults to 4).
#' @param tile_text_place Where inside the box to place the text. Default is 'bottom'; other options are 'topleft', 'top', 'topright', etc. See `treemapify::geom_treemap()`.
#' @param x_title The x scale title. Default to NULL.
#' @param title Plot title. Default to NULL.
#' @param subtitle Plot subtitle. Default to NULL.
#' @param caption Plot caption. Default to NULL.
#' @param theme Whatever theme. Default to theme_reach().
#'
#' @return A waffle chart
#'
#' @export
treemap <- function(df,
                   x,
                   y,
                   tile_border_size = 2,
                   tile_start = "topleft",
                   tile_corner_radius = grid::unit(0, "pt"),
                   tile_text = TRUE,
                   tile_text_size = 20,
                   tile_text_color = "white",
                   tile_text_threshold_display = 4,
                   tile_text_place = "middle",
                   x_title = NULL,
                   title = NULL,
                   subtitle = NULL,
                   caption = NULL,
                   theme = theme_reach(reverse = TRUE, panel_border = FALSE, axis_x = FALSE, axis_y = FALSE)
                   ){

  # Make plot
  g <- ggplot2::ggplot(
    data = df,
    ggplot2::aes(area = {{ y }}, fill = {{ x }}, label = {{ x }}))

  # Add tile
  g <- g + treemapify::geom_treemap(
    size = tile_border_size,
    radius = tile_corner_radius,
    color = "white",
    start = tile_start
    )

  # Add title, subtitle, caption, x_title, y_title
  g <- g + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    fill = x_title,
  )

  # Theme
  g <- g + theme

  # If tile_text, show text on tiles and remove legend
  if (tile_text) {
    g <- g + treemapify::geom_treemap_text(
      place = tile_text_place,
      start = tile_start,
      min.size = tile_text_threshold_display,
      color = tile_text_color,
      size = tile_text_size
    ) +
      ggplot2::theme(legend.position = "none")
  }

  return(g)

}
