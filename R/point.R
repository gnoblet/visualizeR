#' @title Simple point chart
#'
#' @param df A data frame.
#' @param x A numeric column.
#' @param y A character column or coercible as a character column.
#' @param group Some grouping categorical column, e.g. administrative areas or population groups.
#' @param palette Palette name from 'pal_reach()'.
#' @param reverse Boolean indicating whether the palette should be reversed.
#' @param family The font family for all plot's texts. Default to "Leelawadee".
#' @param alpha Transparency.
#' @param size Dot size. Default to 1.5.
#' @param x_title The x scale title. Default to NULL.
#' @param y_title The y scale title. Default to NULL.
#' @param group_title The group legend title. Default to NULL.
#' @param title Plot title. Default to NULL.
#' @param subtitle Plot subtitle. Default to NULL.
#' @param caption Caption title string. Default to NULL.
#' @param text_size The size of all text other than the title, subtitle and caption. Defaults to 10.
#' @param title_size The size of the title text. Defaults to 14.
#' @param legend_position Position of the legend; Default to "right". Can take "right", "left", "top", "bottom" or "none".
#' @param ... Other arguments to be passed to "ggblanket::gg_col"
#'
#' @description `ggblanket` as internals for deciding whether the bar chart is horizontally readable.
#'
#' @return A bar chart
#'
#' @export
point_reach <- function(df, x, y, group = NULL, palette = "main", reverse = FALSE, family = "Leelawadee", alpha = 1, size = 1.5, x_title = NULL, y_title = NULL, group_title = NULL, title = NULL, subtitle = NULL, caption = NULL, text_size = 10, title_size = 14, legend_position = "right", ...){

  pal <- pal_reach(palette)

  if(is.null(pal)) rlang::warn(
    c(paste0("There is no palette '", palette, "' for initiative 'reach'. Fallback to ggblanket's default color palette."),
      "i" = paste0("Use `pal_reach(show_palettes = T)` to see the list of availabale palettes.")
    )
  )

  pl <- df |>
    ggblanket::gg_point(x = {{ x }},
                      y = {{ y }},
                      col = {{ group }},
                      x_title = x_title,
                      y_title = y_title,
                      col_title = group_title,
                      alpha = alpha,
                      size = size,
                      title = title,
                      subtitle = subtitle,
                      caption = caption,
                      col_legend_place = legend_position,
                      theme = theme_reach(
                        palette = palette,
                        reverse = reverse,
                        family = family,
                        text_size = text_size,
                        title_size = title_size,
                        plot_background_pal = "#FFFFFF",
                        panel_background_pal = "#FFFFFF"
                      ),
                      ...
    )

  return(pl)
}
