#' @title Simple horizontal bar chart
#'
#' @param .tbl Some data
#' @param x Some numeric column on the x scale
#' @param y Some column on the y scale
#' @param group Some grouping categorical column, e.g. administrative areas
#' @param initiative Either "reach" or "agora" or "impact" for the color palette
#' @param pal The color palette from the initiative
#' @param width Width
#' @param x_title The x scale title. Default to empty string
#' @param y_title The y scale title. Default to empty string
#' @param group_title The group legend title. Defaut to NULL
#' @param font_family The font family. Default to "Leelawadee"
#' @param font_size Body font size; default to 12
#' @param position Should the chart be stacked? Default to dodge
#' @param reverse Boolean indicating whether the color palette should be reversed
#' @param title Plot title. Default to empty string
#' @param subtitle Plot subtitle. Default to empty string
#' @param title_font_size Default to 16
#' @param subtitle_font_size Default to 14
#' @param ... Other arguments to be passed to "ggblanket::gg_col"
#'
#' @return A horizontal bar chart
#'
#' @export
hbar <- function(.tbl, x, y, group = NULL, initiative = "reach", pal = "primary", width = 0.5, x_title = "", y_title = "", group_title = NULL, font_family = "Leelawadee",  font_size = 12, position = "dodge", reverse = FALSE, title = "", subtitle = "", title_font_size = 16, subtitle_font_size = 14, ...){


  if (!(initiative %in% c("reach", "agora", "impact"))) rlang::abort(c("Wrong `initiative` arg", "*" = paste0("Arg `initiative` cannot be: ", initiative), "i" = "It must be one of 'reach' or 'agora' or 'impact'"))

  if (initiative == "reach") {
    palette <- pal_reach(pal, reverse = reverse)
    main_col <- cols_reach("main_grey")

    if(is.null(palette)) rlang::warn(
      c(paste0("There is no palette '", pal, "' for initiative 'reach'. Fallback to ggblanket's default color palette."),
        "i" = paste0("Use `pal_reach(show_palettes = T)` to see the list of availabale palettes.")
        )
      )
  }

  if (initiative == "agora") {
    palette <- pal_agora(pal, reverse = reverse)
    main_col <- cols_agora("main_bordeaux")


    if(is.null(palette)) rlang::warn(
      c(paste0("There is no palette '", pal, "' for initiative 'agora'. Fallback to ggblanket's default color palette."),
               "i" = paste0("Use `pal_agora(show_palettes = T)` to see the list of availabale palettes.")
        )
      )
  }

  if (initiative == "impact") rlang::warn("IMPACT colors are under development. Fallback to ggblanket's default.")

  hbar <- .tbl |>
    ggblanket::gg_col(x = {{ x }},
           y = {{ y }},
           col = {{ group }},
           theme = ggblanket::gg_theme(font = font_family, pal_title = main_col),
           x_title = x_title,
           y_title = y_title,
           col_title = group_title,
           alpha_fill = 1,
           pal = palette,
           width = width,
           position = position,
           stat = "identity",
           title = "",
           subtitle = "",
           ...
           )

  return(hbar)
}



#' @title Simple horizontal bar chart which scales x_labels to percentages
#'
#' @param .tbl Some data
#' @param x Some numeric column on the x scale
#' @param y Some column on the y scale
#' @param group Some grouping categorical column, e.g. administrative areas
#' @param initiative Either "reach" or "agora" or "impact" for the color palette
#' @param pal The color palette from the initiative
#' @param width Width
#' @param x_title The x scale title. Default to empty string
#' @param y_title The y scale title. Default to empty string
#' @param group_title The group legend title. Defaut to NULL
#' @param font_family The font family. Default to "Leelawadee"
#' @param font_size Body font size; default to 12
#' @param position Should the chart be stacked? Default to dodge
#' @param reverse Boolean indicating whether the color palette should be reversed
#' @param title Plot title. Default to empty string
#' @param subtitle Plot subtitle. Default to empty string
#' @param title_font_size Default to 16
#' @param subtitle_font_size Default to 14
#' @param ... Other arguments to be passed to "ggblanket::gg_col"
#'
#' @return A horizontal bar chart
#'
#' @export
hbar_percent <- function(.tbl, x, y, group = NULL, initiative = "reach", pal = "primary", width = 0.5, x_title = "", y_title = "", group_title = NULL, font_family = "Leelawadee",  font_size = 12, position = "dodge", reverse = FALSE, title = "", subtitle = "",  title_font_size = 16, subtitle_font_size = 14, ...){


  if (!(initiative %in% c("reach", "agora", "impact"))) rlang::abort(c("Wrong `initiative` arg", "*" = paste0("Arg `initiative` cannot be: ", initiative), "i" = "It must be one of 'reach' or 'agora' or 'impact'"))

  if (initiative == "reach") {
    palette <- pal_reach(pal, reverse = reverse)
    main_col <- cols_reach("main_grey")

    if(is.null(palette)) rlang::warn(
      c(paste0("There is no palette '", pal, "' for initiative 'reach'. Fallback to ggblanket's default color palette."),
        "i" = paste0("Use `pal_reach(show_palettes = T)` to see the list of availabale palettes.")
      )
    )
  }

  if (initiative == "agora") {
    palette <- pal_agora(pal, reverse = reverse)
    main_col <- cols_agora("main_bordeaux")


    if(is.null(palette)) rlang::warn(
      c(paste0("There is no palette '", pal, "' for initiative 'agora'. Fallback to ggblanket's default color palette."),
        "i" = paste0("Use `pal_agora(show_palettes = T)` to see the list of availabale palettes.")
      )
    )
  }

  if (initiative == "impact") rlang::warn("IMPACT colors are under development. Fallback to ggblanket's default.")

  hbar <- .tbl |>
    ggblanket::gg_col(x = {{ x }},
                      y = {{ y }},
                      col = {{ group }},
                      theme = ggblanket::gg_theme(font = font_family, pal_title = main_col, size_body = font_size, size_title = title_font_size, size_subtitle = subtitle_font_size),
                      x_title = x_title,
                      y_title = y_title,
                      col_title = group_title,
                      alpha_fill = 1,
                      pal = palette,
                      width = width,
                      x_labels = scales::percent,
                      position = position,
                      stat = "identity",
                      title = "",
                      subtitle = "",
                      ...
    )

  return(hbar)
}


