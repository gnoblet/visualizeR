#' @title Simple horizontal bar chart
#'
#' @description with nice percentage x labels
#'
#' @param .tbl Some data
#' @param x Some numeric column on the x scale
#' @param y Some column on the y scale
#' @param group Some grouping categorical column, e.g. administrative areas
#' @param initiative Either "reach" or "agora" or "impact" for the color palette
#' @param x_title The x scale title. Default to empty string
#' @param y_title The y scale title. Default to empty string
#' @param group_title The group legend title. Defaut to NULL
#' @param font_family The font family. Default to "Leelawadee"
#' @param stack Should the chart be stacked? Default to "FALSE" (dodge)
#' @param ... Other arguments to be passed to "simplevis::gg_hbar" or "simplevis:gg_hbar_col"
#'
#' @return A horizontal bar chart
#'
#' @export
hbar_percent <- function(.tbl, x, y, group = NULL, initiative = "reach", x_title = "", y_title = "", group_title = NULL, font_family = "Leelawadee",  stack = FALSE, ...){


  if (!(initiative %in% c("reach", "agora", "impact"))) rlang::abort(c("Wrong `initiative` arg", "*" = paste0("Arg `initiative` cannot be: ", initiative), "i" = "It must be one of 'reach' or 'agora' or 'impact'"))

  if (initiative == "reach") main_col <- cols_reach("main_grey")

  if (initiative == "agora") main_col <- cols_agora("main_bordeaux")

  if (initiative == "impact") rlang::abort("IMPACT colors are under development")

  if (is.null(group)) {
     hbar <- .tbl |>
       simplevis::gg_hbar(
         x_var = {{ x }},
         y_var = {{ y }},
         theme = simplevis::gg_theme(font = font_family, pal_title =  main_col),
         x_title = x_title,
         y_title = y_title,
         alpha_fill = 1,
         pal = main_col,
         x_labels = scales::percent,
         stack = stack,
         ...)
  } else {
    group_name <- rlang::as_name(rlang::enquo(group))
    if_not_in_stop(.tbl, group_name)

    hbar <- .tbl |>
      simplevis::gg_hbar_col(
        x_var = {{ x }},
        y_var = {{ y }},
        col_var = {{ group }},
        theme = simplevis::gg_theme(font = font_family, pal_title =  main_col),
        x_title = x_title,
        y_title = y_title,
        col_title = group_title,
        alpha_fill = 1,
        pal = main_col,
        x_labels = scales::percent,
        stack = stack,
        ...)
  }

  return(hbar)
}

#' @title Simple horizontal bar chart
#'
#' @description without any change to the x scale
#'
#' @param .tbl Some data
#' @param x Some numeric column on the x scale
#' @param y Some column on the y scale
#' @param group Some grouping categorical column, e.g. administrative areas
#' @param initiative Either "reach" or "agora" or "impact" for the color palette
#' @param x_title The x scale title. Default to empty string
#' @param y_title The y scale title. Default to empty string
#' @param group_title The group legend title. Defaut to NULL
#' @param font_family The font family. Default to "Leelawadee"
#' @param stack Should the chart be stacked? Default to "FALSE" (dodge)
#' @param ... Other arguments to be passed to "simplevis::gg_hbar" or "simplevis:gg_hbar_col"
#'
#' @return A horizontal bar chart
#'
#' @export
hbar <- function(.tbl, x, y, group = NULL, initiative = "reach", x_title = "", y_title = "", group_title = NULL, font_family = "Leelawadee",  stack = FALSE, ...){


  if (!(initiative %in% c("reach", "agora", "impact"))) rlang::abort(c("Wrong `initiative` arg", "*" = paste0("Arg `initiative` cannot be: ", initiative), "i" = "It must be one of 'reach' or 'agora' or 'impact'"))

  if (initiative == "reach") main_col <- cols_reach("main_grey")

  if (initiative == "agora") main_col <- cols_agora("main_bordeaux")

  if (initiative == "impact") rlang::abort("IMPACT colors are under development")

  if (is.null(group)) {
    hbar <- .tbl |>
      simplevis::gg_hbar(
        x_var = {{ x }},
        y_var = {{ y }},
        theme = simplevis::gg_theme(font = font_family, pal_title =  main_col),
        x_title = x_title,
        y_title = y_title,
        alpha_fill = 1,
        pal = main_col,
        ...)
  } else {
    group_name <- rlang::as_name(rlang::enquo(group))
    if_not_in_stop(.tbl, group_name)

    hbar <- .tbl |>
      simplevis::gg_hbar_col(
        x_var = {{ x }},
        y_var = {{ y }},
        col_var = {{ group }},
        theme = simplevis::gg_theme(font = font_family, pal_title =  main_col),
        x_title = x_title,
        y_title = y_title,
        col_title = group_title,
        alpha_fill = 1,
        pal = main_col,
        stack = stack,
        ...)
  }

  return(hbar)
}

