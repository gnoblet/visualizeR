#' Color scale constructor for REACH or AGORA colors
#'
#' @param initiative Either "reach" or "agora" or "default".
#' @param palette Palette name from `pal_reach()` or `pal_agora()`.
#' @param discrete Boolean indicating whether color aesthetic is discrete or not.
#' @param reverse Boolean indicating whether the palette should be reversed.
#' @param reverse_guide Boolean indicating whether the guide should be reversed.
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradient(), used respectively when discrete is TRUE or FALSE.
#'
#' @return A color scale for ggplot
#'
#' @export
scale_color <- function(initiative = "reach", palette = "main", discrete = TRUE, reverse = FALSE, reverse_guide = TRUE, ...) {

  if (initiative == "reach") {

    pal <- pal_reach(palette)

    if (is.null(pal)) {

      pal <- pal_fallback(
        reverse = reverse,
        discrete = discrete,
        color_ramp_palette = TRUE)

      rlang::warn(
        c(
          paste0("There is no palette '", palette, "' for the selected initiative. Fallback to pal_fallback()."),
          "i" = paste0("Use `pal_reach(show_palettes = TRUE)` to see the list of available palettes.")
        )
      )

      if (discrete) palette <- "viridis" else palette <- "magma"

    } else {

      pal <- pal_reach(
        palette = palette,
        reverse = reverse,
        color_ramp_palette = TRUE,
        show_palettes = FALSE
      )

    }

  } else if (initiative == "agora") {

    pal <- pal_agora(palette)

    if (is.null(pal)) {

      pal <- pal_fallback(
        reverse = reverse,
        discrete = discrete,
        color_ramp_palette = TRUE)

      rlang::warn(
        c(
          paste0("There is no palette '", palette, "' for the selected initiative. Fallback to pal_fallback()."),
          "i" = paste0("Use `pal_reach(show_palettes = TRUE)` to see the list of available palettes.")
        )
      )

      if (discrete) palette <- "viridis" else palette <- "magma"

    } else {

      pal <- pal_agora(
        palette = palette,
        reverse = reverse,
        color_ramp_palette = TRUE,
        show_palettes = FALSE
      )
    }

  } else if (initiative == "default") {

    pal <- pal_fallback(
      reverse = reverse,
      discrete = discrete,
      color_ramp_palette = TRUE)

    if (discrete) palette <- "viridis" else palette <- "magma"

  } else {
    rlang::abort(
      c(
        paste0("There is no initiative '", initiative, "."),
        "i" = paste0("initiative should be either 'reach', 'agora' or 'default'")
      )
    )
  }

  if (discrete) {
    ggplot2::discrete_scale(
      "colour",
      paste0(initiative, "_", palette),
      palette = pal,
      guide = ggplot2::guide_legend(
        title.position = "top",
        draw.ulim = TRUE,
        draw.llim = TRUE,
        ticks.colour = "#F1F3F5",
        reverse = reverse_guide
      ),
      ...
    )
  } else {
    ggplot2::scale_color_gradientn(
      colours = pal(256),
      guide = ggplot2::guide_colorbar(
        title.position = "top",
        draw.ulim = TRUE,
        draw.llim = TRUE,
        ticks.colour = "#F1F3F5",
        reverse = reverse_guide
      ),
      ...
    )
  }
}



#' Fill scale constructor for REACH or AGORA colors
#'
#' @param initiative Either "reach" or "agora" or "default".
#' @param palette Palette name from `pal_reach()` or `pal_agora()`.
#' @param discrete Boolean indicating whether color aesthetic is discrete or not.
#' @param reverse Boolean indicating whether the palette should be reversed.
#' @param reverse_guide Boolean indicating whether the guide should be reversed.
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradient(), used respectively when discrete is TRUE or FALSE.
#'
#' @return A fill scale for ggplot.
#'
#' @export
scale_fill <- function(initiative = "reach", palette = "main", discrete = TRUE, reverse = FALSE, reverse_guide = TRUE, ...) {

  if (initiative == "reach") {

    pal <- pal_reach(palette)

    if (is.null(pal)) {

      pal <- pal_fallback(
        reverse = reverse,
        discrete = discrete,
        color_ramp_palette = TRUE)

      rlang::warn(
        c(
          paste0("There is no palette '", palette, "' for the selected initiative. Fallback to pal_fallback()."),
          "i" = paste0("Use `pal_reach(show_palettes = TRUE)` to see the list of available palettes.")
        )
      )

      if (discrete) palette <- "viridis" else palette <- "magma"

    } else {

      pal <- pal_reach(
        palette = palette,
        reverse = reverse,
        color_ramp_palette = TRUE,
        show_palettes = FALSE
      )

    }

  } else if (initiative == "agora") {

    pal <- pal_agora(palette)

    if (is.null(pal)) {

      pal <- pal_fallback(
        reverse = reverse,
        discrete = discrete,
        color_ramp_palette = TRUE)

      rlang::warn(
        c(
          paste0("There is no palette '", palette, "' for the selected initiative. Fallback to pal_fallback()."),
          "i" = paste0("Use `pal_reach(show_palettes = TRUE)` to see the list of available palettes.")
        )
      )

      if (discrete) palette <- "viridis" else palette <- "magma"

    } else {

      pal <- pal_agora(
        palette = palette,
        reverse = reverse,
        color_ramp_palette = TRUE,
        show_palettes = FALSE
      )
    }

  } else if (initiative == "default") {

    pal <- pal_fallback(
      reverse = reverse,
      discrete = discrete,
      color_ramp_palette = TRUE)

    if (discrete) palette <- "viridis" else palette <- "magma"

  } else {
    rlang::abort(
      c(
        paste0("There is no initiative '", initiative, "."),
        "i" = paste0("initiative should be either 'reach', 'agora' or 'default'")
      )
    )
  }

  if (discrete) {
    ggplot2::discrete_scale(
      "colour",
      paste0(initiative, "_", palette),
      palette = pal,
      guide = ggplot2::guide_legend(
        title.position = "top",
        draw.ulim = TRUE,
        draw.llim = TRUE,
        ticks.colour = "#F1F3F5",
        reverse = reverse_guide
      ),
      ...
    )
  } else {
    ggplot2::scale_color_gradientn(
      colours = pal(256),
      guide = ggplot2::guide_colorbar(
        title.position = "top",
        draw.ulim = TRUE,
        draw.llim = TRUE,
        ticks.colour = "#F1F3F5",
        reverse = reverse_guide
      ),
      ...
    )
  }
}
