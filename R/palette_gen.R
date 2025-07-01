#' Generate color palettes
#'
#' [palette_gen()] generates a color palette and let you choose whether continuous or discrete. [palette_gen_categorical()] and [palette_gen_sequential()] generates respectively discrete and continuous palettes.
#'
#' @param palette Palette name from [palette()].
#' @param type "categorical" or "sequential" or "divergent".
#' @param direction 1 or -1; should the order of colors be reversed?
#' @param ... Additional arguments to pass to [colorRampPalette()] when type is "continuous".
#'
#' @export
palette_gen <- function(palette, type, direction = 1, ...) {
  if (type %notin% c("categorical", "sequential", "divergent")) {
    rlang::abort("'type' must be categorical or continuous or divergent.")
  }

  if (type == "categorical") {
    return(palette_gen_categorical(palette = palette, direction = direction))
  }

  if (type %in% c("sequential", "divergent")) {
    return(palette_gen_sequential(
      palette = palette,
      direction = direction,
      ...
    ))
  }
}


#' @rdname palette_gen
#'
#' @export
palette_gen_categorical <- function(palette = "cat_5_main", direction = 1) {
  if (abs(direction) != 1) {
    rlang::abort("Direction must be either 1 or -1.")
  }

  pal <- palette(palette)

  f <- function(n) {
    if (is.null(n)) {
      n <- length(pal)
    }

    if (n > length(pal)) {
      rlang::warn("Not enough colors in this palette!")
    }

    pal <- if (direction == 1) pal else rev(pal)

    pal <- pal[1:n]

    return(pal)
  }

  return(f)
}

#' @rdname palette_gen
#'
#' @export
palette_gen_sequential <- function(palette = "seq_5_main", direction = 1, ...) {
  if (abs(direction) != 1) {
    rlang::abort("Direction must be either 1 or -1.")
  }

  pal <- palette(palette)

  pal <- if (direction == 1) pal else rev(pal)

  grDevices::colorRampPalette(pal, ...)
}
