

#' Wrapper around `tmap::tm_polygons()` with sane defaults for plotting indicator values
#'
#' @param poly Multipolygon shape defined by sf package.
#' @param col Numeric attribute to map.
#' @param buffer A buffer, either one value or a vector of 4 values (left, bottom, right, top).
#' @param n The desire number of classes.
#' @param style Method to process the color scale for continuous numerical variables. See `classInt::classIntervals()` for details.
#' @param palette Vector of fill colors as hexadecimal values. For REACH color palettes, it is possible to use `pal_reach()`. For now,'palette' must be changed manually, accordingly to the number of drawn classes.
#' @param as_count Boolean. When col is a numeric variable, should it be processed as a count variable? For instance, 0, 1-10, 11-20.
#' @param color_na Fill color for missing data.
#' @param text_na Legend text for missing data.
#' @param legend_title Legend title.
#' @param legend_text_separator Text separator for classes. E.g. " to " will give 0, 1 to 10, 11 to 20.
#' @param border_alpha Transparency of the border.
#' @param border_col Color of the border.
#' @param lwd Linewidth of the border.
#' @param ... Other arguments to pass to `tmap::tm_polygons()`.
#'
#' @return A tmap layer.
#' @export
#'
add_indicator_layer <- function(
    poly,
    col,
    buffer = NULL,
    n = 5,
    style = "pretty",
    palette = pal_reach("red_5"),
    as_count = TRUE,
    color_na = cols_reach("white"),
    text_na = "Missing data",
    legend_title = "Proportion (%)",
    legend_text_separator = " - ",
    border_alpha = 1,
    border_col = cols_reach("lt_grey_1"),
    lwd = 1,
    ...){

  #------ Checks and make valid

  rlang::check_installed("tmap", reason = "Package \"tmap\" needed for `add_indicator_layer()` to work. Please install it.")

  poly <- sf::st_make_valid(poly)

  #------ Other checks

  col_name <- rlang::as_name(rlang::enquo(col))
  if_not_in_stop(poly, col_name, "poly", "col")

  if (!is.numeric(poly[[col_name]])) rlang::abort(glue::glue("{col_name} is not numeric."))


  #------ Prepare data

  if(!is.null(buffer)){ buffer <- buffer_bbox(poly, buffer) } else { buffer <- NULL }


  #------ Polygon layer

  layer <- tmap::tm_shape(
    poly,
    bbox = buffer
  ) +
    tmap::tm_polygons(
      col = col_name,
      n = n,
      style = style,
      palette = palette,
      as.count = as_count,
      colorNA = color_na,
      textNA = text_na,
      title = legend_title,
      legend.format = list(text.separator = legend_text_separator),
      borderl.col = border_col,
      border.alpha = border_alpha,
      lwd = lwd,
      ...
    )

  return(layer)

}




#' Add admin boundaries (lines) and the legend
#'
#' @param lines List of multiline shape defined by sf package.
#' @param colors Vector of hexadecimal codes. Same order as lines.
#' @param labels Vector of labels in the legend. Same order as lines.
#' @param lwds Vector of line widths. Same order as lines.
#' @param title Legend title.
#' @param buffer A buffer, either one value or a vector of 4 values (left, bottom, right, top).
#' @param ... Other arguments to pass to each shape in `tmap::tm_lines()`.
#'
#' @return A tmap layer.
#' @export
#'
add_admin_boundaries <- function(lines, colors, labels, lwds, title = "", buffer = NULL, ...){


  #------ Package check

  rlang::check_installed("tmap", reason = "Package \"tmap\" needed for `add_admin_boundaries()` to work. Please install it.")


  #------ Check that the length of vectors is identical between arguments

  if(!inherits(lines, "list")) rlang::abort("Please provide a list for lines.")

  ll <- list(lines, colors, labels, lwds)
  if (!all(sapply(ll,length) == length(ll[[1]]))) rlang::abort("lines, colors, labels, lwds do not all have the same length.")


  #------ Make valid

  lines <- lapply(lines, \(x) sf::st_make_valid(x))


  #------ Prepare legend
  legend_lines <- tmap::tm_add_legend("line",
                                      title = title,
                                      col = colors,
                                      lwd = lwds,
                                      labels = labels)


  #------ Let's go with all line shapes

  if(!is.null(buffer)){ buffer <- buffer_bbox(lines[[1]], buffer) } else { buffer <- NULL }


  layers <- tmap::tm_shape(lines[[1]], bbox = buffer) +
    tmap::tm_lines(lwd = lwds[[1]], col = colors[[1]], ...)

  if (length(lines) == 1) {

    layers <- layers + legend_lines

    return(layers)

  } else {

    for(i in 2:length(lines)){

      layers <- layers + tmap::tm_shape(shp = lines[[i]]) + tmap::tm_lines(lwd = lwds[[i]], col = colors[[i]], ...)
    }

    layers <- layers + legend_lines

    return(layers)

  }
}




#' Basic defaults based on `tmap::tm_layout()`
#'
#' @param title Map title.
#' @param legend_position Legend position. Not above the map is a good start.
#' @param frame Boolean. Legend frame?
#' @param legend_frame Legend frame color.
#' @param legend_text_size Legend text size in 'pt'.
#' @param legend_title_size Legend title size in 'pt'.
#' @param title_size Title text size in 'pt'.
#' @param title_fontface Title fontface. Bold if you wanna exemplify a lot what it is about.
#' @param title_color Title font color.
#' @param fontfamily Overall fontfamily. Leelawadee is your precious.
#' @param ... Other arguments to pass to `tmap::tm_layout()`.
#'
#' @return A tmap layer.
#' @export
#'
add_layout <- function(
    title = NULL,
    legend_position = c(0.02, 0.5),
    frame = FALSE,
    legend_frame = cols_reach("main_grey"),
    legend_text_size = 0.6,
    legend_title_size = 0.8,
    title_size = 0.9,
    title_fontface = "bold",
    title_color = cols_reach("main_grey"),
    # check.and.fix = TRUE,
    fontfamily = "Leelawadee",
    ...){

  layout <- tmap::tm_layout(
    title = title,
    legend.position = legend_position,
    legend.frame = legend_frame,
    frame = FALSE,
    legend.text.size = legend_text_size,
    legend.title.size = legend_title_size,
    title.size = title_size,
    title.fontface = title_fontface,
    title.color = title_color,
    fontfamily = fontfamily,
    ...)

  return(layout)

  }




#' Wrapper around `tmap::tm_text()` with sane defaults for plotting admin labels.
#'
#' @param point Multipoint shape defined by sf package.
#' @param text Text labels column.
#' @param size Relative size of the text labels.
#' @param fontface Fontface.
#' @param fontfamily Fontfamily. Leelawadee is your precious.
#' @param shadow Boolean. Add a shadow around text labels. Issue opened on Github to request.
#' @param auto_placement Logical that determines whether the labels are placed automatically.
#' @param remove_overlap Logical that determines whether the overlapping labels are removed.
#' @param ... Other arguments to pass to `tmap::tm_text()`.
#'
#' @return A tmap layer.
#' @export
#'
add_admin_labels <- function(point,
                             text,
                             size = 0.5,
                             fontface = "bold",
                             fontfamily = "Leelawadee",
                             shadow = TRUE,
                             auto_placement = FALSE,
                             remove_overlap = FALSE,
                             ...){


  #------ Restrictive sf checks (might not be necessary depending on the desired behaviour)

  rlang::check_installed("tmap", reason = "Package \"tmap\" needed for `add_indicator_layer()` to work. Please install it.")

  point <- sf::st_make_valid(point)

  #------ Other checks

  text_name <- rlang::as_name(rlang::enquo(text))
  if_not_in_stop(point, text_name, "point", "text")

  #------ Point text layer

  layer <- tmap::tm_shape(point) +
    tmap::tm_text(text = text_name,
                  size = size,
                  fontface = fontface,
                  fontfamily = fontfamily,
                  shadow = shadow,
                  auto.placement = auto_placement,
                  remove.overlap = remove_overlap,
                  ...)

  return(layer)

}




#' Add a compass
#'
#' @param text_size Relative font size.
#' @param position Position of the compass. Vector of two values, specifying the x and y coordinates.
#' @param color_dark Color of the dark parts of the compass.
#' @param text_color color of the text.
#' @param type Compass type, one of: "arrow", "4star", "8star", "radar", "rose".
#' @param ... Other arguments to pass to `tmap::tm_compass()`.
#'
#' @return A tmap layer.
#' @export
#'
add_compass <- function(text_size = 0.6,
                        position = c("right", 0.8),
                        color_dark = cols_reach("black"),
                        text_color = cols_reach("black"),
                        type = "4star",
                        ...){

  compass <- tmap::tm_compass(
    text.size = text_size,
    position = position,
    color.dark = color_dark,
    type = type,
    text.color = text_color
  )

  return(compass)

}




#' Add a scale bar
#'
#' @param text_size Relative font size.
#' @param position Position of the compass. Vector of two values, specifying the x and y coordinates.
#' @param color_dark Color of the dark parts of the compass.
#' @param breaks Breaks of the scale bar. If not specified, breaks will be automatically be chosen given the prefered width of the scale bar. Example: c(0, 50, 100).
#' @param ... Other arguments to pass to `tmap::tm_compass()`.
#'
#' @return A tmap layer.
#' @export
#'
add_scale_bar <- function(text_size = 0.6,
                          position = c("left", 0.01),
                          color_dark = cols_reach("black"),
                          breaks = c(0, 50, 100),
                          ...){

  scale_bar <- tmap::tm_scale_bar(
    text.size = text_size,
    position = position,
    color.dark = color_dark,
    breaks = breaks,
    ...
  )

  return(scale_bar)

}




#' Do you want to credit someone or some institution?
#'
#' @param text Text.
#' @param size Relative text size.
#' @param bg_color Background color.
#' @param position Position. Vector of two coordinates. Usually somewhere down.
#' @param ... Other arguments to pass to `tmap::tm_credits()`.
#'
#' @return A tmap layer.
#' @export
#'
add_credits <- function(text, size = 0.4, bg_color = NA, position = c(0.75, 0.02), ...){

  tmap::tm_credits(text,
                   size = size,
                   bg.color = bg_color,
                   position = position,
                   ...)
}

