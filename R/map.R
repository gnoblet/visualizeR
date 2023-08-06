#' Wrapper around `ggplot2::geom_sf()` with sane defaults for plotting choropleth
#'
#' @param poly Multipolygon shape defined by sf package.
#' @param col Numeric attribute to map.
#' @param n The desire number of classes.
#' @param initiative One of "reach", "agora", or "default"
#' @param palette Vector of fill colors as hexadecimal values. For REACH color palettes, it is possible to use `pal_reach()`. For now,'palette' must be changed manually, accordingly to the number of drawn classes.
#' @param style Method to process the color scale for continuous numerical variables. See `classInt::classIntervals()` for details.
#' @param intervals Boolean. TRUE, let's make classes. FALSE, let's use a gradient.
#' @param font_family Font family.
#' @param legend_title Legend title.
#' @param legend_positin Legend position.
#' @param drop Boolean. Drop missing data?
#' @param text_na Legend text for missing data.
#' @param color_na Fill color for missing data.
#'
#' @return A ggplot base choropleth.
#'
#' @export
add_indicator_layer <- function(poly,
                                col,
                                n = 5,
                                initiative = "reach",
                                palette = "red_5",
                                style = "pretty",
                                intervals = TRUE,
                                font_family = "segoeui",
                                legend_title = "Proportion (%)",
                                legend_position = c(0, 0.95),
                                drop = FALSE,
                                text_na = "Missing data",
                                color_na = cols_reach("white")){

  #------ Checks and make valid

  poly <- sf::st_make_valid(poly)

  #------ Other checks

  col_name <- rlang::as_name(rlang::enquo(col))
  if_not_in_stop(poly, col_name, "poly", "col")

  if (!is.numeric(poly[[col_name]])) rlang::abort(glue::glue("{col_name} is not numeric."))


  #------ Prepare data

  if (intervals) {

     classes <- classInt::classIntervals(poly[[col_name]], n = n, style = style)
    col_class_name <- paste0(col_name, "_class")

    poly <- poly |>
      dplyr::mutate("{col_class_name}" := cut({{ col }}, classes$brks, include.lowest = TRUE))

    legend_labels <- c(levels(poly[[col_class_name]]), text_na)

    discrete <- TRUE

    layer <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = poly, ggplot2::aes(fill = !!rlang::sym(col_class_name)), color = "transparent") +
      scale_fill(initiative = initiative, palette = palette, discrete = discrete, reverse_guide = FALSE, name = legend_title, labels = legend_labels, drop = drop, na.value = color_na)

  } else {

    discrete <- FALSE

    layer <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = poly, ggplot2::aes(fill = !!rlang::sym(col_name)), color = "transparent") +
      scale_fill(initiative = initiative, palette = palette, discrete = discrete, reverse_guide = FALSE, name = legend_title, na.value = color_na)

  }

  #------ Make map layer

 layer <- layer +
    ggplot2::theme_void() +
    ggplot2::theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position = legend_position,
      # Set fontfamily
      text = ggplot2::element_text(family = font_family)
    )

  return(layer)

}




#' Add admin boundaries (lines) and the legend
#'
#' @param map Is there a previous map layer? Default to NULL.
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
add_admin_boundaries <- function(map = NULL, lines, colors, labels, lwds, legend_title = ""){


  if(is.null(map)) map <- ggplot2::ggplot()

  #------ Check that the length of vectors is identical between arguments

  if(!inherits(lines, "list")) rlang::abort("Please provide a list for lines.")

  ll <- list(lines, colors, labels, lwds)
  if (!all(sapply(ll,length) == length(ll[[1]]))) rlang::abort("lines, colors, labels, lwds do not all have the same length.")


  #------ Make valid

  lines <- lapply(lines, \(x) sf::st_make_valid(x))


  #------ Let's go with all line shapes

  for (i in 1:length(lines)) {
    lines[[i]] <- lines[[i]] |>
      dplyr::mutate(color = colors[[i]],
                    label = labels[[i]],
                    lwd = lwds[[i]])
  }


  layers <- map + ggplot2::geom_sf(data = lines[[1]], ggplot2::aes(color = .data[["label"]], linewidth = .data[["label"]]))

  if (length(lines) > 1){

    for(i in 2:length(lines)){

      data <- lines[[i]]
      color <- labels[[i]]
      size <- labels[[i]]

      layers <- layers + ggplot2::geom_sf(data = data, ggplot2::aes(color = .data[["label"]], linewidth = .data[["label"]]))

    }
  }
  #
  layers <- layers +
    ggplot2::scale_color_manual(name = legend_title, values = setNames(colors, labels), breaks = labels) +
    ggplot2::scale_discrete_manual("linewidth", name = legend_title, values = setNames(lwds, labels), breaks = labels)


  return(layers)

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
add_text_labels <- function(map = NULL,
                            point,
                            text,
                            size = 0.5,
                            fontface = "bold",
                            fontfamily = "Leelawadee",
                            halo_radius = 0.15,
                            halo_color = "white",
                            angle = 0,
                            force = 0,
                            force_pull = 0){

  if(is.null(map)) map <- ggplot()

  col_name <- rlang::as_name(rlang::enquo(text))

  layer <- map +
    ggspatial::geom_spatial_text_repel(
      data = point,
      ggplot2::aes(
        x = X,
        y = Y,
        label = !!rlang::sym(col_name)),
      crs = sf::st_crs(point)$input,
      force = force,
      force_pull = force_pull,
      size = 3,
      angle = angle,
      fontface = fontface,
      family = fontfamily,
      bg.r = halo_radius,
      bg.color = halo_color)


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

