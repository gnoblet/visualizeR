library(ggplot2)
library(visualizeR)
# install.packages('ggsn')
library(ggsn)
# install.packages("ggspatial")
library(ggspatial)
# install.packages("ggrepel")
library(ggrepel)

remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)

# library(remotes)
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# library(Rttf2pt1)
# extrafont::font_import()
extrafont::loadfonts()

library(classInt)
classes <- classIntervals(indicator_admin1[["opn_dfc"]], n = 5, style = "pretty") #can use "jenks" but is very slow
bbox <- buffer_bbox(sf::st_bbox(indicator_admin1), 0.1)
prop_x = 0.95
prop_y = 0.95
bbox_arrow <- c(x = bbox[["xmin"]] + prop_x * (bbox[["xmax"]] - bbox[["xmin"]]), y =  bbox[["ymin"]] + prop_y * (bbox[["ymax"]] - bbox[["ymin"]]))

indicator_admin1 <- indicator_admin1 |>
  dplyr::mutate(opn_dfc_class = cut(opn_dfc, classes$brks, include.lowest = TRUE))
# |>
  # dplyr::mutate(opn_dfc_class = stringr::str_replace_na(opn_dfc_class, "Missing data"))

legend_labels <- c(levels(indicator_admin1$opn_dfc_class), "Missing data")

ggplot() +
  # annotation_map_tile(type = "stamenwatercolor") +
  geom_sf(data = indicator_admin1, aes(fill = opn_dfc_class), color = "transparent") +
  scale_fill(palette = "red_5", discrete = TRUE, reverse_guide = FALSE, name = "Proportion (%)", labels = legend_labels, drop = FALSE) +
  scalebar(st.size = 3, border.size = 0.5, x.min = bbox[["xmin"]], y.min = bbox[["ymin"]], x.max = bbox[["xmax"]], y.max = bbox[["ymax"]], dist = 50, dist_unit = "km", transform = TRUE) +
  north(symbol = 12,  anchor = bbox_arrow, x.min = bbox[["xmin"]], y.min = bbox[["ymin"]], x.max = bbox[["xmax"]], y.max = bbox[["ymax"]]) +
  theme_void() +
  theme(
    # legend.justification defines the edge of the legend that the legend.position coordinates refer to
    legend.justification = c(0, 1),
    # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
    legend.position = c(0, .95),
    # Set fontfamily
    text = element_text(family = "Leelawadee")
  )



centroid_admin1 <- visualizeR::centroid_admin1 |>
  # sf::st_set_crs(4326) |>
  sf::st_coordinates() |>
  dplyr::bind_cols(centroid_admin1) |>
  sf::st_as_sf(remove = FALSE, crs = sf::st_crs(4326))


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
      aes(x = X, y = Y, label = !!rlang::sym(col_name)),
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



add_admin_boundaries <- function(map = NULL, lines, colors, labels, lwds, legend_title = "", buffer = NULL, ...){


  if(is.null(map)) map <- ggplot()

  #------ Check that the length of vectors is identical between arguments

  if(!inherits(lines, "list")) rlang::abort("Please provide a list for lines.")

  ll <- list(lines, colors, labels, lwds)
  if (!all(sapply(ll,length) == length(ll[[1]]))) rlang::abort("lines, colors, labels, lwds do not all have the same length.")


  #------ Make valid

  lines <- lapply(lines, \(x) sf::st_make_valid(x))


  # #------ Prepare legend
  # legend_lines <- tmap::tm_add_legend("line",
  #                                     title = title,
  #                                     col = colors,
  #                                     lwd = lwds,
  #                                     labels = labels)

  #------ Let's go with all line shapes

  for (i in 1:length(lines)) {
    lines[[i]] <- lines[[i]] |>
      dplyr::mutate(color = colors[[i]],
                    label = labels[[i]],
                    lwd = lwds[[i]])
  }


  layers <- map + ggplot2::geom_sf(data = lines[[1]], aes(color = .data[["label"]], linewidth = .data[["label"]]))

  if (length(lines) > 1){

    for(i in 2:length(lines)){

      data <- lines[[i]]
      color <- labels[[i]]
      size <- labels[[i]]

      layers <- layers + ggplot2::geom_sf(data = data, aes(color = .data[["label"]], linewidth = .data[["label"]]))

    }
  }
  #
  layers <- layers +
    ggplot2::scale_color_manual(name = legend_title, values = setNames(colors, labels), breaks = labels) +
    ggplot2::scale_discrete_manual("linewidth", name = legend_title, values = setNames(lwds, labels), breaks = labels)


  return(layers)


}

test <- add_indicator_layer(indicator_admin1, opn_dfc, n = 5, text_na = "Et ouais")
test <- add_admin_boundaries(map = test,
                             lines = list(line_admin1, border_admin0, frontier_admin0),
                             colors = cols_reach("main_lt_grey", "main_grey", "black"),
                             lwds = c(0.5, 1, 2),
                             labels = c("Department", "Country", "Dominican Rep. frontier"),
                             legend_title = "Administrative boundaries")
test <- add_text_labels(map = test, centroid_admin1, ADM1_FR_UPPER)
test


# map
ggplot2::ggsave(test,
                "R/test.pdf",
                height = 4.5,
                width = 6,
                device = cairo_pdf
)

test <- add_text_labels(map = test, centroid_admin1, ADM1_FR_UPPER)

ggplot2::ggsave("test.svg", test, device = "svg", width = 6.5, height = 4)



test

test <- add_indicator_layer(indicator_admin1, opn_dfc, n = 5)
  add_text_labels(map = NULL, centroid_admin1, ADM1_FR_UPPER)
  add_admin_boundaries(map = NULL,
                       lines = list(line_admin1, border_admin0, frontier_admin0),
                       colors = cols_reach("main_lt_grey", "dk_grey", "black"),
                       lwds = c(0.5, 2, 3),
                       labels = c("Department", "Country", "Dominican Rep. frontier"),
                       title = "Administrative boundaries")


library(patchwork)
test+test2


#  # Final first draft -------------------------------------------------------



add_indicator_layer <- function(poly, col, n = 5, palette = "red_5", style = "pretty", discrete = TRUE, legend_title = "Proportion (%)", drop = FALSE, buffer = NULL, fontfamily = "Segoe UI", text_NA = "Missing data", legend_position = c(0, 0.95)){

  #------ Checks and make valid

  rlang::check_installed("tmap", reason = "Package \"tmap\" needed for `add_indicator_layer()` to work. Please install it.")

  poly <- sf::st_make_valid(poly)

  #------ Other checks

  col_name <- rlang::as_name(rlang::enquo(col))
  if_not_in_stop(poly, col_name, "poly", "col")

  if (!is.numeric(poly[[col_name]])) rlang::abort(glue::glue("{col_name} is not numeric."))


  #------ Prepare data

  if(!is.null(buffer)){ buffer <- buffer_bbox(poly, buffer) } else { buffer <- NULL }

  classes <- classInt::classIntervals(poly[[col_name]], n = n, style = style)
  col_class_name <- paste0(col_name, "_class")

  poly <- poly |>
    dplyr::mutate("{col_class_name}" := cut({{ col }}, classes$brks, include.lowest = TRUE))

  legend_labels <- c(levels(poly[[col_class_name]]), text_NA)

  #------ Make map layer

  layer <- ggplot() +
    geom_sf(data = poly, aes(fill = !!rlang::sym(col_class_name)), color = "transparent") +
    scale_fill(palette = palette, discrete = discrete, reverse_guide = FALSE, name = legend_title, labels = legend_labels, drop = drop) +
    theme_void() +
    theme(
      # legend.justification defines the edge of the legend that the legend.position coordinates refer to
      legend.justification = c(0, 1),
      # Set the legend flush with the left side of the plot, and just slightly below the top of the plot
      legend.position = legend_position,
      # Set fontfamily
      text = element_text(family = fontfamily)
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(color = cols_reach("main_lt_grey"))))

  return(layer)

}



add_labs <- function(map = NULL, title = NULL, subtitle = NULL){

}


#------ example
tt <- add_indicator_layer(indicator_admin1, opn_dfc, n = 5)

