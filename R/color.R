#' Helpers to extract defined colors as hex codes
#'
#' [color()] returns the requested columns, returns NA if absent. [color_pattern()] returns all colors that start with the pattern.
#'
#' @param ... Character names of colors. If NULL returns all colors.
#' @param unname Boolean. Should the output vector be unnamed? Default to `TRUE`.
#' @section Naming of colors:
#' * All branding colors start with "branding";
#' * All , categorical colors start with ", cat_";
#' * All sequential colors start with "seq_";
#'
#' Then, a number indi, cates the number of colors that belong to the palettes, a string the name of the palette, and, finally, a number the position of the color. E.g., "seq_5_red_4" would be the 4th color of a continuous palettes of 5 colors in the red band. Exception is made for white, light_grey, dark_grey, and black.
#'
#'
#' @return Hex codes named or unnamed.
#'
#' @export
color <- function(..., unname = TRUE) {

  #------ Prep

  # Retrieve colors
  cols <- c(...)

  # Defined colors
  colors <- c(
    white = "#FFFFFF"
    , lighter_grey = "#F5F5F5"
    , light_grey = "#E3E3E3"
    , dark_grey = "#464647"
    , light_blue_grey = "#B3C6D1"
    , grey = "#71716F"
    , black = "#000000"
    , cat_2_yellow_1 = "#ffc20a"
    , cat_2_yellow_2 = "#0c7bdc"
    , cat_2_light_1 = "#fefe62"
    , cat_2_light_2 = "#d35fb7"
    , cat_2_green_1 = "#1aff1a"
    , cat_2_green_2 = "#4b0092"
    , cat_2_blue_1 = "#1a85ff"
    , cat_2_blue_2 = "#d41159"
    , cat_5_main_1 = "#083d77" # yale blue
    , cat_5_main_2 = "#4ecdc4" # robin egg blue
    , cat_5_main_3 = "#f4c095" # peach
    , cat_5_main_4 = "#b47eb3" # african violet
    , cat_5_main_5 = "#ffd5ff" # mimi pink
    , seq_5_main_1 = "#083d77" # yale blue
    , seq_5_main_2 = "##396492"
    , seq_5_main_3 = "#6b8bad"
    , seq_5_main_4 = "#9cb1c9"
    , seq_5_main_5 = "#ced8e4"
    , cat_5_ibm_1 = "#648fff"
    , cat_5_ibm_2 = "#785ef0"
    , cat_5_ibm_3 = "#dc267f"
    , cat_5_ibm_4 = "#fe6100"
    , cat_5_ibm_5 = "#ffb000"
    , cat_3_aquamarine_1 = "aquamarine2"
    , cat_3_aquamarine_2 = "cornflowerblue"
    , cat_3_aquamarine_3 = "brown1"
    , cat_3_tol_high_contrast_1 = "#215589"
    , cat_3_tol_high_contrast_2 = "#cfaa34"
    , cat_3_tol_high_contrast_3 = "#a35364"
    , cat_8_tol_adapted_1 = "#332e86"
    , cat_8_tol_adapted_2 = "#50504f"
    , cat_8_tol_adapted_3 = "#3dab9a"
    , cat_8_tol_adapted_4 = "#86ccee"
    , cat_8_tol_adapted_5 = "#ddcb77"
    , cat_8_tol_adapted_6 = "#ee5859"
    , cat_8_tol_adapted_7 = "#aa4599"
    , cat_8_tol_adapted_8 = "#721220"
    , div_5_orange_blue_1 = "#c85200"
    , div_5_orange_blue_2 = "#e48646"
    , div_5_orange_blue_3 = "#cccccc"
    , div_5_orange_blue_4 = "#6b8ea4"
    , div_5_orange_blue_5 = "#366785"
    , div_5_green_purple_1 = "#c85200"
    , div_5_green_purple_2 = "#e48646"
    , div_5_green_purple_3 = "#cccccc"
    , div_5_green_purple_4 = "#6b8ea4"
    , div_5_green_purple_5 = "#366785"
  )


  #------ Checks

  # Check that if ... is not null, all colors are defined
  if (!is.null(cols)) {
    if (cols %notallin% names(colors)) {
      rlang::abort(c(
        "Some colors not defined",
        "*" = glue::glue_collapse(...[which(!... %in% names(cols))], sep  = ", ", last = ", and "),
        "i" = "Use `color(unname = FALSE)` to see all named available colors."
      )
    )
    }
  }

  # ------ Return

  if (is.null(cols)) {
    cols_to_return <- colors
  } else {
    cols_to_return <- colors[cols]
  }

  if (unname) {
    cols_to_return <- unname(cols_to_return)
  }

  return(cols_to_return)
}

#' @rdname color
#'
#' @param pattern Pattern of the start of colors' name.
#'
#' @export
color_pattern <- function(pattern, unname = TRUE){

  #------ Checks

  # Check that pattern is a character scalar
  checkmate::assert_character(pattern, len = 1)

  # Check that unname is a logical scalar
  checkmate::assert_logical(unname, len = 1)

  #------ Get colors

  # Get colors
  col <- color(unname = FALSE)
  col <- col[startsWith(names(col), pattern)]

  if (unname) {
    col <- unname(col)
  }

  # If col is of length 0, warn
  if (length(col) == 0) {
    rlang::warn(c(
      "No colors match the pattern",
      "*" = glue::glue("Pattern used is:'{pattern}'"),
      "i" = "Use `color(unname = FALSE)` to see all named available colors."
    ))
  }

  return(col)
}
