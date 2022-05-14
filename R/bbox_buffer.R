#' @title Bbbox buffer
#'
#' @param sf_obj A `sf` object
#' @param buffer A buffer, either one value or a vector of 4 values (left, bottom, right, top). Default to 0.
#'
#' @return A bbox with a buffer
#'
#' @export
buffer_bbox <- function(sf_obj, buffer = 0){

  rlang::check_installed("sf", reason = "Package \"sf\" needed for `buffer_bbox()` to work. Please install it.")


  if (!(length(buffer) %in% c(1,4)) | !is.numeric(buffer)) stop("Please provide a numeric buffer of length 1 or 4.")

  bbox <- sf::st_bbox(sf_obj)
  xrange <- bbox$xmax - bbox$xmin # range of x values
  yrange <- bbox$ymax - bbox$ymin # range of y values


  bbox_with_buffer <- if (length(buffer) == 1) {
    c(
      bbox[1] - (buffer * xrange), # xmin - left
      bbox[2] - (buffer * yrange), # ymin - bottom
      bbox[3] + (buffer * xrange), # xmax - right
      bbox[4] + (buffer * yrange) # ymax - top
    )
  } else if (length(buffer) == 4) {
    c(
      bbox[1] - (buffer[1] * xrange), # xmin - left
      bbox[2] - (buffer[2] * yrange), # ymin - bottom
      bbox[3] + (buffer[3] * xrange), # xmax - right
      bbox[4] + (buffer[4] * yrange) # ymax - top
    )
  } else {
    print("Missed something while writing the funtion.")
  }

}
