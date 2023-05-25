pal_fallback <- function(reverse = FALSE,
                         color_ramp_palette = FALSE,
                         discrete = FALSE,
                         n = 5,
                         ...){

  pal <- if(discrete) { viridisLite::viridis(n) } else {viridisLite::magma(n)}

  if (reverse) pal <- rev(pal)

  if (color_ramp_palette) {
    rlang::check_installed("grDevices", reason = "Package \"grDevices\" needed for `pal_fallback()` with 'color_ramp_palette' set to `TRUE` to work. Please install it.")

    pal <- grDevices::colorRampPalette(pal, ...)
  }

  return(pal)

}
