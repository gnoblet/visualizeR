% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pal_reach.R
\name{pal_reach}
\alias{pal_reach}
\title{Return function to interpolate a REACH color palette}
\usage{
pal_reach(
  palette = "main",
  reverse = FALSE,
  color_ramp_palette = FALSE,
  show_palettes = FALSE,
  ...
)
}
\arguments{
\item{palette}{Character name of a palette in REACH palettes}

\item{reverse}{Boolean indicating whether the palette should be reversed}

\item{color_ramp_palette}{Should the output be a `grDevices::colorRampPalette` function or a vector of hex codes? Default to the former with `TRUE`}

\item{show_palettes}{Should the ouput be the set of palettes names to pick from? Default to `FALSE`}

\item{...}{Additional arguments to pass to colorRampPalette()}
}
\value{
A color palette
}
\description{
Return function to interpolate a REACH color palette
}
