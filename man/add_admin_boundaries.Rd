% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map.R
\name{add_admin_boundaries}
\alias{add_admin_boundaries}
\title{Add admin boundaries (lines) and the legend}
\usage{
add_admin_boundaries(
  lines,
  colors,
  labels,
  lwds,
  title = "",
  buffer = NULL,
  ...
)
}
\arguments{
\item{lines}{List of multiline shape defined by sf package.}

\item{colors}{Vector of hexadecimal codes. Same order as lines.}

\item{labels}{Vector of labels in the legend. Same order as lines.}

\item{lwds}{Vector of line widths. Same order as lines.}

\item{title}{Legend title.}

\item{buffer}{A buffer, either one value or a vector of 4 values (left, bottom, right, top).}

\item{...}{Other arguments to pass to each shape in `tmap::tm_lines()`.}
}
\value{
A tmap layer.
}
\description{
Add admin boundaries (lines) and the legend
}
