% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alluvial.R
\name{alluvial}
\alias{alluvial}
\title{Simple alluvial chart}
\usage{
alluvial(
  df,
  from,
  to,
  value,
  group = NULL,
  alpha = 0.5,
  from_levels = NULL,
  value_title = NULL,
  group_title = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  rect_color = cols_reach("white"),
  rect_border_color = cols_reach("main_grey"),
  rect_text_color = cols_reach("main_grey"),
  theme = theme_reach(axis_y = FALSE, legend_position = "none")
)
}
\arguments{
\item{df}{A data frame.}

\item{from}{A character column of upstream stratum.}

\item{to}{A character column of downstream stratum.}

\item{value}{A numeric column of values.}

\item{group}{The grouping column to fill the alluvium with.}

\item{alpha}{Fill transparency. Default to 0.5.}

\item{from_levels}{Order by given from levels?}

\item{value_title}{The value/y scale title. Default to NULL.}

\item{group_title}{The group title. Default to NULL.}

\item{title}{Plot title. Default to NULL.}

\item{subtitle}{Plot subtitle. Default to NULL.}

\item{caption}{Plot caption. Default to NULL.}

\item{rect_color}{Stratum rectangles' fill color.}

\item{rect_border_color}{Stratum rectangles' border color.}

\item{rect_text_color}{Stratum rectangles' text color.}

\item{theme}{Whatever theme. Default to theme_reach().}
}
\value{
A donut chart to be used parsimoniously
}
\description{
Simple alluvial chart
}
