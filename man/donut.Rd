% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/donut.R
\name{donut}
\alias{donut}
\title{Simple donut chart (to be used parsimoniously), can be a pie chart}
\usage{
donut(
  df,
  x,
  y,
  alpha = 1,
  x_title = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  arrange = TRUE,
  hole_size = 3,
  add_text = TRUE,
  add_text_treshold_display = 5,
  add_text_color = "white",
  add_text_suffix = "",
  theme = theme_reach(legend_reverse = TRUE)
)
}
\arguments{
\item{df}{A data frame.}

\item{x}{A character column or coercible as a character column. Will give the donut's fill color.}

\item{y}{A numeric column.}

\item{alpha}{Fill transparency.}

\item{x_title}{The x scale title. Default to NULL.}

\item{title}{Plot title. Default to NULL.}

\item{subtitle}{Plot subtitle. Default to NULL.}

\item{caption}{Plot caption. Default to NULL.}

\item{arrange}{TRUE or FALSE. Arrange by highest percentage first.}

\item{hole_size}{Hole size. Default to 3. If less than 2, back to a pie chart.}

\item{add_text}{TRUE or FALSE. Add the value as text.}

\item{add_text_treshold_display}{Minimum value to add the text label.}

\item{add_text_color}{Text color.}

\item{add_text_suffix}{If percent is FALSE, should we add a suffix to the text label?}

\item{theme}{Whatever theme. Default to theme_reach().}
}
\value{
A donut chart to be used parsimoniously
}
\description{
Simple donut chart (to be used parsimoniously), can be a pie chart
}
