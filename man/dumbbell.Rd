% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dumbbell.R
\name{dumbbell}
\alias{dumbbell}
\title{Make dumbbell chart.}
\usage{
dumbbell(
  df,
  col,
  group_x,
  group_y,
  point_size = 5,
  point_alpha = 1,
  segment_size = 2.5,
  segment_color = cols_reach("main_lt_grey"),
  group_x_title = NULL,
  group_y_title = NULL,
  x_title = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  line_to_y_axis = TRUE,
  line_to_y_axis_type = 3,
  line_to_y_axis_width = 0.5,
  line_to_y_axis_color = cols_reach("main_grey"),
  add_text = TRUE,
  add_text_vjust = 2,
  add_text_size = 3.5,
  add_text_color = cols_reach("main_grey"),
  theme = theme_reach(palette = "primary")
)
}
\arguments{
\item{df}{A data frame.}

\item{col}{A numeric column.}

\item{group_x}{The grouping column on the x-axis; only two groups.}

\item{group_y}{The grouping column on the y-axis.}

\item{point_size}{Point size.}

\item{point_alpha}{Point alpha.}

\item{segment_size}{Segment size.}

\item{segment_color}{Segment color.}

\item{group_x_title}{X-group and legend title.}

\item{group_y_title}{Y-axis and group title.}

\item{x_title}{X-axis title.}

\item{title}{Title.}

\item{subtitle}{Subtitle.}

\item{caption}{Caption.}

\item{line_to_y_axis}{TRUE or FALSE; add a line connected points and Y-axis.}

\item{line_to_y_axis_type}{Line to Y-axis type.}

\item{line_to_y_axis_width}{Line to Y-axis width.}

\item{line_to_y_axis_color}{Line to Y-axis color.}

\item{add_text}{TRUE or FALSE; add text at the points.}

\item{add_text_vjust}{Vertical adjustment.}

\item{add_text_size}{Text size.}

\item{add_text_color}{Text color.}

\item{theme}{A ggplot2 theme, default to `theme_reach()`}
}
\value{
A dumbbell chart.
}
\description{
Make dumbbell chart.
}
