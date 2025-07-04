% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lollipop.R
\name{hlollipop}
\alias{hlollipop}
\alias{lollipop}
\title{Simple lollipop chart}
\usage{
hlollipop(..., flip = TRUE, theme_fun = theme_lollipop(flip = flip))

lollipop(
  df,
  x,
  y,
  group = "",
  facet = "",
  order = "y",
  x_rm_na = TRUE,
  y_rm_na = TRUE,
  group_rm_na = TRUE,
  facet_rm_na = TRUE,
  y_expand = 0.1,
  add_color = color("cat_5_main_1"),
  add_color_guide = TRUE,
  flip = FALSE,
  wrap = NULL,
  alpha = 1,
  x_title = NULL,
  y_title = NULL,
  group_title = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  dot_size = 4,
  line_size = 0.8,
  line_color = color("dark_grey"),
  dodge_width = 0.9,
  theme_fun = theme_lollipop(flip = flip, axis_text_x_angle = 0, axis_text_x_vjust = 0.5,
    axis_text_x_hjust = 0.5),
  scale_fill_fun = scale_fill_visualizer_discrete(),
  scale_color_fun = scale_color_visualizer_discrete()
)
}
\arguments{
\item{...}{Additional arguments passed to `lollipop()`}

\item{flip}{TRUE or FALSE (default). Default to TRUE or horizontal lollipop plot.}

\item{theme_fun}{Whatever theme function. For no custom theme, use theme_fun = NULL.}

\item{df}{A data frame.}

\item{x}{A quoted character column or coercible as a character column.}

\item{y}{A quoted numeric column.}

\item{group}{Some quoted grouping categorical column, e.g. administrative areas or population groups.}

\item{facet}{Some quoted grouping categorical column, e.g. administrative areas or population groups.}

\item{order}{A character scalar specifying the order type (one of "none", "y", "grouped"). See details.}

\item{x_rm_na}{Remove NAs in x?}

\item{y_rm_na}{Remove NAs in y?}

\item{group_rm_na}{Remove NAs in group?}

\item{facet_rm_na}{Remove NAs in facet?}

\item{y_expand}{Multiplier to expand the y axis.}

\item{add_color}{Add a color to dots (if no grouping).}

\item{add_color_guide}{Should a legend be added?}

\item{wrap}{Should x-labels be wrapped? Number of characters.}

\item{alpha}{Fill transparency for dots.}

\item{x_title}{The x scale title. Default to NULL.}

\item{y_title}{The y scale title. Default to NULL.}

\item{group_title}{The group legend title. Default to NULL.}

\item{title}{Plot title. Default to NULL.}

\item{subtitle}{Plot subtitle. Default to NULL.}

\item{caption}{Plot caption. Default to NULL.}

\item{dot_size}{The size of the dots.}

\item{line_size}{The size/width of the line connecting dots to the baseline.}

\item{line_color}{The color of the line connecting dots to the baseline.}

\item{dodge_width}{Width for position dodge when using groups (controls space between grouped lollipops).}

\item{scale_fill_fun}{Scale fill function. Default to scale_fill_visualizer_discrete().}

\item{scale_color_fun}{Scale color function. Default to scale_color_visualizer_discrete().}
}
\value{
A ggplot object
}
\description{
`lollipop()` is a simple lollipop chart (dots connected to the baseline by a segment) with some customization allowed.
`hlollipop()` uses `lollipop()` with sane defaults for a horizontal lollipop chart.
}
\examples{
\dontrun{
df <- data.frame(x = letters[1:5], y = c(10, 5, 7, 12, 8))
# Vertical lollipop
lollipop(df, "x", "y")
# Horizontal lollipop
hlollipop(df, "x", "y")
}
}
