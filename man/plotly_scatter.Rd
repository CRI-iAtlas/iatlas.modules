% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotly_scatter.R
\name{plotly_scatter}
\alias{plotly_scatter}
\title{Plotly Scatter}
\usage{
plotly_scatter(
  plot_data,
  x_col = "x",
  y_col = "y",
  color_col = x_col,
  key_col = x_col,
  text_col = x_col,
  xlab = "",
  ylab = "",
  title = "",
  source_name = NULL,
  identity_line = FALSE,
  horizontal_line = FALSE,
  horizontal_line_y = NULL,
  format_func = format_plotly
)
}
\arguments{
\item{plot_data}{A dataframe}

\item{x_col}{A string}

\item{y_col}{A string}

\item{color_col}{A string or NA}

\item{key_col}{A string or NA}

\item{text_col}{A string or NA}

\item{xlab}{A string}

\item{ylab}{A string}

\item{title}{A string}

\item{source_name}{A string or NULL}

\item{identity_line}{True or False}

\item{horizontal_line}{True or False}

\item{horizontal_line_y}{A numeric or NULL}

\item{format_func}{A function}
}
\description{
Plotly Scatter
}
