% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotly_bar.R
\name{plotly_bar}
\alias{plotly_bar}
\title{Plotly Bar}
\usage{
plotly_bar(
  plot_data,
  x_col = "x",
  y_col = "y",
  error_col = NA,
  color_col = x_col,
  key_col = x_col,
  text_col = x_col,
  xlab = "",
  ylab = "",
  title = "",
  source_name = NULL,
  bar_colors = NULL,
  format_func = format_plotly
)
}
\arguments{
\item{plot_data}{A dataframe}

\item{x_col}{A string}

\item{y_col}{A string}

\item{error_col}{A string or NA}

\item{color_col}{A string or NA}

\item{key_col}{A string or NA}

\item{text_col}{A string or NA}

\item{xlab}{A string}

\item{ylab}{A string}

\item{title}{A string}

\item{source_name}{A string or NULL}

\item{bar_colors}{A string or NULL}

\item{format_func}{A function}
}
\description{
Plotly Bar
}
