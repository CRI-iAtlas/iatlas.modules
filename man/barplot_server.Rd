% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/barplot_server.R
\name{barplot_server}
\alias{barplot_server}
\title{Barplot Server}
\usage{
barplot_server(
  id,
  plot_data_function,
  feature_classes = shiny::reactive(NULL),
  barplot_xlab = shiny::reactive(""),
  barplot_ylab = shiny::reactive(""),
  barplot_title = shiny::reactive(""),
  barplot_label = shiny::reactive("Feature"),
  drilldown = shiny::reactive(F),
  ...
)
}
\arguments{
\item{id}{Module ID}

\item{plot_data_function}{A shiny::reactive that returns a function
The function must take an argument called ".feature_class" and return a
dataframe with columns "sample", "feature", "feature_value", "group",
and optionally "group_description"}

\item{feature_classes}{A shiny::reactive that returns a vector of strings.
One of these strings are passed to plot_data_function}

\item{barplot_xlab}{A shiny::reactive that returns a string}

\item{barplot_ylab}{A shiny::reactive that returns a string}

\item{barplot_title}{A shiny::reactive that returns a string}

\item{barplot_label}{A shiny::reactive that returns a string}

\item{drilldown}{A shiny::reactive that returns True or False}

\item{...}{shiny::reactives passed to drilldown_scatterplot_server}
}
\description{
Barplot Server
}
