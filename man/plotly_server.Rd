% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotly_server.R
\name{plotly_server}
\alias{plotly_server}
\title{Plotly Server}
\usage{
plotly_server(
  id,
  plot_data,
  group_data = shiny::reactive(NULL),
  eventdata = shiny::reactive(NULL)
)
}
\arguments{
\item{id}{Module ID}

\item{plot_data}{A shiny::reactive that returns a dataframe}

\item{group_data}{A shiny::reactive that returns NULL or a dataframe}

\item{eventdata}{A shiny::reactive that returns NULL or a dataframe}
}
\description{
Plotly Server
}
