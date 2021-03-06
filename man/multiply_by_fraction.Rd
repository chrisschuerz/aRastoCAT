% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregate_var.R
\name{multiply_by_fraction}
\alias{multiply_by_fraction}
\title{Helper function to multiply each 'timestep' column of the data set by the
areal fraction of each pixel (speeds up the process compared to mutate_at)}
\usage{
multiply_by_fraction(tbl)
}
\arguments{
\item{tbl}{The data table that holds the area fraction for all pixels, the
data for each time step and each pixel and the grouping variable index.}
}
\value{
Returns the tibble including the index and all timesteps multiplied
  by fraction.
}
\description{
Helper function to multiply each 'timestep' column of the data set by the
areal fraction of each pixel (speeds up the process compared to mutate_at)
}
\keyword{internal}
