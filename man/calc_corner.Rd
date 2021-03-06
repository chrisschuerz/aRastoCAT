% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_polygon_grid.R
\name{calc_corner}
\alias{calc_corner}
\title{Helper function to calculate the mid point between four lat/lon points to
derive the corner points of the polygon grid cells.}
\usage{
calc_corner(ind, mtr)
}
\arguments{
\item{ind}{Index positions in the matrix}

\item{mtr}{matrix for which the mid points are calculated.}
}
\value{
Returns a matrix holding the interpolated mid points of the original
  matrix
}
\description{
Helper function to calculate the mid point between four lat/lon points to
derive the corner points of the polygon grid cells.
}
\keyword{internal}
