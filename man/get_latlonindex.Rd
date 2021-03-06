% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/limit_extent.R
\name{get_latlonindex}
\alias{get_latlonindex}
\title{Get the indices in the original lat/lon matrices that cover the trimmed
matrices.}
\usage{
get_latlonindex(lat_lon, lat_lon_trim)
}
\arguments{
\item{lat_lon}{List holding the initial lat_lon matrices}

\item{lat_lon_trim}{List holding the trimmed lat_lon matrices}
}
\value{
Returns the updated longitude indices to which the matrices will be
  reduced.
}
\description{
Get the indices in the original lat/lon matrices that cover the trimmed
matrices.
}
\keyword{internal}
