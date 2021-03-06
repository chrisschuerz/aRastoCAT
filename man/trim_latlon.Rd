% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/limit_extent.R
\name{trim_latlon}
\alias{trim_latlon}
\title{Trim the lat/lon matrices to the extent of the provided shape file}
\usage{
trim_latlon(lat, lon, bbox)
}
\arguments{
\item{lat}{Matrix holding the latitude values of each pixel}

\item{lon}{Matrix holding the longitude values of each pixel}

\item{bbox}{Boundary box of the shape file}
}
\value{
Returns a list of length 2 holding the trimmed lat and lon matrices.
}
\description{
Trim the lat/lon matrices to the extent of the provided shape file
}
\keyword{internal}
