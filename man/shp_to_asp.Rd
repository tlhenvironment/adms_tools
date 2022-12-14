% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shp_to_asp.R
\name{shp_to_asp}
\alias{shp_to_asp}
\title{Convert shp to .ASP for ADMS runs}
\usage{
shp_to_asp(shp, filepath, height = 3)
}
\arguments{
\item{shp}{(sf shapefile) Input sf shapefile}

\item{filepath}{(character) Path for saving file}

\item{height}{(numeric) Height of the receptor points (in m). Defaults to 3}
}
\value{
No return
}
\description{
Converts a shapefile into a .ASP file, which denotes static ADMS output points
}
\keyword{adms,}
\keyword{asp,}
\keyword{monitors}
