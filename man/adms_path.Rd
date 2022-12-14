% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adms_path.R
\name{adms_path}
\alias{adms_path}
\title{ADMS path}
\usage{
adms_path(datetime, adms_version, basedir = "/home/dataop/data/nmodel/adms")
}
\arguments{
\item{datetime}{(character) Basefolder of ADMS results. Default is "/home/dataop/data/nmodel/adms"}

\item{version}{(character) e.g. "hkv20b"}
}
\value{
An object of class \code{character}
}
\description{
Finds path to ADMS file (.nc) based on datetime and version
}
\keyword{adms}
