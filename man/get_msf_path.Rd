% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-path-from-object.R
\name{get_msf_path}
\alias{get_msf_path}
\title{Get path to \code{MSF} file path}
\usage{
get_msf_path(.mod, .check_exists = TRUE)
}
\arguments{
\item{.mod}{a \code{bbi_model} object}

\item{.check_exists}{If \code{TRUE}, the default, will throw an error if the file does not exist}
}
\value{
absolute file path to \code{MSF} file, or \code{NULL} if one doesnt exist.
}
\description{
Extract \code{MSF} file path as defined in a \verb{$ESTIMATION} record in a \code{NONMEM}
control stream file, and construct absolute file path
}
\keyword{internal}
