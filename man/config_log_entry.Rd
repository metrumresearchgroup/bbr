% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/config-log.R
\name{config_log_entry}
\alias{config_log_entry}
\title{Parse a bbi config file}
\usage{
config_log_entry(cfg_mod, path, fields = NULL)
}
\arguments{
\item{cfg_mod}{A model object.}

\item{path}{A string giving the path to \code{bbi_config.json} for \code{cfg_mod}.}

\item{fields}{A character vector of fields to include.}
}
\value{
A list whose elements include
\itemize{
\item the path to the model file (minus extension)
\item \code{fields}
\item whether the model file has changed
\item whether the data file has changed
\item the version of NONMEM
}

The return value is \code{NULL} if any element of \code{fields} is not found in
\code{path}.
}
\description{
Parse a bbi config file
}
\keyword{internal}
