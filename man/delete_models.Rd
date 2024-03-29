% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test-threads.R
\name{delete_models}
\alias{delete_models}
\title{Remove model files associated with the specified tags}
\usage{
delete_models(.mods, .tags = "test threads", .force = FALSE)
}
\arguments{
\item{.mods}{a bbi model object or list of model objects.}

\item{.tags}{a character vector identifying the tags of the models you want to delete.
If set to \code{NULL}, no filtering will be performed and all associated model files will be deleted.}

\item{.force}{logical (T/F). If \code{TRUE}, do not prompt the user if they want to delete the models.}
}
\description{
Remove model files associated with the specified tags
}
