% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-summaries.R
\name{as_summary_list}
\alias{as_summary_list}
\alias{as_summary_list.bbi_summary_log_df}
\title{Convert object to \code{bbi_summary_list}}
\usage{
as_summary_list(.sums)

\method{as_summary_list}{bbi_summary_log_df}(.sums)
}
\arguments{
\item{.sums}{Object to convert.}
}
\description{
This is used to convert an object containing \verb{bbi_\{.model_type\}_summary} objects into a \code{bbi_summary_list}.
Currently it is only used for converting a \code{bbi_summary_log_df} into a \code{bbi_summary_list}
(primarily so that it can more easily be mapped over), but theoretically it could be used for other purposes in the future.
Note this is primarily intended as a developer function, though it was exposed because users may have a use for it as well.
}
\section{Methods (by class)}{
\itemize{
\item \code{as_summary_list(bbi_summary_log_df)}: Convert a \code{bbi_summary_log_df} into a \code{bbi_summary_list}

}}
