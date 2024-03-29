% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test-threads.R
\name{check_run_times}
\alias{check_run_times}
\title{Check estimation time for models run with various threads values}
\usage{
check_run_times(.mods, .return_times = "estimation_time", .wait = TRUE, ...)
}
\arguments{
\item{.mods}{a bbi model object or list of model objects. Generally created by \code{test_threads()}.}

\item{.return_times}{character vector denoting which times from \code{model_summary()} you want to return.
See details for more information.}

\item{.wait}{logical. If TRUE, pass \code{.mods} to \code{wait_for_nonmem()} before returning results.}

\item{...}{args passed through to \code{wait_for_nonmem()}.}
}
\value{
A tibble with columns \code{threads} (number of threads) and \code{time}
(elapsed estimation time in seconds for test models).
}
\description{
Check estimation time for models run with various threads values
}
\details{
\code{.return_times} can be any subset of \code{c("estimation_time", "covariance_time", "postprocess_time", "cpu_time")}.
Users can also specify \code{"all"}, which is the shorthand method for selecting all 4 of those columns.
}
\examples{
\dontrun{
mods <- test_threads(mod, .threads = c(2, 4))

# If models have not finished:
check_run_times(mods, .wait = TRUE, .time_limit = 300)
check_run_times(mods, .wait = TRUE, .return_times = c("estimation_time", "covariance_time"))

# If models have already finished:
check_run_times(mods, .wait = FALSE)
check_run_times(mods, .wait = FALSE, .return_times = "all")
}
}
