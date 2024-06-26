% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modify-records.R
\name{assert_record_type}
\alias{assert_record_type}
\title{Helper for checking if a specified record type is valid.}
\usage{
assert_record_type(.mod, type)
}
\arguments{
\item{.mod}{a bbr model object}

\item{type}{record type. This may be spelled any way that's accepted in a
\code{NONMEM} control stream.}
}
\description{
Checks if the specified record type is valid. Note that this does \emph{not} check
if the record is present in the control stream, just whether \code{nmrec} recognizes
the spelling.
}
\details{
This function is basically meant to ensure that \code{type} is an available
record name within \code{ls(nmrec:::record_names)}. This function should search
there (and no longer require a model object) if those names become exported.

Run the following command to see what record types are available/supported by
\code{nmrec}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{`ls(nmrec:::record_names)`
}\if{html}{\out{</div>}}
}
\keyword{internal}
