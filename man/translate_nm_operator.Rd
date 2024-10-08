% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter-nm-data.R
\name{translate_nm_operator}
\alias{translate_nm_operator}
\title{Function to translate \code{NONMEM} operators to \code{R} operators}
\usage{
translate_nm_operator(expr)
}
\arguments{
\item{expr}{String. A \code{NONMEM} ignore/accept expression}
}
\value{
A \code{\link[dplyr:filter]{dplyr::filter()}} expression
}
\description{
Function to translate \code{NONMEM} operators to \code{R} operators
}
\note{
\code{.EQN.} and \code{.NEN.} are available after \verb{NONMEM 7.3}
}
\examples{
\dontrun{

translate_nm_operator(c("AGE.NE.30", "ID.EQ.2", "WT/=70"))
#> [1] "AGE!=30" "ID==2" "WT!=70"
}
}
\seealso{
\code{\link[=invert_operator]{invert_operator()}}, \code{\link[=translate_nm_expr]{translate_nm_expr()}}
}
\keyword{internal}
