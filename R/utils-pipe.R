#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

if (getRversion() >= "2.15.1") {
  # Prevent 'R CMD check' from complaining about "." argument used in pipes.
  utils::globalVariables(".")
}
