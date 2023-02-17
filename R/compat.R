# Compatibility kludges

# dplyr 1.1.0
#
#  * introduced reframe() and deprecated returning multiple rows via summarise()
#
#  * added a 'multiple' argument to join functions to control the behavior when
#    an observation in x matches multiple observations in y. For equality and
#    rolling joins, it keeps the pre-1.1.0 behavior of returning all the rows
#    but warns.
if (utils::packageVersion("dplyr") < "1.1.0") {
  reframe <- dplyr::summarise
  left_join_all <- dplyr::left_join
} else {
  reframe <- dplyr::reframe
  left_join_all <- function(...) dplyr::left_join(..., multiple = "all")
}
