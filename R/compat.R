# Compatibility kludges

# dplyr 1.1.0
#
#  * introduced reframe() and deprecated returning multiple rows via summarise()
if (utils::packageVersion("dplyr") < "1.1.0") {
  reframe <- dplyr::summarise
} else {
  reframe <- dplyr::reframe
}
