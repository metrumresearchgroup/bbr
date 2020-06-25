#' Skip test if over Github API rate limit
#'
#' This is based on a function of the same name from the `remotes` package.
#' Note, this uses no token, so it is checking the public rate limit.
#' This is appropriate because this package has no interface for passing in a token, so it will always be hitting the public API.
#' @param by if less than this number of requests are left before hitting the rate limit, skip the test
skip_if_over_rate_limit <- function(by = 5) {

  res <- jsonlite::fromJSON("https://api.github.com/rate_limit", simplifyDataFrame = FALSE)

  res <- res$rate$remaining

  if (is.null(res) || res <= by) skip("Over the GitHub rate limit")
}
