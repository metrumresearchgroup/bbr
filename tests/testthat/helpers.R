skip_if_over_rate_limit <- function(by = 5) {
  # this uses no token, so it is checking the public rate limit.
  # this is appropriate because this package has no interface for passing in a token, so it will always be hitting the public API.

  res <- jsonlite::fromJSON("https://api.github.com/rate_limit", simplifyDataFrame = FALSE)

  res <- res$rate$remaining

  if (is.null(res) || res <= by) skip("Over the GitHub rate limit")
}
