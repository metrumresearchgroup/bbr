skip_if_over_rate_limit <- function(by = 5) {

  res <- jsonlite::fromJSON("https://api.github.com/rate_limit", simplifyDataFrame = FALSE)

  res <- res$rate$remaining

  if (is.null(res) || res <= by) skip("Over the GitHub rate limit")
}
