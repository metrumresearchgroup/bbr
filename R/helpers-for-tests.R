#' Skip test if over Github API rate limit
#'
#' This is based on a function of the same name from the `remotes` package.
#' Note, this uses no token, so it is checking the public rate limit.
#' This is appropriate because this package has no interface for passing in a token, so it will always be hitting the public API.
#' @importFrom utils download.file
#' @importFrom jsonlite fromJSON
#' @param by if less than this number of requests are left before hitting the rate limit, skip the test
#' @keywords internal
skip_if_over_rate_limit <- function(by = 5) {

  tmp <- tempfile(fileext = '.json')

  on.exit(unlink(tmp),add = TRUE)

  download.file("https://api.github.com/rate_limit", destfile = tmp, quiet = TRUE)

  res <- jsonlite::fromJSON(tmp, simplifyDataFrame = FALSE)

  res <- res$rate$remaining

  if (is.null(res) || res <= by) testthat::skip("Over the GitHub rate limit")
}


#' Skip test if not on Metworx or Drone
#'
#' Checks for Metworx and Drone environment variables and skips the test if neither are found.
#' This is primarily used for tests that require `bbi` to be installed.
#' @param .test_name Character scalar to identify the test being potentially skipped.
#'   This is printed in the skip message
#' @keywords internal
skip_if_not_drone_or_metworx <- function(.test_name) {
  if (Sys.getenv("METWORX_VERSION") == "" && Sys.getenv("DRONE") != "true") {
    testthat::skip(paste(.test_name, "only runs on Metworx or Drone"))
  }
}

#' Skip test if bbi version is as old or older than the given version.
#'
#' If the version can't be parsed by [package_version()], the test is never
#' skipped.
#'
#' @param v a package version or a string that can be converted to one
skip_if_old_bbi <- function(v) {
    v_bbi <- package_version(bbi_version(), strict = FALSE)
    if (!is.na(v_bbi)) {
      if (v_bbi <= package_version(v)) {
        testthat::skip(
          glue("bbi version is  {v_bbi}. Test requires bbi version > {v}"))
      }
    }
}
