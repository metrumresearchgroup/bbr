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


#' Skip test if not on Metworx or in CI
#'
#' Checks for Metworx and CI environment variables and skips the test if neither are found.
#' This is primarily used for tests that require `bbi` to be installed.
#' @param .test_name Character scalar to identify the test being potentially skipped.
#'   This is printed in the skip message
#' @keywords internal
skip_if_not_ci_or_metworx <- function(.test_name) {
  if (Sys.getenv("METWORX_VERSION") == "" && Sys.getenv("CI") != "true") {
    testthat::skip(paste(.test_name, "only runs on Metworx or CI"))
  }
}

#' Skip long-running tests
#'
#' For example, tests that actual submit models to be run.
#' @param .explanation Reason for skipping tests, or description of tests being skipped
#' @keywords internal
skip_long_tests <- function(.explanation = "Skipping long running tests") {
  if (Sys.getenv("SKIP_LONG_TESTS") == "true") {
    testthat::skip(.explanation)
  }
}

#' Skip test if current bbi version is below specified one.
#'
#' @param v a package version (or a string that can be converted to one) to pass
#'   as the .min_version argument of `test_bbi_version()`.
#' @keywords internal
skip_if_old_bbi <- function(v) {
  if (!test_bbi_version(read_bbi_path(), v)) {
    testthat::skip(
      glue("bbi version is {bbi_version()}. Test requires version {v} or later"))
  }
}

#' Skip test if current nmrec version is below specified one.
#'
#' @param v a package version (or a string that can be converted to one)
#' @keywords internal
skip_if_old_nmrec <- function(v) {
  test_nmrec <- utils::packageVersion("nmrec") >= v
  if (!test_nmrec) {
    testthat::skip(
      glue("nmrec version is {utils::packageVersion('nmrec')}. Test requires version {v} or later"))
  }
}

#' Return path to mpiexec executable to use in tests.
#'
#' @noRd
get_mpiexec_path <- function() {
  res <- Sys.getenv(
    "BBR_TESTS_MPIEXEC_PATH",
    unset = unname(Sys.which("mpiexec"))
  )
  if (nchar(res)) {
    return(res)
  }

  # Fall back to bbi default.
  return("/usr/local/mpich3/bin/mpiexec")
}
