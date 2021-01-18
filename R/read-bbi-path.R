#' Set the path to the bbi executable
#'
#' [withr::with_options()] is used throughout package tests to set the path to
#' the bbi executable. The test suite will fail if this path does not exist,
#' which limits flexibility across environments, e.g., in continuous
#' integration. Any solution that decouples the `with_options()` pattern from a
#' particular default path must work with [devtools::check()], which runs in a
#' separate process. We can inject environment variables into `check()`, so the
#' default behavior is to rely on such a variable to set the path, falling back
#' to a default path if unset.
#'
#' @param path path to the bbi executable
#' @param default a default path if `path` is `NA`
#'
#' @return `path` if not `NA`, else `default`
#' @examples
#' read_bbi_path()
#' @keywords internal
#' @export
read_bbi_path <- function(path = Sys.getenv("BBI_EXE_PATH", NA),
                          default = BBI_DEFAULT_PATH) {
  checkmate::assert_string(path, na.ok = TRUE)
  checkmate::assert_string(default)

  if (is.na(path)) {
    path <- default
  }
  path
}
