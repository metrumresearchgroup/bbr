.onLoad <- function(libname, pkgname) {

  # set bbi executable path
  if (is.null(getOption("bbr.bbi_exe_path"))) {
    options("bbr.bbi_exe_path" = "bbi")
  }

  # set bbi minimum version
  if (is.null(getOption("bbr.bbi_min_version"))) {
    options("bbr.bbi_min_version" = package_version("2.3.0"))
  }

  # by default turn on strict mode to check for malformed objects and classes, etc.
  if (is.null(getOption("bbr.strict"))) {
    options("bbr.strict" = TRUE)
  }
}
