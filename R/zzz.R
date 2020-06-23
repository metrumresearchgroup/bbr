.onLoad <- function(libname, pkgname) {

  # set bbi executable path
  # if (is.null(getOption("rbabylon.bbi_exe_path"))) {
  #   #options("rbabylon.bbi_exe_path" = "bbi")
  #   warning("options('rbabylon.bbi_exe_path') is NULL.... ADD OTHER STUFF IF WE GO THIS ROUTE")
  # }

  # set bbi minimum version
  if (is.null(getOption("rbabylon.bbi_min_version"))) {
    options("rbabylon.bbi_min_version" = package_version("2.2.0"))
  }

  # by default turn on strict mode to check for malformed objects and classes, etc.
  if (is.null(getOption("rbabylon.strict"))) {
    options("rbabylon.strict" = TRUE)
  }
}
