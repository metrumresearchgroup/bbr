.onLoad <- function(libname, pkgname) {

  # set bbi minimum version
  .dir <- stringr::str_replace(getwd(), "rbabylon/?.*", "rbabylon")
  options("rbabylon.bbi_min_version" = parse_description_version_spec(file.path(.dir, "DESCRIPTION"), "SystemRequirements", "babylon"))

  # set bbi executable path
  if (is.null(getOption("rbabylon.bbi_exe_path"))) {
    options("rbabylon.bbi_exe_path" = "bbi")
  }

  # by default turn on strict mode to check for malformed objects and classes, etc.
  if (is.null(getOption("rbabylon.strict"))) {
    options("rbabylon.strict" = TRUE)
  }
}
