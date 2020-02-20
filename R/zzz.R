.onLoad <- function(libname, pkgname) {

  # set bbi executable path
  if (is.null(getOption("rbabylon.bbi_exe_path"))) {
    options("rbabylon.bbi_exe_path" = "bbi")
  }
  if (is.null(getOption("rbabylon.strict"))) {
    options("rbabylon.strict" = TRUE)
  }
}
