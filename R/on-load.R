#' @import checkmate
.onLoad <- function(libname, pkgname) {

  # set bbi executable path
  if (is.null(getOption("rbabylon.bbi_exe_path"))) {
    options("rbabylon.bbi_exe_path" = "bbi")
  }

  # set valid NONMEM arguments
  if (is.null(getOption("rbabylon.nonmen_args"))) {
    options("rbabylon.nonmen_args" = list(
      arg1 = "numeric",
      arg2 = "character",
      arg3 = "logical"
      ))
  }
}



