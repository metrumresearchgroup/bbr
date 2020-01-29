.onLoad <- function(libname, pkgname) {

  # set bbi executable path
  if (is.null(getOption("rbabylon.bbi_exe_path"))) {
    options("rbabylon.bbi_exe_path" = "/data/apps/bbi")
  }

  # set NONMEM directory path for bbi init
  if (is.null(getOption("rbabylon.nonmem_dir"))) {
    options("rbabylon.nonmem_dir" = "/opt/NONMEM")
  }
}
