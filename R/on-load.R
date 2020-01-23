.onLoad <- function(libname, pkgname) {

  # set bbi executable path
  if (is.null(getOption("rbabylon.bbi_exe_path"))) {
    options("rbabylon.bbi_exe_path" = "bbi")
  }

  # set valid NONMEM arguments
  if (is.null(getOption("rbabylon.nonmen_args"))) {
    options("rbabylon.nonmen_args" = list(
      threads = list(type = "numeric", flag="--threads"),
      debug = list(type = "logical", flag="--debug"),
      json = list(type = "logical", flag="--json"),
      no_cor_file = list(type = "logical", flag="--no-cor-file"),
      no_cov_file = list(type = "logical", flag="--no-cov-file"),
      no_ext_file = list(type = "logical", flag="--no-ext-file"),
      no_grd_file = list(type = "logical", flag="--no-grd-file"),
      no_shk_file = list(type = "logical", flag="--no-shk-file"),
      preview = list(type = "logical", flag="--preview"),
      verbose = list(type = "logical", flag="--verbose")
      ))
  }
}



