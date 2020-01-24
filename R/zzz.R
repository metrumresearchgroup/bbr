.onLoad <- function(libname, pkgname) {

  # set bbi executable path
  if (is.null(getOption("rbabylon.bbi_exe_path"))) {
    options("rbabylon.bbi_exe_path" = "/data/apps/bbi")
  }

  # set NONMEM directory path for bbi init
  if (is.null(getOption("rbabylon.nonmem_dir"))) {
    options("rbabylon.nonmem_dir" = "/opt/NONMEM")
  }

  # set default NONMEM version
  if (is.null(getOption("rbabylon.nonmem_default_version"))) {
    options("rbabylon.nonmem_default_version" = "nm74gf_nmfe")
  }

  # define valid NONMEM arguments
  if (is.null(getOption("rbabylon.nonmen_args"))) {
    options("rbabylon.nonmen_args" = list(
      nonmem_version = list(type = "character", flag="--nmVersion"),
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


# .onUnload <- function(libname, pkgname) {
#
#   # unset bbi executable path
#   options("rbabylon.bbi_exe_path" = NULL)
#
#   # unset NONMEM directory path for bbi init
#   options("rbabylon.nonmem_dir" = NULL)
#
#   # unset default NONMEM version
#   options("rbabylon.nonmem_default_version" = NULL)
#
#   # undefine valid NONMEM arguments
#   options("rbabylon.nonmen_args" = NULL)
# }
