.onLoad <- function(libname, pkgname) {

  # if the option set before load, keep it, else look for an env var
  # if it isn't one of the false-y settings, then will set to true
  if (is.null(getOption("bbr.DEV_no_min_version")) &&
      !Sys.getenv("BBR_DEV_NO_MIN_VERSION", unset = "FALSE") %in% c("0", "FALSE", "false")) {
    options("bbr.DEV_no_min_version" = TRUE)
  }
  # set bbi executable path
  if (is.null(getOption("bbr.bbi_exe_path"))) {
    options("bbr.bbi_exe_path" = BBI_DEFAULT_PATH)
  }

  if (is.null(getOption("bbr.bbi_exe_mode"))) {
    if (identical(check_os(), "linux")) {
      options("bbr.bbi_exe_mode" = "sge")
    } else {
      options("bbr.bbi_exe_mode" = "local")
    }
  }

  # controls printing extra informative messages in functions
  if (is.null(getOption("bbr.verbose"))) {
    options("bbr.verbose" = TRUE)
  }

  # by default turn on strict mode to check for malformed objects and classes, etc.
  if (is.null(getOption("bbr.strict"))) {
    options("bbr.strict" = TRUE)
  }

  # controls printing extra informative messages in functions
  if (is.null(getOption("bbr.gitignore_sim"))) {
    options("bbr.gitignore_sim" = TRUE)
  }
}

.onAttach <- function(libname, pkgname) {
  if (isTRUE(getOption("bbr.DEV_no_min_version"))) {
    packageStartupMessage("setting `bbr.DEV_no_min_version` to TRUE so no validation of bbi version will occur")
  }
}
