#' @name rbabylon
#' @title RBabylon
#' @description RBabylon is a library written in R that facilitates modeling and simulation by exposing functionality from the https://github.com/metrumresearchgroup/babylon project in R
#' From Babylon's docs: Babylon is (will be) a complete solution for managing projects involving modeling and simulation with a number of software solutions used in pharmaceutical sciences.
#' This is a fork of the `nonmemutils`` project that is broader in scope.
#' Initial support encompasses NONMEM however the api is designed in a way to be flexible to handle other software.
NULL

#' Executes a babylon call (`bbi ...`) with processx::run
#' @param .cmd_args A character vector of command line arguments for the execution call
#' @param .verbose Print stdout and stderr as process runs
#' @param ... arguments to pass to processx::run
#' @return output from executed process, as a list with components status, stdout, stderr, and timeout (see ?processx::run for more details)
#' @importFrom processx run
#' @export
bbi_exec <- function(.cmd_args, .verbose = FALSE, ...) {
  bbi_exe_path <- getOption("rbabylon.bbi_exe_path")
  check_bbi_exe(bbi_exe_path)
  output <- processx::run(bbi_exe_path, .cmd_args, ..., error_on_status = FALSE)

  # check output status code
  check_status_code(output, .cmd_args)

  return(output)
}


#' Checks that a bbi binary is present at the path passed to .bbi_exe_path
#' @param .bbi_exe_path Path to bbi exe file that will be checked
#' @export
check_bbi_exe <- function(.bbi_exe_path) {
  # check if this path is not in the already checked paths
  if (is.null(CACHE_ENV$bbi_exe_paths[[.bbi_exe_path]])) {
    which_path <- Sys.which(.bbi_exe_path)
    # if missing, reject it
    if (which_path == "") {
      stop(paste0(
            "`", .bbi_exe_path, "`",
            " was not found on system. Please assign a path to a working version of babylon with `options('rbabylon.bbi_exe_path' = '/path/to/bbi')`")
           )
    # if found, add it
    } else {
      CACHE_ENV$bbi_exe_paths[[.bbi_exe_path]] <- TRUE
    }
  }
  invisible()
}


#' Checks status code from processx::run output
#' @param .output named list output from processx::run()
#' @param .cmd_args character vector of args passed to processx::run(). Used for error printing.
#' @importFrom stringr str_detect
#' @export
check_status_code <- function(.output, .cmd_args) {
  if (.output$status != 0) {
    if (str_detect(.output$stderr, NO_NONMEM_ERR_MSG)) {
      cat("No version of NONMEM is specified. Either open the relevant `babylon.yml` and set a version of NONMEM to `default: true`, or pass a version of NONMEM to `.args=list(nm_version='some_version')`")
    }
    err_msg <- paste0(
      "`bbi ", paste(.cmd_args, collapse=" "), "` returned status code ", .output$status,
      " -- STDOUT: ", .output$stdout,
      " -- STDERR: ", .output$stderr
    )
    stop(err_msg)
  }
}


#' Executes (`bbi --help`) with processx::run and prints the output string
#' @param .cmd_args You can optionally pass a vector of args to get help about a specific call
#' @importFrom processx run
#' @export
bbi_help <- function(.cmd_args=NULL) {
  if (is.null(.cmd_args)) {
    .cmd_args <- "--help"
  } else {
    .cmd_args <- c(.cmd_args, "--help")
  }
  output <- bbi_exec(.cmd_args)
  cat(output$stdout)
}


#' Executes (`bbi init`) with processx::run in specified directory
#' @param .dir Path to directory to run `init` in (and put the resulting `babylon.yml` file)
#' @param .nonmem_dir Path to directory with the NONMEM installation.
#' @importFrom processx run
#' @export
bbi_init <- function(.dir, .nonmem_dir) {
  # check for files in NONMEM directory
  nm_files <- list.files(.nonmem_dir)
  if (length(nm_files) == 0) {
    stop(paste0("Nothing was found in ", .nonmem_dir, ". Please pass a path to a working installation of NONMEM to `.nonmem_dir`, for example `.nonmem_dir='/opt/NONMEM'`"))
  }

  # execute init
  output <- bbi_exec(c("init",  paste0("--dir=", .nonmem_dir)), wd = .dir)
  cat(output$stdout)
}


