#' @name rbabylon
#' @title RBabylon
#' @description RBabylon is a library written in R that facilitates modeling and simulation by exposing functionality from the https://github.com/metrumresearchgroup/babylon project in R
#' From Babylon's docs: Babylon is (will be) a complete solution for managing projects involving modeling and simulation with a number of software solutions used in pharmaceutical sciences.
#' This is a fork of the `nonmemutils`` project that is broader in scope.
#' Initial support encompasses NONMEM however the api is designed in a way to be flexible to handle other software.
#' @importFrom glue glue
#' @importFrom rlang .data
NULL

#' Executes a babylon call (`bbi ...`) with processx::process$new()
#' @param .cmd_args A character vector of command line arguments for the execution call
#' @param .verbose Print stdout and stderr as process runs #### NOT IMPLEMENTED?
#' @param .wait If true, don't return until process has exited.
#' @param ... arguments to pass to processx::process$new()
#' @return A named list with process object and some metadata
#'         process -- The process object (see ?processx::process$new for more details on what you can do with this)
#'         output -- the stdout and stderr from the process, if `.wait = TRUE`. If `.wait = FALSE` this will be NULL.
#'         bbi -- character scaler with the execution path used for bbi
#'         cmd_args -- character vector of all command arguments passed to the process
#' @importFrom processx process
#' @export
bbi_exec <- function(.cmd_args, .verbose = FALSE, .wait = FALSE, ...) {
  bbi_exe_path <- getOption("rbabylon.bbi_exe_path")
  check_bbi_exe(bbi_exe_path)

  p <- processx::process$new(bbi_exe_path, .cmd_args, ..., stdout = "|", stderr = "2>&1")

  if (.wait) {
    # wait for process and capture stdout and stderr
    output <- p$read_all_output_lines()

    # check output status code
    check_status_code(p$get_exit_status(), output, .cmd_args)
  } else {
    output <- NULL
  }

  # build result object
  res <- list(
    process = p,
    output = output,
    bbi = bbi_exe_path,
    cmd_args = .cmd_args
  )

  attr(res, "class") <- "babylon_result"
  return(res)
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


#' Checks status code from processx process
#' @param .status_code numerical status code from the process
#' @param .output character scalar of output from the process. Used only for error printing.
#' @param .cmd_args character vector of args passed to process. Used only for error printing.
#' @importFrom stringr str_detect
#' @export
check_status_code <- function(.status_code, .output, .cmd_args) {
  if (.status_code != 0) {
    if (str_detect(.output, NO_NONMEM_ERR_MSG)) {
      cat("No version of NONMEM is specified. Either open the relevant `babylon.yml` and set a version of NONMEM to `default: true`, or pass a version of NONMEM to `.args=list(nm_version='some_version')`")
    }
    err_msg <- paste0(
      "`bbi ", paste(.cmd_args, collapse=" "), "` returned status code ", .status_code,
      " -- STDOUT and STDERR: ", .output
    )
    stop(err_msg)
  }
}


#' Executes (`bbi --help`) with bbi_exec and prints the output string
#' @param .cmd_args You can optionally pass a vector of args to get help about a specific call
#' @export
bbi_help <- function(.cmd_args=NULL) {
  if (is.null(.cmd_args)) {
    .cmd_args <- "--help"
  } else {
    .cmd_args <- c(.cmd_args, "--help")
  }
  res <- bbi_exec(.cmd_args, .wait=TRUE)
  cat(paste(res$output, collapse = "\n"))
}


#' Executes (`bbi init`) with bbi_exec() in specified directory
#' @param .dir Path to directory to run `init` in (and put the resulting `babylon.yml` file)
#' @param .nonmem_dir Path to directory with the NONMEM installation.
#' @export
bbi_init <- function(.dir, .nonmem_dir) {
  # check for files in NONMEM directory
  nm_files <- list.files(.nonmem_dir)
  if (length(nm_files) == 0) {
    stop(paste0("Nothing was found in ", .nonmem_dir, ". Please pass a path to a working installation of NONMEM to `.nonmem_dir`, for example `.nonmem_dir='/opt/NONMEM'`"))
  }

  # execute init
  res <- bbi_exec(c("init",  paste0("--dir=", .nonmem_dir)), wd = .dir, .wait=TRUE)
  cat(paste(res$output, collapse = "\n"))
}


