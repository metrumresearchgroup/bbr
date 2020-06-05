#' @name rbabylon
#' @title RBabylon
#' @description RBabylon is a library written in R that facilitates modeling and simulation by exposing functionality from the https://github.com/metrumresearchgroup/babylon project in R
#' From Babylon's docs: Babylon is (will be) a complete solution for managing projects involving modeling and simulation with a number of software solutions used in pharmaceutical sciences.
#' This is a fork of the `nonmemutils`` project that is broader in scope.
#' Initial support encompasses NONMEM however the api is designed in a way to be flexible to handle other software.
#' @importFrom glue glue
#' @importFrom rlang .data :=
#' @import fs
NULL


#' Executes a babylon call (`bbi ...`) with processx::process$new()
#' @param .cmd_args A character vector of command line arguments for the execution call
#' @param .dir The working directory to run command in. Defaults to "."
#' @param .verbose Print stdout and stderr as process runs #### NOT IMPLEMENTED?
#' @param .wait If true, don't return until process has exited.
#' @param ... arguments to pass to processx::process$new()
#' @return An S3 object of class `babylon_process`
#'         process -- The process object (see ?processx::process$new for more details on what you can do with this).
#'         stdout -- the stdout and stderr from the process, if `.wait = TRUE`. If `.wait = FALSE` this will be NULL.
#'         bbi -- character scaler with the execution path used for bbi.
#'         cmd_args -- character vector of all command arguments passed to the process.
#'         working_dir -- the directory the command was run in, passed through from .dir argument.
#' @importFrom processx process
#' @export
bbi_exec <- function(.cmd_args, .dir = ".", .verbose = FALSE, .wait = FALSE, ...) {
  bbi_exe_path <- getOption("rbabylon.bbi_exe_path")
  check_bbi_exe(bbi_exe_path)

  p <- processx::process$new(bbi_exe_path, .cmd_args, ..., wd = .dir,  stdout = "|", stderr = "2>&1")

  if (.wait) {
    # wait for process and capture stdout and stderr
    p$wait()
    # check output status code
    output <- p$read_all_output_lines()
    check_status_code(p$get_exit_status(), output, .cmd_args)

  } else {
    output <- "NO STDOUT BECAUSE `.wait = FALSE`"
  }

  # build result object
  res <- list()
  res[[PROC_PROCESS]] <- p
  res[[PROC_STDOUT]] <- output
  res[[PROC_BBI]] <- bbi_exe_path
  res[[PROC_CMD_ARGS]] <- .cmd_args
  res[[PROC_WD]] <- .dir

  # assign class and return
  res <- create_process_object(res)
  return(res)
}

#' Babylon dry_run
#'
#' Creates a `babylon_process` object with all the required keys, without actually running the command.
#' Returns an S3 object of class `babylon_process` but the `process` and `stdout` elements containing only the string "DRY_RUN".
#' Also contains the element `call` with a string representing the command that could be called on the command line.
#' @param .cmd_args A character vector of command line arguments for the execution call
#' @param .dir The working directory to run command in. Defaults to "."
#' @export
bbi_dry_run <- function(.cmd_args, .dir) {
  # build result object
  res <- list()
  res[[PROC_PROCESS]] <- "DRY_RUN"
  res[[PROC_STDOUT]] <- "DRY_RUN"
  res[[PROC_BBI]] <- getOption("rbabylon.bbi_exe_path")
  res[[PROC_CMD_ARGS]] <- .cmd_args
  res[[PROC_WD]] <- .dir

  # construct call string that _could_ be called on command line
  call_str <- paste(
    "cd", .dir, ";",
    res[[PROC_BBI]],
    paste(.cmd_args, collapse = " ")
  )
  res[[PROC_CALL]] <- call_str

  # assign class and return
  res <- create_process_object(res)
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
  return(invisible())
}


#' Checks status code from processx process
#' @param .status_code numerical status code from the process
#' @param .output character scalar of output from the process. Used only for error printing.
#' @param .cmd_args character vector of args passed to process. Used only for error printing.
#' @importFrom stringr str_detect
#' @export
check_status_code <- function(.status_code, .output, .cmd_args) {
  # consolidate output to a scaler
  .output <- paste(.output, collapse = "\n")

  .custom_msg <- ""
  if (.status_code != 0) {
    if (str_detect(.output, NO_NONMEM_ERR_MSG)) {
      .custom_msg <- "No version of NONMEM is specified. Either open the relevant `babylon.yml` and set a version of NONMEM to `default: true`, or pass a version of NONMEM to `.bbi_args=list(nm_version='some_version')`"
    }
    if (str_detect(.output, MOD_ALREADY_EXISTS_ERR_MSG)) {
      .custom_msg <- "The target output directory already exists. Please pass `.bbi_args=list(overwrite=TRUE)` to your `submit_model()` call. You can also set `overwrite: true` in the model .yaml file or the babylon.yaml file."
    }

    err_msg <- paste0(
      .custom_msg, "\n\n",
      "`bbi ", paste(.cmd_args, collapse=" "), "` returned status code ", .status_code,
      " -- STDOUT and STDERR: \n", .output
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
#' @param .nonmem_version Character scaler for default version of NONMEM to use. If left NULL, function will exit and tell you which versions were found in `.nonmem_dir`
#' @param .no_default_version Boolean to force creation of babylon.yaml with NO default NONMEM version. FALSE by default, and not encouraged.
#' @importFrom yaml read_yaml write_yaml
#' @export
bbi_init <- function(.dir, .nonmem_dir, .nonmem_version = NULL, .no_default_version = FALSE) {
  # check for files in NONMEM directory
  nm_files <- list.files(.nonmem_dir)
  if (length(nm_files) == 0) {
    stop(glue("Nothing was found in {.nonmem_dir}. Please pass a path to a working installation of NONMEM to `.nonmem_dir`, for example `.nonmem_dir='/opt/NONMEM'`"))
  } else if (is.null(.nonmem_version) && !.no_default_version) {
    stop(glue("Must specify a `.nonmem_version` for bbi_init(). {.nonmem_dir} contains the following options: `{paste(nm_files, collapse='`, `')}`"))
  }

  # execute init
  res <- bbi_exec(c("init",  paste0("--dir=", .nonmem_dir)), .dir = .dir, .wait=TRUE)

  # set default NONMEM version
  if (!is.null(.nonmem_version)) {
    # load babylon.yaml
    bbi_yaml_path <- file.path(.dir, "babylon.yaml")
    bbi_yaml <- read_yaml(bbi_yaml_path)

    # check for valid version
    if (!(.nonmem_version %in% names(bbi_yaml$nonmem))) {
      stop(glue("Must specify a valid `.nonmem_version` for bbi_init(). {bbi_yaml_path} contains the following options: `{paste(names(bbi_yaml$nonmem), collapse='`, `')}`"))
    }

    # set default version and write to disk
    bbi_yaml[['nonmem']][[.nonmem_version]]['default'] <- TRUE
    write_yaml(bbi_yaml, bbi_yaml_path)
  }

  cat(paste(res$output, collapse = "\n"))
}
