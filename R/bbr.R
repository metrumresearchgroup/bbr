#' @name bbr
#' @title bbr
#' @description `bbr` is a library written in R that facilitates modeling and
#'   simulation by exposing functionality from the
#'   [bbi](https://github.com/metrumresearchgroup/bbi) project in R.
#'   From `bbi`'s docs: `bbi` is a solution for managing projects involving
#'   modeling and simulation with a number of software solutions used in
#'   pharmaceutical sciences. This is a fork of the `nonmemutils` project that
#'   is broader in scope. Initial support encompasses NONMEM, however the API is
#'   designed in a way to be flexible to handle other software.
#'
#' @details
#' `bbr` uses several fundamental objects, each of which has a number of S3
#' methods associated with it:
#'
#' * `bbi_{.model_type}_model` -- Represents a single model. Contains paths to
#'   any elements of this model that are stored on disk (e.g. a NONMEM control
#'   stream) as well as various fields containing metadata about the model. You
#'   can read more about these fields, and how to interact with them, in the
#'   following help pages: [modify_tags()] [modify_notes()] [modify_based_on()]
#'   [modify_description()] [modify_bbi_args()]
#' * `bbi_{.model_type}_summary` -- Contains information and diagnostics about
#'   a model that has been run. See [model_summary()] for more information about
#'   contents and usage.
#' * `bbi_log_df` -- A tibble containing information about multiple models.
#'   There are several sub-classes of `bbi_log_df` tibbles, variously containing
#'   metadata, information, and diagnostics about the models. See the following
#'   help pages for more information: [run_log()] [summary_log()] [config_log()]
#'
#' @importFrom glue glue
#' @importFrom rlang .data :=
#' @importFrom lifecycle deprecate_warn deprecate_stop
#' @import fs
NULL


#' Execute call to bbi
#'
#' Private implementation function that executes a bbi call (`bbi ...`) with
#' processx::process$new()
#'
#' @inheritParams bbi_version
#' @param .cmd_args A character vector of command line arguments for the execution call
#' @param .dir The working directory to run command in. Defaults to "."
#' @param .verbose Print stdout and stderr as process runs #### NOT IMPLEMENTED?
#' @param .wait If true, don't return until process has exited.
#' @param ... arguments to pass to processx::process$new()
#'
#' @return An S3 object of class `bbi_process` with the following elements
#'
#'   * process -- The process object (see ?processx::process$new for more
#'   details on what you can do with this).
#'
#'   * stdout -- the stdout and stderr from the process, if `.wait = TRUE`. If
#'   `.wait = FALSE` this contain a message with the tempfile path where stdout
#'   and stderr have been redirected.
#'
#'   * bbi -- character scalar with the execution path used for bbi.
#'
#'   * cmd_args -- character vector of all command arguments passed to the
#'   process.
#'
#'   * working_dir -- the directory the command was run in, passed through from
#'   .dir argument.
#'
#' @importFrom processx process
#' @importFrom readr read_lines
#' @keywords internal
bbi_exec <- function(.cmd_args,
                     .dir = ".",
                     .verbose = FALSE,
                     .wait = FALSE,
                     .bbi_exe_path = getOption("bbr.bbi_exe_path"),
                     ...) {
  check_bbi_exe(.bbi_exe_path)

  stdout_file <- tempfile("bbi_exec_out_")

  p <- processx::process$new(
    .bbi_exe_path,
    .cmd_args,
    ...,
    wd = .dir,
    stdout = stdout_file,
    stderr = "2>&1"
  )
  if (.wait) {
    # wait for process and capture stdout and stderr
    if (all(c("nonmem", "summary", "--json") %in% .cmd_args)) {
      # this was originally just a wait, however this caused an infinite
      # hang with model 510 summary in the tests as of 0.7.0.8009 on mac
      # it _could_ be due to processes finishing so fast that ps/processx
      # are not able to adequately track the status of the subprocess while
      # waiting.
      # This was attempted to be artifically delayed by making the ext 50k lines
      # but this still resulted in the hang so not positive. Have not seen
      # this error in other models.
      # To get around this false hang we want to timeout.
      # This should be ok as the json should
      # be back at that point, in which p$read_all_output_lines will return
      # valid values.
      #
      # in addition to give a debug capability, setting the timeout to
      # an option default of 3. generally summary should return in 10 ms
      # or so, however if we move towards providing batch summaries
      # this process could take longer. This will give us an escape hatch
      # in such a case that people report errors where summary takes longer
      # then we can establish a longer potential timeout or more heuristics
      # around what time might be (eg if see its a summary on a range of models)
      p$wait(timeout = getOption("bbr.summary_wait_timeout", 1000L))
    } else {
      p$wait()
    }
    # check output status code
    output <- read_lines(stdout_file)
    check_status_code(p$get_exit_status(), output, .cmd_args)

  } else {
    output <- paste("NO STDOUT BECAUSE `.wait = FALSE`. stdout and stderr redirected to", stdout_file)
  }
  # build result object
  res <- list()
  res[[PROC_PROCESS]] <- p
  res[[PROC_STDOUT]] <- output
  res[[PROC_BBI]] <- .bbi_exe_path
  res[[PROC_CMD_ARGS]] <- .cmd_args
  res[[PROC_WD]] <- .dir

  # assign class and return
  res <- create_process_object(res)
  return(res)
}

#' Creates a `bbi_process` object with all the required keys, without actually running the command
#'
#' Returns an S3 object of class `bbi_process` but the `process` and `stdout` elements containing only the string "DRY_RUN".
#' Also contains the element `call` with a string representing the command that could be called on the command line.
#' @param .cmd_args A character vector of command line arguments for the execution call
#' @param .dir The working directory to run command in. Defaults to "."
#' @keywords internal
bbi_dry_run <- function(.cmd_args, .dir) {
  # build result object
  res <- list()
  res[[PROC_PROCESS]] <- "DRY_RUN"
  res[[PROC_STDOUT]] <- "DRY_RUN"
  res[[PROC_BBI]] <- getOption("bbr.bbi_exe_path")
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

#' Check that bbi is installed
#'
#' Raises an error if a bbi executable is not found at `.bbi_exe_path`.
#'
#' @inheritParams bbi_version
#' @return `NULL`, invisibly
#' @keywords internal
check_bbi_exe <- function(.bbi_exe_path) {
  # check if this path is not in the already checked paths
  if (is.null(CACHE_ENV$bbi_exe_paths[[.bbi_exe_path]])) {
    which_path <- Sys.which(.bbi_exe_path)

    # if missing, reject it
    if (which_path == "") {
      stop(glue("`{.bbi_exe_path}` was not found on system. Please assign a path to a working version of bbi with `options('bbr.bbi_exe_path' = '/path/to/bbi')`"))
    }

    # if version too low, reject it
    check_bbi_version_constraint(.bbi_exe_path)

    # if found, and passes version constraint, add it to cache
    CACHE_ENV$bbi_exe_paths[[.bbi_exe_path]] <- TRUE
  }
  return(invisible())
}


#' Check that bbi satisfies a version constraint
#'
#' @importFrom stringr str_replace_all
#' @inheritParams bbi_version
#' @return `NULL` if `.bbi_exe_path` satisfies the constraint
#' @keywords internal
check_bbi_version_constraint <- function(.bbi_exe_path = getOption('rbabylon.bbi_exe_path')) {
  if (isTRUE(getOption("rbabylon.DEV_no_min_version"))) {
    return(invisible(TRUE))
  }
  .bbi_exe_path <- Sys.which(.bbi_exe_path)
  if (.bbi_exe_path == "") {
    stop(glue("`{.bbi_exe_path}` was not found on the system."))
  }

  this_version <- bbi_version(.bbi_exe_path)
  test_version_test <- package_version(str_replace_all(this_version, "[^0-9\\.]", ""))

  if (test_version_test < getOption("bbr.bbi_min_version")) {
    strict_mode_error(paste(
      glue("The executable at `{.bbi_exe_path}` is version {this_version} but the minimum supported version of bbi is {getOption('bbr.bbi_min_version')}"),
      glue("Call `use_bbi('{dirname(.bbi_exe_path)}')` to update to the most recent release."),
      sep = "\n"
    ))
  }
}


#' Checks status code from processx process
#' @param .status_code numerical status code from the process
#' @param .output character scalar of output from the process. Used only for error printing.
#' @param .cmd_args character vector of args passed to process. Used only for error printing.
#' @importFrom stringr str_detect
#' @export
check_status_code <- function(.status_code, .output, .cmd_args) {
  # consolidate output to a scalar
  .output <- paste(.output, collapse = "\n")

  .custom_msg <- ""
  if (.status_code != 0) {
    if (str_detect(.output, NO_NONMEM_ERR_MSG)) {
      .custom_msg <- "No version of NONMEM is specified. Either open the relevant `bbi.yaml` and set a version of NONMEM to `default: true`, or pass a version of NONMEM to `.bbi_args=list(nm_version='some_version')`"
    }
    if (str_detect(.output, MOD_ALREADY_EXISTS_ERR_MSG)) {
      .custom_msg <- "The target output directory already exists. Please pass `.bbi_args=list(overwrite=TRUE)` to your `submit_model()` call. You can also set `overwrite: true` in the model .yaml file or the `bbi.yaml` file."
    }

    err_msg <- paste0(
      .custom_msg, "\n\n",
      "`bbi ", paste(.cmd_args, collapse=" "), "` returned status code ", .status_code,
      " -- STDOUT and STDERR: \n", .output
    )
    stop(err_msg)
  }
}


#' Executes (`bbi --help`) and prints the output string
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


#' Initialize bbi
#'
#' Executes `bbi init ...` in specified directory. This creates a `bbi.yaml`
#' file, which contains defaults for many configurable `bbi` settings, in
#' that directory.
#'
#' @details For `bbr` to make any calls out to `bbi` (for example in
#'   [submit_model()] or [model_summary()]) it must find a `bbi.yaml` file.
#'   The default behavior is to look for this file in the same directory as the
#'   model. `submit_model()` and `submit_models()` also support passing a
#'   configuration file via the `.config_path` argument.
#'
#' @param .dir Path to directory to run `init` in (and put the resulting
#'   `bbi.yaml` file)
#' @param .nonmem_dir Path to directory with the NONMEM installation.
#' @param .nonmem_version Character scalar for default version of NONMEM to use.
#'   If left NULL, function will exit and tell you which versions were found in
#'   `.nonmem_dir`
#' @param .no_default_version If `TRUE`, force creation of bbi.yaml with
#'   **no default NONMEM version**. `FALSE` by default, and using `TRUE` is
#'   *not* encouraged.
#' @importFrom yaml read_yaml write_yaml
#' @importFrom fs dir_exists
#' @export
bbi_init <- function(.dir, .nonmem_dir, .nonmem_version = NULL, .no_default_version = FALSE) {
  # check that destination directory exists
  if (!fs::dir_exists(.dir)) {
    stop(paste(
      glue("Cannot find {file.path(getwd(), .dir)}"),
      "Make sure you are in the correct working directory and have passed the correct path to bbi_init(.dir)",
      sep = "\n"
    ), call. = FALSE)
  }

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
    # load bbi.yaml
    bbi_yaml_path <- file.path(.dir, "bbi.yaml")
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
