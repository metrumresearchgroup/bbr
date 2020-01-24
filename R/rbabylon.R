#globalVariables(c("board", "column", "created_at", "issue", "merged_at", "project", "title"))

#' @name rbabylon
#' @title RBabylon
#' @description RBabylon is a library written in R that facilitates modeling and simulation by exposing functionality from the https://github.com/metrumresearchgroup/babylon project in R
#' From Babylon's docs: Babylon is (will be) a complete solution for managing projects involving modeling and simulation with a number of software solutions used in pharmaceutical sciences.
#' This is a fork of the `nonmemutils`` project that is broader in scope.
#' Initial support encompasses NONMEM however the api is designed in a way to be flexible to handle other software.
## #' @param org Name of organization to query
## #' @param repo Name of repository to query
## #' @param .api_url Optional API url to query. Defaults to "https://api.github.com/graphql"
## #' @param pagination_limit Upper limit on number of rows to return. Function will paginate through results as long as this limit is not exceeded. Generally defaults to NULL for no limit.
NULL

#' Executes a babylon call (`bbi ...`) with processx::run
#' @param .cmd_args A character vector of command line arguments for the execution call
#' @param .verbose Print stdout and stderr as process runs
#' @return output from executed process, as a list with components status, stdout, stderr, and timeout (see ?processx::run for more details)
#' @importFrom processx run
#' @export
bbi_exec <- function(.cmd_args, .verbose = FALSE, ...) {

  output <- processx::run(getOption("rbabylon.bbi_exe_path"), .cmd_args, ..., error_on_status = FALSE)

  # check output status code
  check_status_code(output, .cmd_args)

  return(output)
}


#' Checks status code from processx::run output
#' @param .model_type Type of model to summarize. Currently only supports "nonmem"
#' @export
check_status_code <- function(.output, .cmd_args) {
  if (.output$status != 0) {
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
#' @param .nonmem_dir Path to directory with the NONMEM installation. Defaults to `getOption("rbabylon.nonmem_dir")`
#' @importFrom processx run
#' @export
bbi_init <- function(.dir, .nonmem_dir=NULL) {
  # define NONMEM directory
  if (is.null(.nonmem_dir)) {
    .nonmem_dir <- getOption("rbabylon.nonmem_dir")
  }
  nonmem_arg <- paste0("--dir=", .nonmem_dir)

  # execute init
  output <- bbi_exec(c("init", nonmem_arg), wd = .dir)
  cat(output$stdout)
}


