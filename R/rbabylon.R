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
#' @param .path Full path to model file that will be run
#' @param .cmd_args A character vector of command line arguments for the execution call
#' @return output from executed process, as a list with components status, stdout, stderr, and timeout (see ?processx::run for more details)
#' @importFrom processx run
#' @export
bbi_exec <- function(.path, .cmd_args, ...) {
  # parse model directory and model filename
  model_dir <- dirname(.path)
  model <- basename(.path)
  if (model_dir == ".") {
    # given a path that is just the model name, set directory to getwd()
    model_dir <- getwd()
  }
  output <- processx::run(getOption("rbabylon.bbi_exe_path"), c(.cmd_args, model), wd = model_dir, ..., error_on_status = FALSE)

  # check output status code
  check_status_code(output)

  return(output)
}

#' Executes (`bbi --help`) with processx::run and prints the output string
#' @importFrom processx run
#' @export
bbi_help <- function() {
  output <- processx::run(getOption("rbabylon.bbi_exe_path"), "--help", error_on_status = FALSE)
  # check output status code
  check_status_code(output)
  cat(output$stdout)
}

