# Batch Processing of Parameter Estimates in R

#' Batch Processing of Parameter Estimates
#'
#' Calls out to `bbi nonmem params` and returns a tibble of parameter estimates.
#' The tibble will always have columns `absolute_model_path`, `run`,
#' `error_msg`, and `termination_code`. All other columns names are those of the
#' parameter estimates found in the `.ext` files detected in the directory
#' passed. Note: the passed directory is _not_ searched recursively. Passed
#' directory will be searched for model files and output directories for found
#' models will be searched for `.ext` files.
#'
#' @details
#'
#' `error_msg`: column will be `NA` if error is not detected. Column will contain message if known error is detected. The known errors
#' are
#'   - `"no ext file contents"`: `.ext` file detected, but contains nothing
#'   - `"no estimation output detected"`: `.ext` file detected, has contents, but is missing line `-1000000000` which are the final parameter estimates
#'
#' `termination_code`: parsed from line `-1000000007` of the `.ext`
#'
#' **parameter columns:** naming will be the same as names found in `.ext` file.
#' If `.ext` files within the directory contain differing parameter names, the
#' output tibble will show all parameter names across files and NA values for
#' the files where a given parameter is missing.
#'
#' @importFrom dplyr rename select mutate everything
#' @importFrom tidyr as_tibble
#'
#' @param .path a path to a directory containing model sudirectories for batch parameter processing.
#' @param ... args passed through to [bbi_exec()]
#' @param .dry_run show what the command would be without actually running it
#' @export
param_estimates_batch <- function(.path,
                                  ...,
                                  .dry_run = FALSE) {

  .path <- fs::path_abs(.path)
  if (!dir_exists(.path)) {
    err_msg <-
      glue("batch_parameter_estimates('{.path}') failed; unable to locate {.path}")
    stop(err_msg, call. = FALSE)
  }

  # path containing .ext file
  ext_file_dir <- basename(.path)

  # for bbi nonmem params, need to pass the directory containing .ext
  # cd to the root directory cointaining directory with .ext files
  .path <- dirname(.path)

  cmd_args <- c("nonmem", "params", "--dir", ext_file_dir)

  # if .dry_run return output call
  if (.dry_run) {
    res <- bbi_dry_run(cmd_args, .path)
    return(res)
  }

  res <- tryCatch(
    bbi_exec(cmd_args, .dir = .path, ..., .wait = TRUE),
    error = function(e) {
      err_msg <-
        glue(
          "batch_parameter_estimates('{.path}') failed with the following error. This may be because the modeling run has not finished successfully.\n\nERROR: \n{e}"
        )
      stop(err_msg, call. = FALSE)
    }
  )

  df <- read.csv(text = res$stdout, header = TRUE)

  # if none succeeded, there will be an empty X column instead of params
  if ("X" %in% names(df)) df <- select(df, -.data$X)

  df %>%
    as_tibble() %>%
    rename(
      absolute_model_path = .data$dir,
      error_msg = .data$error,
      termination_code = .data$termination
    ) %>%
    mutate(
      across(everything(), ~ ifelse(. == "", NA, .)),
      absolute_model_path = file.path(.path, .data$absolute_model_path),
      run = basename(.data$absolute_model_path)
    ) %>%
    select(
      .data$absolute_model_path,
      .data$run,
      .data$error_msg,
      .data$termination_code,
      everything()
    )
}
