# Batch Processing of Parameter Estimates in R

#' Batch Processing of Parameter Estimates
#'
#' Calls out to `bbi nonmem params` and returns a tibble of parameter estimates.
#' The primary reason for this function is for things like bootstraps or
#' simulations where parameter estimates for a large number of models (hundreds
#' or thousands) need to be pulled in together. In those cases, this function is
#' _much_ faster than using [model_summaries()] and [param_estimates()],
#' because it does _not_ parse all of the other information contained in a
#' `bbi_nonmem_summary` object.
#'
#' @details
#'
#' The tibble will always have columns `absolute_model_path`, `run`,
#' `error_msg`, and `termination_code`. All other column names are those of the
#' parameter estimates found in the `.ext` files detected in the directory
#' passed. Note: the passed directory is _not_ searched recursively. Passed
#' directory will be searched for model files and output directories for found
#' models will be searched for `.ext` files.
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
#' @importFrom dplyr rename select mutate everything starts_with across
#' @importFrom data.table fread
#' @importFrom tibble as_tibble
#'
#' @param .path a path to a directory containing model sudirectories for batch parameter processing.
#' @param ... args passed through to [bbi_exec()]
#' @param .dry_run show what the command would be without actually running it
#' @export
param_estimates_batch <- function(.path,
                                  ...,
                                  .dry_run = FALSE) {

  check_bbi_version_constraint(.min_version = "3.1.0", .function = "param_estimates_batch")

  .path <- fs::path_abs(.path)
  if (!dir_exists(.path)) {
    err_msg <-
      glue("param_estimates_batch('{.path}') failed; unable to locate {.path}")
    stop(err_msg, call. = FALSE)
  }

  # path containing .ext file
  ext_file_dir <- basename(.path)

  # for bbi nonmem params, need to pass the directory containing .ext
  # cd to the root directory containing directory with .ext files
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
          "param_estimates_batch('{.path}') failed with the following error. This may be because the modeling run has not finished successfully.\n\nERROR: \n{e}"
        )
      stop(err_msg, call. = FALSE)
    }
  )

  df <- as_tibble(fread(text = res$stdout))

  # throw out extra column that gets created if no models succeeded
  if ("V4" %in% names(df)) df <- select(df, -.data$V4)

  # format and return
  df %>%
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
