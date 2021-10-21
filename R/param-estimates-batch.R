# Batch Processing of Parameter Estimates in R

#' Batch Processing of Parameter Estimates
#'
#' Calls out to `bbi nonmem params` and returns a tibble of parameter estimates.
#' The tibble will always have columns `absolute_model_path` and `error_msg`.
#' All proceeding columns names are those of the parameter estimates found in the .ext files detected in the directory passed
#' Directory passed is searched recursively, i.e. all detected .ext files in subdirectories will be included in the output tibble
#' @details
#'
#' `error_msg`: column will be NA if error is not detected. Column will contain message if known error is detected. The known errors
#' are
#'   - `"no ext file contents"`: .ext file detected, but contains nothing
#'   - `"no estimation output detected"`: .ext file detected, has contents, but is missing line -1000000000 which are the final parameter estimates
#' parameter columns: naming will be the same as names found in .ext file. If .ext files within the directory contain differing parameter names,
#' the output tibble will show all parameter names across files and NA values for the files where a given parameter is missing.
#'
#'
#' @param .path a path to a directory containing model sudirectories for batch parameter processing.
#' @param ... args passed through to [bbi_exec()]
#' @param .dry_run show what the command would be without actually running it
#' @export
param_estimates_batch <- function(.path,
                                  ...,
                                  .dry_run = FALSE) {
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
  if ("X" %in% names(df)) df <- select(df, -X)

  df <- df %>%
    tidyr::as_tibble() %>%
    dplyr::rename(
      'absolute_model_path' = 'dir',
      'error_msg' = 'error',
      'termination_code' = 'termination'
    ) %>%
    mutate(across(everything(), ~ ifelse(. == "", NA, .)),
           absolute_model_path = file.path(.path, absolute_model_path))

  # ordering
  ordered_cols <- c(which(startsWith(names(df), 'SIGMA')),
                    which(startsWith(names(df), 'THETA')),
                    which(startsWith(names(df), 'OMEGA')))

  ordered_cols <- c(ordered_cols,
                    seq(names(df))[!seq(names(df)) %in% ordered_cols])

  df[ordered_cols] %>%
    dplyr::relocate('absolute_model_path',
                    'error_msg',
                    'termination_code')
}
