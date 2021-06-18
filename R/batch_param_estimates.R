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
batch_param_estimates <- function(
  .path,
  ...,
  .dry_run = FALSE
){

  if(.dry_run){
    print('dry')
  }

  if(!dir_exists(.path)){
    err_msg <- glue("batch_parameter_estimates('{.path}') failed; unable to locate {.path}")
    stop(err_msg, call. = FALSE)
  }

  # extract all .ext files found recursively in path
  ext_files_path <- check_batch_ext_file(.path)

  purrr::map_dfr(ext_files_path, function(.x){
    get_fast_params(.x, ..., .dry_run = .dry_run)
  }) %>%
    as_tibble()

}


###################################
# PRIVATE IMPLEMENTATION FUNCTIONS
###################################


#' Private helper function to extract param estimates from .ext file in directory
#' @param .x The full path to the ext file
#' @keywords internal
get_fast_params <- function(
  .x,
  ...,
  .dry_run = FALSE
){

  # for bbi nonmem params, need to pass the directory containing .ext
  # cd to the root directory cointaining directory with .ext
  .path <- dirname(dirname(.x))

  # path containing .ext file
  ext_file_dir <- basename(dirname(.x))

  cmd_args <- c("nonmem", "params", ext_file_dir)

  # if .dry_run return output call
  if (.dry_run) {
    res <- bbi_dry_run(cmd_args, .path)
    return(purrr::map(res, paste, collapse = ';'))
  }


  res <- tryCatch(
    bbi_exec(cmd_args, .dir = .path, ..., .wait = TRUE),
    error = function(e) {
      known_errors <- c('no ext file contents',
                        "no estimation output detected")
      error_present <- purrr::map_lgl(known_errors, ~grepl(.x, e$message))
      if(sum(error_present) == 1){
        msg <- known_errors[error_present]
        list(
          stdout = c("dir,error_msg", glue("{.path},{msg}"))
        )
      } else {
        err_msg <- glue("batch_parameter_estimates('{.path}') failed with the following error. This may be because the modeling run has not finished successfully.\n\nERROR: \n{e}")
        stop(err_msg, call. = FALSE)
      }
    }
  )

  tbl <- data.table::fread(text = res$stdout) %>%
    dplyr::rename('absolute_model_path' = 'dir')

  tbl[['absolute_model_path']] <- tools::file_path_sans_ext(get_model_path(.mod))

  if(!'error_msg' %in% names(tbl)){
    tbl[['error_msg']] <- NA
  }

  tbl <- tbl %>% dplyr::relocate('error_msg', .after = 'absolute_model_path')

  return(tbl)
}


#' Private helper function to look for .ext function in a directory
#' @param .x The directory path to look in for the ext file
#' @keywords internal
check_batch_ext_file <- function(.x) {
  ext_file <- fs::dir_ls(.x, type = "file", glob = "*.ext", recurse = TRUE)
  if (length(ext_file) == 0) {
    stop(glue("Unable to locate any `.ext` files in dir: {.x}. Check to be sure this is a NONMEM output folder, and that the run has finished successfully."))
  }
  ext_file
}
