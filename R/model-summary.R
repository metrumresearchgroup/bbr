# Parsing model summary outputs into R

################################################
# s3 dispatches to parse NONMEM output to list
################################################

#' Summarize model outputs
#'
#' Calls out to bbi and returns a named list of class `bbi_{.model_type}_summary` with model outputs and diagnostics.
#'
#' @details
#' **NONMEM**
#'
#' The returned list for a NONMEM model will contain the following top-level elements:
#'  * run_details
#'  * run_heuristics
#'  * parameters_data
#'  * parameter_names
#'  * ofv
#'  * condition_number
#'  * shrinkage_details
#'
#' The summary call will error if it does not find certain files in the output folder.
#' However, you can override this behavior with the following file-specific flags:
#'  * `no_ext_file`
#'  * `no_grd_file`
#'  * `no_shk_file`
#'
#' If you are using an estimation method that does not produce any of the following files,
#' or they are missing for some other legitimate reason, pass the appropriate flags through the `.bbi_args` argument.
#' For example, if have asked to skip the `$COV` step, you would call `model_summary(..., .bbi_args = list(no_cov_file = TRUE))`.
#'
#' Additionally, if you have renamed the `.ext` file from its default of `<root>.ext` you will need to pass
#' `ext_file = "NEWNAME"` to `.bbi_args`.
#'
#' @param .mod Model to summarize.
#' @param .bbi_args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`.
#' See [print_bbi_args()] for full list of options.
#' @param ... args passed through to `bbi_exec()`
#' @param .dry_run show what the command would be without actually running it
#' @param .directory Model directory which `.mod` path is relative to. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`. **Only used when passing a path for `.mod` instead of a `bbi_{.model_type}_model` object.**
#' @export
model_summary <- function(
  .mod,
  .bbi_args = NULL,
  ...,
  .dry_run = FALSE,
  .directory = NULL
) {
  UseMethod("model_summary")
}

#' @describeIn model_summary Get model summary from `bbi_nonmem_model` object
#' @export
model_summary.bbi_nonmem_model <- function(
  .mod,
  .bbi_args = NULL,
  ...,
  .dry_run = FALSE,
  .directory = NULL
) {

  if (!is.null(.directory)) {
    warning(paste(glue("Passed `.directory = {.directory}` to model_summary.bbi_nonmem_model().") ,
                  "This argument is only valid when passing a path to `model_summary()`.",
                  "`bbi_nonmem_model` object was passed, so `.directory` inferred from `.mod${WORKING_DIR}`"))
  }

  res_list <- nonmem_summary(
    .mod = .mod,
    .bbi_args = .bbi_args,
    ...,
    .dry_run = .dry_run
  )
  return(res_list)
}


###################################
# PRIVATE IMPLEMENTATION FUNCTIONS
###################################

#' Run `bbi nonmem summary` and parse the output to a list
#'
#' Private implementation function called by `model_summary()` dispatches.
#' @param .mod `bbi_nonmem_model` object for summary
#' @param .bbi_args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`. Run [print_bbi_args()] to see valid arguments.
#' @return List of S3 Class "bbi_nonmem_summary" with all summary information
#' @keywords internal
nonmem_summary <- function(
  .mod,
  .bbi_args = NULL,
  ...,
  .dry_run = FALSE
) {

  # check against YAML
  check_yaml_in_sync(.mod)

  # extract output path
  .path <- file.path(.mod[[WORKING_DIR]], .mod[[YAML_OUT_DIR]])

  # lst file can both be the check for whether the dir is a nonmem output dir
  # and also the output always should be runname.lst so we can determine the model name
  # this is definitely better checking for .mod as there are temporary files with that extension
  # as well
  lst_file_path <- check_lst_file(.path)

  # build command line args
  if (is.null(.bbi_args)) {
    .bbi_args <- list()
  }
  .bbi_args <- purrr::list_modify(.bbi_args, json = TRUE)

  args_vec <- check_bbi_args(.bbi_args)
  cmd_args <- c("nonmem", "summary", get_model_id(lst_file_path), args_vec)

  # if .dry_run return output call
  if (.dry_run) {
    res <- bbi_dry_run(cmd_args, .path)
    return(res)
  }

  # otherwise, execute
  res <- tryCatch(
    bbi_exec(cmd_args, .dir = .path, ..., .wait = TRUE),
    error = function(e) {
      err_msg <- glue("nonmem_summary('{.path}') failed with the following error. This may be because the modeling run has not finished successfully.\n\nERROR: \n{e}")
      stop(err_msg, call. = FALSE)
    }
  )

  res_list <- res$stdout %>%
    paste(collapse="") %>%
    jsonlite::fromJSON(simplifyDataFrame = FALSE)

  res_list <- create_summary_object(res_list)

  return(res_list)
}

#' Private helper function to look for .lst function in a directory
#' @param .x The directory path to look in for the lst file
#' @keywords internal
check_lst_file <- function(.x) {
  lst_file <- fs::dir_ls(.x, type = "file", glob = "*.lst")
  if (!length(lst_file)) {
    stop(glue("Unable to locate `.lst` file in dir: {.x}. Check to be sure this is a NONMEM output folder, and that the run has finished successfully."))
  }
  lst_file
}

