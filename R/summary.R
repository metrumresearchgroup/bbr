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
#'  * shrinkage_details
#'  * covariance_theta
#'  * correlation_theta
#'
#' The summary call will error if it does not find certain files in the output folder.
#' However, you can override this behavior with the following file-specific flags:
#'  * `no_cor_file`
#'  * `no_cov_file`
#'  * `no_ext_file`
#'  * `no_grd_file`
#'  * `no_shk_file`
#'
#' If you are using an estimation method that does not produce any of the following files,
#' or they are missing for some other legitimate reason, pass the appropriate flags through the `.bbi_args` argument.
#' For example, if have asked to skip the `$COV` step, you would call `model_summary(..., .bbi_args = list(no_cov_file = TRUE))`.
#'
#' @param .mod Model to summarize. Can be a `bbi_{.model_type}_model` object, a file path, or an integer corresponding to a file path.
#' @param .model_type String specifying the type of model, either 'nonmem' or 'stan'.  **Only used when passing a path for `.mod` instead of a `bbi_{.model_type}_model` object.**
#' @param .bbi_args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`.
#' @param ... args passed through to `bbi_exec()`
#' @param .dry_run show what the command would be without actually running it
#' @param .directory Model directory which `.mod` path is relative to. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`. **Only used when passing a path for `.mod` instead of a `bbi_{.model_type}_model` object.**
#' @export
model_summary <- function(
  .mod,
  .model_type = NULL,
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
  .model_type = NULL,
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

  if (!is.null(.model_type)) {
    warning(paste(glue("Passed `.model_type = {.model_type}` to model_summary.bbi_nonmem_model().") ,
                  "This argument is only valid when passing a path to `model_summary()`.",
                  "`bbi_nonmem_model` object was passed, so `.model_type` set to `nonmem`"))
  }

  res_list <- nonmem_summary(
    .mod = .mod,
    .bbi_args = .bbi_args,
    ...,
    .dry_run = .dry_run
  )
  return(res_list)
}

#' @describeIn model_summary Get model summary from output directory path
#' @export
model_summary.character <- function(
  .mod,
  .model_type = c("nonmem", "stan"),
  .bbi_args = NULL,
  ...,
  .dry_run = FALSE,
  .directory = get_model_directory()
) {

  # check for .directory and combine with .mod
  .mod <- combine_directory_path(.directory, .mod)

  # check model type
  .model_type <- match.arg(.model_type)
  if (.model_type == "nonmem") {
    .mod <- read_model(.mod)
    res_list <- nonmem_summary(
      .mod = .mod,
      .bbi_args = .bbi_args,
      ...,
      .dry_run = .dry_run
    )
  } else if (.model_type == "stan") {
    stop(NO_STAN_ERR_MSG)
  } else {
    stop(glue("Passed `{.model_type}`. Valid options: `{paste(SUPPORTED_MOD_TYPES, collapse = ', ')}`"))
  }
  return(res_list)
}


#' @describeIn model_summary Get model summary from numeric input.
#' This will only work if you are calling from the same directory as the models, or if you have set `options('rbabylon.model_directory')` to the directory constaining the relevant model.
#' @export
model_summary.numeric <- function(
  .mod,
  .model_type = c("nonmem", "stan"),
  .bbi_args = NULL,
  ...,
  .dry_run = FALSE,
  .directory = get_model_directory()
) {

  # convert to character
  .mod <- as.character(.mod)

  # call character dispatch
  res_list <- model_summary(
    .mod = .mod,
    .model_type = .model_type,
    .bbi_args = .bbi_args,
    ...,
    .dry_run = .dry_run,
    .directory = .directory
  )

  return(res_list)
}


#' Run `bbi nonmem summary` and parse the output to a list
#'
#' Private implementation function called by `model_summary()` dispatches.
#' @param .mod `bbi_nonmem_model` object for summary
#' @param .bbi_args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`. Run `print_nonmem_args()` to see valid arguments.
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

  args_vec <- check_nonmem_args(.bbi_args)
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


