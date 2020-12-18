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
#'  * **absolute_model_path** -- Absolute path to the model that generated this summary.
#'    Note, using this directly is discouraged in favor of using the "getters"
#'    described in `?get_path_from_object`.
#'  * **run_details** -- General details about the run including estimation
#'    method, numbers of patients and records, significant digits, run time, and
#'    more.
#'  * **run_heuristics** -- Boolean flags indicating potential issues with the
#'    run. For example, if there was a large condition number or a parameter near
#'    boundary. `TRUE` for a given element indicates that issue was detected.
#'    `FALSE` can either mean it was not detected or it was not applicable for
#'    that run.
#'  * **parameters_data** -- Data about parameter estimates. This can be
#'    accessed directly, but is much easier to look at with the
#'    [param_estimates()] function (which parses this, and other related elements,
#'    into a tibble).
#'  * **parameter_names** -- Names of parameters (`THETA1`, `THETA2`, etc.).
#'    Parsed into tibble by [param_estimates()].
#'  * **ofv** -- Objective function value, with and without constant. List with
#'    one element for each estimation method.
#'  * **condition_number** -- Condition number, if eigenvalues were returned
#'    from NONMEM. List with one element for each estimation method.
#'  * **shrinkage_details** -- Information about shrinkage. The shrinkage values
#'    (using SD parameterization) are parsed into the tibble output from
#'    [param_estimates()]. List of lists with one element for each estimation
#'    method, and one element per sub-population within that.
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
#' @seealso [model_summaries()]
#' @param .mod Model to summarize.
#' @param .bbi_args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`.
#' See [print_bbi_args()] for full list of options.
#' @param ... args passed through to [bbi_exec()]
#' @param .dry_run show what the command would be without actually running it
#' @export
model_summary <- function(
  .mod,
  .bbi_args = NULL,
  ...,
  .dry_run = FALSE
) {
  UseMethod("model_summary")
}

#' @describeIn model_summary Get model summary from `bbi_nonmem_model` object
#' @export
model_summary.bbi_nonmem_model <- function(
  .mod,
  .bbi_args = NULL,
  ...,
  .dry_run = FALSE
) {

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
  .path <- get_output_dir(.mod)

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

  res_list <- list()
  res_list[[ABS_MOD_PATH]] <- tools::file_path_sans_ext(get_model_path(.mod))

  bbi_list <- res$stdout %>%
    paste(collapse="") %>%
    jsonlite::fromJSON(simplifyDataFrame = FALSE)

  res_list <- combine_list_objects(res_list, bbi_list)

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

