# Parsing model summary outputs into R

################################################
# s3 dispatches to parse NONMEM output to list
################################################

#' Summarize model outputs
#'
#' Calls out to `bbi` and returns a named list of class
#' `bbi_{.model_type}_summary` with model outputs and diagnostics. If user wants
#' to summarize multiple models, you can pass a list of
#' `bbi_{.model_type}_model` objects to [model_summaries()], or pass a directory
#' path to [summary_log()] to summarize all models in that directory (and, by
#' default, all directories below it). Note: the summary object does _not_
#' contain the covariance or correlation matrices, but these can be retrieved
#' with [cov_cor()]. Also, if you need to pull in _only_ the parameter estimates
#' for a large number of NONMEM models, consider using
#' [param_estimates_batch()].
#'
#' @details
#'
#' **NONMEM**
#'
#' The returned list for a NONMEM model will contain the following top-level elements:
#'  * **absolute_model_path** -- Absolute path to the model that generated this summary.
#'    Note, using this directly is discouraged in favor of using the "getters"
#'    described in the [`get_path_from_object`] help page.
#'  * **run_details** -- General details about the run including estimation
#'    method, numbers of subjects and records, significant digits, run time, and
#'    more.
#'  * **run_heuristics** -- Boolean flags indicating potential issues with the
#'    run. `TRUE` for a given element indicates that issue was detected. `FALSE`
#'    can either mean it was not detected or it was not applicable for that run.
#'    If any of these come up `TRUE` it is best to investigate the issue, likely
#'    by checking the `.lst` file. This can be done easily with
#'    [build_path_from_model()] like `build_path_from_model(.mod, ".lst")`. See
#'    below for details on each heuristic.
#'    * `covariance_step_aborted` -- It is possible for the Estimation Step to
#'      terminate successfully, but have the Covariance Step generate an error
#'      message. In general, successful completion of the Covariance Step
#'      requires a better defined minimum than does the successful completion of
#'      the Estimation Step. It is not necessarily an error but a warning, often
#'      due to the model being overparameterized. Look at the variance
#'      components of your model parameters (look for very small values e.g.
#'      10^-6) and consider removing some of them to simplify the model.
#'    * `large_condition_number` -- `TRUE` if `condition_number > 1000`. Could
#'      indicate highly correlated parameters, overparameterization, or other
#'      issues. The condition number is the ratio of the largest eigenvalue to
#'      the smallest eigenvalue. If there is only one eigenvalue, condition
#'      number will be `1.0`.
#'    * `correlations_not_ok` -- Not currently implemented in `bbi`; will always
#'      be `FALSE`.
#'    * `parameter_near_boundary` -- At least one scaled parameter estimate is
#'      near the boundary of what was specified. This message indicates that one
#'      of your parameters is either too small or too large, which sometimes may
#'      be dealt with by changing your initial estimates. The boundary test is
#'      user-selectable.
#'    * `hessian_reset` -- Indicates some problem during the search,
#'      specifically that the approximation to the Hessian is no longer
#'      acceptable, and must be “recomputed”. User should check the `.lst` file
#'      to see how many resets occur and on which iterations. This message can
#'      be safely ignored _unless_ more than just a couple of resets occur, or
#'      if resets occur right before the end of the search. In these cases a
#'      poor fit (judged by other means) can result and the user should
#'      carefully regard their setup and consider possible reasons why a
#'      minimum--let alone one which corresponds to a good estimate--was
#'      difficult to find. There could be many possible reasons. The appearance
#'      of the message can also explain why an unusually large number of
#'      function evaluations were used for the iteration (extra ones were needed
#'      to compute the new Hessian and perform a line search along the new
#'      direction) and why an unusually long CPU time was needed for the
#'      iteration (if intermediate output is being monitored). Its appearance
#'      suggests that the search is not going easily, and perhaps (thought not
#'      necessarily) that something is wrong with the model specification.
#'    * `has_final_zero_gradient` -- Could indicate a coding error, or that
#'      there is not enough information for NONMEM to estimate the parameter.
#'      Use [check_grd()] to see which parameters are having issues. A gradient
#'      of 0 at the start of the search indicates that there is a lack of
#'      information for NONMEM to use in order to determine the best estimate
#'      for that parameter. Zero gradient at the beginning of the iteration
#'      process could indicate lack of information for that parameter (e.g.
#'      adding gender effect and having a dataset with only males). The EM
#'      algorithms report if a gradient for one of the THETAs is zero,
#'      indicating improper model specification or coding.
#'    * `minimization_terminated` -- The minimization was terminated. This may also
#'      involve considering posteriori non-identifiability because the data does not
#'      really let you estimate some parameter, for example between subject
#'      variability. In this case, consider simplifying the _between subject variability_ model.
#'      If minimization was terminated due to rounding errors (`error=134`) this
#'      message indicates that a sufficient number of digits were lost during
#'      estimation resulting in termination of the optimization. One solution could
#'      be to rerun the model with a new set of initial estimates and changing
#'      `SIGDIGITS` to a higher number. Also consider if the model is
#'      overparameterized.
#'    * `eta_pval_significant` -- `TRUE` if any of the ETA p-values are < 0.05
#'      (extracted from `$shrinkage_details$pval`). This indicates large
#'      deviations from zero, which violates the assumption that ETA has a mean of
#'      zero with a variance of OMEGA. This could be the consequence of asymmetric
#'      shrinkage or a misfit model.
#'    * `prderr` -- `TRUE` if a `PRDERR` file is present in the output folder.
#'      This indicates that NONMEM is unable to compute a prediction for a given
#'      data record with the current THETA and ETA values.
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
#' @seealso [model_summaries()], [summary_log()], [cov_cor()], [param_estimates()]
#' @param .mod Model to summarize.
#' @param .bbi_args A named list specifying arguments to pass to bbi formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`.
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
#' @param .bbi_args A named list specifying arguments to pass to bbi formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`. Run [print_bbi_args()] to see valid arguments.
#'
#' @importFrom purrr list_modify
#' @importFrom rlang list2
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

  # build command line args
  if (is.null(.bbi_args)) {
    .bbi_args <- list()
  }
  .bbi_args <- purrr::list_modify(.bbi_args, json = TRUE)

  args_vec <- check_bbi_args(.bbi_args)
  cmd_args <- c("nonmem", "summary", get_model_id(.mod), args_vec)

  # if .dry_run return output call
  if (.dry_run) {
    res <- bbi_dry_run(cmd_args, .path)
    return(res)
  }

  # otherwise, execute
  res <- bbi_exec(cmd_args, .dir = .path, ..., .wait = TRUE, .check_status = FALSE)

  res_list <- list2(
    !!ABS_MOD_PATH := tools::file_path_sans_ext(get_model_path(.mod))
  )

  bbi_list <- res$stdout %>%
    paste(collapse="") %>%
    jsonlite::fromJSON(simplifyDataFrame = FALSE)

  if (!is.null(bbi_list$error_msg)) {
    err_msg <- glue("model_summary({get_model_id(.mod)}) failed with the following error. This may be because the modeling run has not finished successfully.\n\nERROR: \n{bbi_list$error_msg}")
    stop(err_msg, call. = FALSE)
  }

  res_list <- combine_list_objects(res_list, bbi_list)

  res_list <- create_summary_object(res_list)

  return(res_list)
}
