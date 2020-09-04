
############################################
# format NONMEM output to parameter tables
############################################

#' Parses parameter estimates table
#'
#' Returns a tibble containing parameter estimates from a model or batch of models.
#' Takes either a `bbi_{.model_type}_summary`, `bbi_summary_list`, or `bbi_summary_log_df` object.
#' Details about the tibble that is returned are in the return value section below.
#'
#' @return
#' **Single model:** For `bbi_{.model_type}_summary` dispatch returns a tibble with the following columns:
#'
#' * parameter_names -- Parameter name ("THETA1", "THETA2", etc.)
#' * estimate -- Parameter estimate
#' * stderr -- Standard Error
#' * random_effect_sd -- OMEGA and SIGMA elements in standard deviation/correlation format
#' * random_effect_sdse -- Standard errors to the OMEGA and SIGMA elements in standard deviation/correlation format
#' * fixed -- TRUE if parameter is fixed, FALSE otherwise
#' * diag -- TRUE if parameter is a diagonal element in random effect matrix, FALSE if off-diagonal, NA if parameter is a THETA
#'
#' **Multiple models:** For `bbi_summary_list` or `bbi_summary_log_df` objects returns a long-format tibble with the following columns for all models in the input:
#'
#' * absolute_model_path -- absolute path to the model yaml (without extension), serving as the unique identifier for the model
#' * parameter_names -- Parameter name ("THETA1", "THETA2", etc.)
#' * estimate -- Parameter estimate
#'
#' See examples section for how to pivot this to a wide-format table (one model per row, one parameter per column).
#'
#' @examples
#' \dontrun{
#'  # pivot to a wide table
#'  summary_log() %>%
#'    param_estimates() %>%
#'    tidyr::pivot_wider(
#'      names_from = parameter_names,
#'      values_from = estimate
#'    )
#' }
#'
#' @seealso [param_labels()] [apply_indices()]
#' @param .summary `bbi_{.model_type}_summary`, `bbi_summary_list`, or `bbi_summary_log_df` object
#' @export
param_estimates <- function(.summary) {
  UseMethod("param_estimates")
}

#' @describeIn param_estimates Takes `bbi_nonmem_summary` object, the output of [model_summary()].
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map_at map_depth map_lgl
#' @importFrom rlang list2
#' @export
param_estimates.bbi_nonmem_summary <- function(.summary) {
  num_methods <- length(.summary[[SUMMARY_PARAM_DATA]])
  param_names <- .summary[[SUMMARY_PARAM_NAMES]]

  summary_vars <- with(
    .summary[[SUMMARY_PARAM_DATA]][[num_methods]],
    list(
      estimate = estimates,
      stderr = std_err,
      random_effect_sd = random_effect_sd,
      random_effect_sdse = random_effect_sdse,
      fixed = fixed
    )
  )

  # left-pad the random effect variables with NAs
  summary_vars_padded <- purrr::map_at(
    summary_vars,
    .at = c("random_effect_sd", "random_effect_sdse"),
    ~ c(rep(NA, length(param_names[["theta"]])), .)
  )

  all_vars <- c(
    rlang::list2(!!SUMMARY_PARAM_NAMES := param_names),
    summary_vars_padded
  )

  param_df <-
    all_vars %>%
    purrr::map_depth(.depth = 1, unlist, use.names = FALSE) %>%
    purrr::map_at(
      .at = c("stderr", "random_effect_sd", "random_effect_sdse"),
      ~ . %||% NA_real_
    ) %>%
    tibble::as_tibble()

  param_df[["fixed"]] <- param_df[["fixed"]] == 1 # convert from 1/0 to T/F
  param_df[["diag"]] <- map_lgl(param_df[[SUMMARY_PARAM_NAMES]], is_diag)

  return(param_df)
}


#' @describeIn param_estimates Takes a `bbi_summary_list` object, the output of [model_summaries()].
#' @importFrom purrr map_df
#' @importFrom dplyr mutate select
#' @export
param_estimates.bbi_summary_list <- function(.summary) {

  param_df <- map_df(.summary, function(.s) {
    if(!is.na(.s[[SL_ERROR]])){
      warning(paste("Missing summary for", .s[[ABS_MOD_PATH]], "\n", .s[[SL_ERROR]]))
      return(NULL)
    }
    .s[[SL_SUMMARY]] %>%
      param_estimates() %>%
      mutate(!!ABS_MOD_PATH := .s[[ABS_MOD_PATH]]) %>%
      select(.data[[ABS_MOD_PATH]], .data[[SUMMARY_PARAM_NAMES]], .data[["estimate"]])
  })

  return(param_df)
}

#' @describeIn param_estimates Takes a `bbi_summary_log_df` object, the output of [summary_log()].
#' @export
param_estimates.bbi_summary_log_df <- function(.summary) {
  # extract needed pieces into a `bbi_summary_list` object
  .summary <- as_summary_list(.summary)

  # pass to `bbi_summary_list` dispatch
  param_df <- param_estimates(.summary)

  return(param_df)
}


#' Check if diagonal index or not
#'
#' Private helper to unpack an matrix index string like '(3,3)' is for a diagonal (i.e. if the numbers are the same)
#' @param .name A character scalar containing an index string
#' @importFrom stringr str_replace_all str_split
#' @keywords internal
is_diag <- function(.name) {
  .ind <- .name %>%
    str_replace_all(glue("^.*\\(|\\)"), "") %>%
    str_split(",") %>% unlist()

  if (length(.ind) == 1) {
    return(invisible(NA))
  }

  return(.ind[1] == .ind[2])
}
