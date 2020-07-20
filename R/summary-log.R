#' Add data from `model_summaries()` to `bbi_run_log_df`
#'
#' Runs `model_summaries()` on all models in the input and return a subset of the resulting summaries as a tibble.
#' `summary_log()` will return a new tibble with the `"absolute_model_path"` column as the primary key,
#' and all the columns extracted from `model_summaries()` (but none of the other columns from the input tibble).
#' `add_summary()` returns the input tibble, with all the columns extracted from `model_summaries()` joined onto it.
#' See details section for fields included.
#'
#' @details
#' The following fields from `bbi_nonmem_summary` are extracted and included by default.
#' If you would like more fields, you can set `.keep_bbi_object = TRUE` and extract them manually.
#'
#' * `error_msg` -- Error message from `model_summary()`. If `NULL` the call succeeded. If not `NULL`, the rest of the fields will be `NULL`.
#' * `needed_fail_flags` -- Logical for whether the call initially failed, but passed with the inclusion of `.fail_flags`
#' * `bbi_summary` -- The full `bbi_nonmem_summary` object. This will only be included if `.keep_bbi_object = TRUE` was passed.
#' * `ofv` -- Objective function value from last estimation method, using `$ofv$ofv_no_constant` field.
#' * `param_count` -- Count of (non-fixed) parameters estimated in final estimation method.
#' * `estimation_method` -- Character vector of estimation method(s) used. Extracted from `$run_details`
#' * `problem_text` -- Character vector of text from `$PROB`. Extracted from `$run_details`
#' * `number_of_patients` -- Extracted from `$run_details`
#' * `number_of_obs` -- Extracted from `$run_details`
#' * `covariance_step_aborted` -- Extracted from `$run_heuristics`
#' * `large_condition_number` -- Extracted from `$run_heuristics`
#' * `correlations_not_ok` -- Extracted from `$run_heuristics`
#' * `parameter_near_boundary` -- Extracted from `$run_heuristics`
#' * `hessian_reset` -- Extracted from `$run_heuristics`
#' * `has_final_zero_gradient` -- Extracted from `$run_heuristics`
#' * `minimization_terminated` -- Extracted from `$run_heuristics`
#'
#' @param .mods The model object that will be passed through to `model_summaries()` before constructing the output tibble. Could be
#' a `bbi_run_log_df` tibble,
#' a list of `bbi_{.model_type}_model ` objects,
#' a character vector of file paths to models,
#' a numeric vector of integers corresponding to a file names of a models.
#' Could also be a `bbi_summary_list` (output from `model_summaries()`) in which case it is passed straight through and `model_summaries()` is _not_ re-run.
#' @param .keep_bbi_object `FALSE` by default. If `TRUE`, a list column will be added containing the full `bbi_nonmem_summary` object for each row.
#' Use this if you would like to extract additional fields from the summary object.
#' @param ... Arguments passed through to `model_summaries()`.
#' @importFrom dplyr mutate
#' @importFrom purrr map transpose
#' @importFrom tibble as_tibble
#' @importFrom glue glue
#' @importFrom tidyr unnest_wider
#' @export
summary_log <- function(
  .mods,
  ...,
  .keep_bbi_object = FALSE
) {

  # get list of summaries
  if (inherits(.mods, "bbi_summary_list")) {
    res_list <- .mods
  } else {
    res_list <- model_summaries(.mods, ...)
  }

  # create tibble from list of lists
  res_df <- res_list %>% transpose() %>% as_tibble() %>%
    mutate(
      !!ABS_MOD_PATH := unlist(.data[[ABS_MOD_PATH]]),
      error_msg = unlist(.data$error_msg),
      needed_fail_flags = unlist(.data$needed_fail_flags)
    )

  # if ALL models failed, the next section will error trying to unnest them,
  # and everything else will be NULL/NA anyway, so we just return the errors here.
  if (all(!is.na(res_df$error_msg))) {
    warning(glue("ALL {nrow(res_df)} MODEL SUMMARIES FAILED in `summary_log()` call. Check `error_msg` column for details."))
    return(select(res_df, -.data$bbi_summary, -.data$needed_fail_flags))
  }

  res_df <- mutate(res_df,
      d =            extract_details(.data$bbi_summary),
      ofv =          extract_ofv(.data$bbi_summary),
      param_count =  extract_param_count(.data$bbi_summary),
      h =            extract_heuristics(.data$bbi_summary)
    ) %>%
    unnest_wider(.data$d) %>% unnest_wider(.data$h)

  if (isFALSE(.keep_bbi_object)) {
    res_df <- select(res_df, -.data$bbi_summary)
  }

  return(res_df)
}


#' @describeIn summary_log Create `summary_log()` tibble and join against the input `bbi_run_log_df` tibble.
#' @param .log_df a `bbi_run_log_df` tibble
#' @importFrom dplyr left_join
add_summary <- function(
  .log_df,
  ...,
  .keep_bbi_object = FALSE
) {

  # check input df
  check_bbi_run_log_df_object(.log_df)

  sum_df <- summary_log(
    .mods = .log_df,
    ...,
    .keep_bbi_object = .keep_bbi_object
  )

  out_df <- left_join(.log_df, sum_df, by = ABS_MOD_PATH)

  return(out_df)
}


###################
# helper functions
###################

#' Extract from summary object
#'
#' These are all helper functions to extract a specific field or sub-field from a `bbi_{.model_type}_summary` object.
#' @name extract_from_summary
#' @param .s The summary object to extract from
NULL

#' @describeIn extract_from_summary Extract count of non-fixed parameters
#' @importFrom purrr map map_int
extract_param_count <- function(.s) {

  .pm <- map(.s, "parameters_data")

  .out <- map_int(.pm, function(.x) {
    num_methods <- length(.x)

    .fix <- unlist(.x[[num_methods]]$fixed)
    length(.fix) - sum(.fix)
  })

  return(.out)
}

#' @describeIn extract_from_summary Extract `run_details` field
#' @importFrom purrr map
extract_details <- function(.s) {
  .rd <- map(.s, "run_details", .default = NA)

  KEEPERS <- c("estimation_method", "problem_text", "number_of_patients", "number_of_obs")

  .out <- map(.rd, function(.x) {
    if(!inherits(.x, "list")) {
      return(NULL)
    }
    return(.x[KEEPERS])
  })

  return(.out)
}

#' @describeIn extract_from_summary Extract `run_heuristics` field
#' @importFrom purrr map
extract_heuristics <- function(.s) {
  .rh <- map(.s, "run_heuristics", .default = NA)

  KEEPERS <- c(
    "covariance_step_aborted",
    "large_condition_number",
    "correlations_not_ok",
    "parameter_near_boundary",
    "hessian_reset",
    "has_final_zero_gradient",
    "minimization_terminated"
  )

  .out <- map(.rh, function(.x) {
    if(!inherits(.x, "list")) {
      return(NULL)
    }
    return(.x[KEEPERS])
  })

  return(.out)
}

#' @describeIn extract_from_summary Extract objective function value
#' @importFrom purrr map map_dbl
#' @param .subfield The field to extract from within "ofv". Defaults to the objective function value with no constant added, but the other fields are available too.
extract_ofv <- function(.s, .subfield = c("ofv_no_constant", "ofv_with_constant", "ofv")) {
  .subfield <- match.arg(.subfield)
  .ofv <- map(.s, "ofv")
  .out <- map_dbl(.ofv, .subfield, .default = NA_real_)
  return(.out)
}


