#' Create a tibble from `model_summaries()` data
#'
#' Runs [model_summaries()] on all models in the input and returns a subset of the each resulting summary as a tibble.
#'
#' @return An object of class `bbi_summary_log_df`, which includes the fields described below. If _all_ model summaries
#' fail, the returned tibble will only contain the `absolute_model_path`, `run`, and `error_msg` columns.
#'
#' `summary_log()` creates a new tibble with one row per model
#' found in `.base_dir` (and subdirectories, if `.recurse = TRUE`).
#'
#' `add_summary()` adds these fields to the tibble passed to `.log_df`.
#'
#' @details
#' The following fields from `bbi_nonmem_summary` (the output of `model_summary()`) are extracted and included by default.
#' If you would like more fields from the summary object, you can extract them manually from the `bbi_summary` list column.
#'
#' * `error_msg` -- Error message from [model_summary()]. If `NULL` the call succeeded. If not `NULL`, the rest of the fields will be `NULL`.
#' * `needed_fail_flags` -- Logical for whether the call initially failed, but passed with the inclusion of `.fail_flags`. See [model_summaries()] docs for more details.
#' * `bbi_summary` -- The full `bbi_nonmem_summary` object for each row. This can be queried further by extracting it as a list, or by using `dplyr::mutate()` etc.
#' * `ofv` -- Objective function value _with no constant_ from the final estimation method. The constant, and the value _with_ the constant can be found in `$ofv`.
#' * `param_count` -- Count of (non-fixed) parameters estimated in final estimation method.
#' * `estimation_method` -- Character vector of estimation method(s) used. Extracted from `$run_details`.
#' * `problem_text` -- Character vector of text from `$PROB`. Extracted from `$run_details`.
#' * `number_of_patients` -- Count of unique patients in the input data set, extracted from `$run_details`.
#' * `number_of_obs` -- Total count of observations in the input data set, extracted from `$run_details`.
#' * `condition_number` -- The condition number for the final estimation method, if present.
#' * `any_heuristics` -- Logical indicating whether _any_ of the columns extracted from `$run_heuristics` are `TRUE`. Duplicative information, but helpful for filtering.
#' * `covariance_step_aborted` -- Extracted from `$run_heuristics`.
#' * `large_condition_number` -- `TRUE` if `condition_number > 1000`. Extracted from `$run_heuristics`.
#' * `correlations_not_ok` -- Not currently implemented in `bbi`; will always be `FALSE`. Extracted from `$run_heuristics`.
#' * `parameter_near_boundary` -- Extracted from `$run_heuristics`.
#' * `hessian_reset` -- Extracted from `$run_heuristics`.
#' * `has_final_zero_gradient` -- Extracted from `$run_heuristics`.
#' * `minimization_terminated` -- Extracted from `$run_heuristics`.
#' * `eta_pval_significant` -- `TRUE` if any of the ETA p-values are < 0.05. Extracted from `$shrinkage_details$pval`.
#' * `prderr` -- `TRUE` if a `PRDERR` file is present in the output folder.
#'
#' @seealso [run_log()], [config_log()]
#' @inheritParams run_log
#' @param ... Arguments passed through to [model_summaries()].
#' @importFrom dplyr mutate
#' @importFrom purrr map transpose
#' @importFrom tibble as_tibble
#' @importFrom glue glue
#' @importFrom tidyr unnest_wider
#' @export
summary_log <- function(.base_dir, .recurse = TRUE, ...) {
  checkmate::assert_string(.base_dir)

  mod_list <- find_models(.base_dir, .recurse)

  res_df <- summary_log_impl(mod_list, ...)

  return(res_df)
}


#' @rdname summary_log
#' @param .log_df a `bbi_run_log_df` tibble (the output of [run_log()])
#' @param ... Arguments passed through to [model_summaries()]
#' @export
add_summary <- function(
  .log_df,
  ...
) {
  df <- add_log_impl(.log_df, summary_log_impl, ...)
  return(df)
}


#' Build summary log
#'
#' Private implementation function for building the summary log from a list of model objects.
#' This is called by both [summary_log()] and [add_summary()].
#' @importFrom stringr str_subset
#' @importFrom fs dir_ls file_exists
#' @importFrom purrr map_df map_chr
#' @importFrom dplyr select everything mutate mutate_at vars
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @param .mods List of model objects that will be passed to [model_summaries()].
#' @param ... Arguments passed through to [model_summaries()]
#' @keywords internal
summary_log_impl <- function(.mods, ...) {

  if(length(.mods) == 0) {
    return(tibble())
  }

  check_model_object_list(.mods)

  res_list <- model_summaries(.mods, ...)

  # create tibble from list of lists
  res_df <- res_list %>%
    transpose() %>%
    as_tibble() %>%
    mutate(
      !!ABS_MOD_PATH := unlist(.data[[ABS_MOD_PATH]]),
      !!SL_ERROR := unlist(.data[[SL_ERROR]]),
      !!SL_FAIL_FLAGS := unlist(.data[[SL_FAIL_FLAGS]])
    ) %>%
    add_run_id_col()

  # if ALL models failed, the next section will error trying to unnest them,
  # and everything else will be NULL/NA anyway, so we just return the errors here.
  if (all(!is.na(res_df[[SL_ERROR]]))) {
    warning(glue("ALL {nrow(res_df)} MODEL SUMMARIES FAILED in `summary_log()` call. Check `error_msg` column for details."))
    return(select(res_df, -.data[[SL_SUMMARY]], -.data[[SL_FAIL_FLAGS]]))
  }

  res_df <- mutate_at(
    res_df,
    vars(SL_SUMMARY),
    list(
      d = extract_details,
      ofv = extract_ofv,
      param_count = extract_param_count,
      condition_number = extract_condition_number,
      h = extract_heuristics
    ))

  res_df <- res_df %>% unnest_wider(.data$d) %>% unnest_wider(.data$h)

  res_df <- create_summary_log_object(res_df)

  return(res_df)
}

###################
# helper functions
###################

#' Extract from summary object
#'
#' These are all helper functions to extract a specific field or sub-field from a `bbi_{.model_type}_summary` object.
#' @name extract_from_summary
#' @param .s The summary object to extract from
#' @keywords internal
NULL

#' @describeIn extract_from_summary Extract count of non-fixed parameters
#' @importFrom purrr map map_int
#' @keywords internal
extract_param_count <- function(.s) {

  .pm <- map(.s, "parameters_data")

  .out <- map_int(.pm, function(.x) {

    .fix <- unlist(.x[[length(.x)]]$fixed)

    .pm_count <- length(.fix) - sum(.fix)

    if (.pm_count == 0) {
      .pm_count <- NA_integer_
    }
    .pm_count
  })

  return(.out)
}

#' @describeIn extract_from_summary Extract `run_details` field
#' @importFrom purrr map
#' @keywords internal
extract_details <- function(.s) {
  .rd <- map(.s, SUMMARY_DETAILS, .default = NA)

  .out <- map(.rd, function(.x) {
    if(!inherits(.x, "list")) {
      return(NULL)
    }

    return(.x[DETAILS_ELEMENTS])
  })

  return(.out)
}

#' @describeIn extract_from_summary Extract `run_heuristics` field
#' @importFrom purrr map
#' @keywords internal
extract_heuristics <- function(.s) {
  .rh <- map(.s, "run_heuristics", .default = NA)

  .out <- map(.rh, function(.x) {
    if(!inherits(.x, "list")) {
      return(NULL)
    }

    .any <- list()
    .any[[ANY_HEURISTICS]] = any(unlist(.x))

    return(combine_list_objects(.any, .x))
  })

  return(.out)
}

#' @describeIn extract_from_summary Extract objective function value (without constant) for the final estimation method
#' @importFrom purrr map map_dbl
#' @keywords internal
extract_ofv <- function(.s) {
  .ofv <- map(.s, function(.x) {
    .x <- .x[["ofv"]]
    if (!is.null(.x)) {
      .x <- .x[[length(.x)]] # take the final estimation method
    }
    return(.x)
  })
  .out <- map_dbl(.ofv, "ofv_no_constant", .default = NA_real_)
  return(.out)
}

#' @describeIn extract_from_summary Extract condition number for the final estimation method
#' @importFrom purrr map map_dbl
#' @keywords internal
extract_condition_number <- function(.s) {
  .ofv <- map(.s, function(.x) {
    .x <- .x[[SUMMARY_COND_NUM]]
    if (!is.null(.x)) {
      .x <- .x[[length(.x)]] # take the final estimation method
    }
    return(.x)
  })
  .out <- map_dbl(.ofv, function(.x) {
    .cn <- .x[[SUMMARY_COND_NUM]]
    if (is.null(.cn) || .cn == BBI_NULL_NUM) {
      .cn <- NA_real_
    }
    return(.cn)
  })
  return(.out)
}

