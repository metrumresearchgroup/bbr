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
#'
#' The following fields from `bbi_nonmem_summary` (the output of
#' `model_summary()`) are extracted and included by default. If you would like
#' more fields from the summary object, you can extract them manually from the
#' `bbi_summary` list column.
#'
#'  * `error_msg` -- Error message from [model_summary()]. If `NULL` the call
#'    succeeded. If not `NULL`, the rest of the fields will be `NULL`.
#'
#'  * `needed_fail_flags` -- Logical for whether the call initially failed, but
#'    passed with the inclusion of `.fail_flags`. See [model_summaries()] docs
#'    for more details.
#'
#'  * `bbi_summary` -- The full `bbi_nonmem_summary` object for each row. This
#'    can be queried further by extracting it as a list, or by using
#'    `dplyr::mutate()` etc.
#'
#'  * `ofv` -- Objective function value _with no constant_ from the final
#'    estimation method. The constant, and the value _with_ the constant can be
#'    found in `$ofv`.
#'
#'  * `param_count` -- Count of (non-fixed) parameters estimated in final
#'    estimation method.
#'
#'  * `estimation_method` -- Character vector of estimation method(s) used.
#'    Extracted from `$run_details`.
#'
#'  * `problem_text` -- Character vector of text from `$PROB`. Extracted from
#'    `$run_details`.
#'
#'  * `number_of_subjects` -- Count of unique subjects in the input data set,
#'    extracted from `$run_details`.
#'
#'  * `number_of_obs` -- Total count of observations in the input data set,
#'    extracted from `$run_details`.
#'
#'  * `condition_number` -- The condition number for the final estimation
#'    method, if present.
#'
#'  * `any_heuristics` -- Logical indicating whether _any_ of the columns
#'    extracted from `$run_heuristics` are `TRUE`. Duplicative information, but
#'    helpful for filtering.
#'
#'  * `$run_heuristics` columns -- One logical column for each element extracted
#'    from `$run_heuristics`. These are named and described in the
#'    [model_summary()] docs.
#'
#' @seealso [run_log()], [config_log()], [model_summary()], [model_summaries()]
#' @inheritParams run_log
#' @param ... Arguments passed through to [model_summaries()].
#' @importFrom dplyr mutate
#' @importFrom purrr map transpose
#' @importFrom tibble as_tibble
#' @importFrom glue glue
#' @importFrom tidyr unnest_wider
#' @export
summary_log <- function(.base_dir, .recurse = FALSE, .include = NULL , ...) {
  checkmate::assert_string(.base_dir)

  mod_list <- find_models(.base_dir, .recurse, .include)

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
#' @importFrom dplyr across select everything mutate
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom tidyselect all_of
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
    return(select(res_df, -all_of(c(SL_SUMMARY, SL_FAIL_FLAGS))))
  }

  # If none of the summary objects are bbi_nonmem_summary objects, the next
  # section will error trying to unnest them, so we just return the objects
  # here.
  no_bbi_sum <- purrr::none(
    res_df[["bbi_summary"]],
    function(x) inherits(x, NM_SUM_CLASS)
  )
  if (no_bbi_sum) {
    return(select(res_df, -all_of(SL_FAIL_FLAGS)))
  }

  fns <- list(
    d = do_if_bbi_sum(extract_details, "list"),
    ofv = do_if_bbi_sum(extract_ofv, "numeric"),
    param_count = do_if_bbi_sum(extract_param_count, "integer"),
    condition_number = do_if_bbi_sum(extract_condition_number, "integer"),
    h = do_if_bbi_sum(extract_heuristics, "list")
  )
  res_df <- mutate(
    res_df,
    across(all_of(SL_SUMMARY), .fns = fns, .names = "{fn}")
  )

  res_df <- res_df %>% unnest_wider("d") %>% unnest_wider("h")

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
#' @param .s A list of `bbi_nonmem_summary` objects to extract from.
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

    # Only index with existing names to avoid $<NA> items, which will make the
    # downstream unnest_wider() call fail.
    idx <- intersect(names(.x), DETAILS_ELEMENTS)
    if (!length(idx)) {
      return(NULL)
    }
    return(.x[idx])
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
    if (is.null(.cn)) {
      .cn <- NA_real_
    }
    return(.cn)
  })
  return(.out)
}

#' Wrap summary extractor so that only `bbi_nonmem_summary` objects are passed
#'
#' @param fn An `extract_from_summary` function that takes a list of
#'   `bbi_nonmem_summary` values.
#' @param mode Mode of result that will be returned by `fn`: "list", "integer",
#'   or "numeric".
#' @return A function that takes a list of `model_summary()` results and calls
#'   `fn` with the subset that are `bbi_nonmem_summary` objects.
#'
#' @noRd
do_if_bbi_sum <- function(fn, mode) {
  function(s) {
    is_sum <- purrr::map_lgl(s, function(x) inherits(x, NM_SUM_CLASS))
    res <- vector(mode = mode, length = length(s))
    res[is_sum] <- fn(s[is_sum])
    if (!identical(mode, "list")) {
      res[!is_sum] <- NA
    }
    return(res)
  }
}
