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
#'  * `aic`, `bic` -- Akaike information criterion and Bayesian information
#'    criterion. These are calculated using NONMEM's objective function value
#'    with constant (available as of NONMEM 7.4) as approximations of the -2
#'    log-likelihood.
#'
#'    NA is reported for models where the final estimation method is one for
#'    which these derived quantities are not meaningful for model comparison
#'    (e.g., SAEM or Bayesian methods).
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

  # NB: Pass calc_aic_bic so that it's not exposed it to summary_log callers via
  # '...'.
  res_df <- summary_log_impl(mod_list, calc_aic_bic = TRUE, ...)

  return(res_df)
}


#' @rdname summary_log
#' @param .log_df a `bbi_run_log_df` tibble (the output of [run_log()])
#' @param ... Arguments passed through to [model_summaries()]
#' @seealso [add_dofv()]
#' @export
add_summary <- function(
  .log_df,
  ...
) {
  # NB: Pass calc_aic_bic so that it's not exposed it to add_summary callers via
  # '...'.
  df <- add_log_impl(.log_df, summary_log_impl, calc_aic_bic = TRUE, ...)
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
#' @param calc_aic_bic Whether to add columns for AIC and BIC.
#' @param ... Arguments passed through to [model_summaries()]
#' @keywords internal
summary_log_impl <- function(
  .mods,
  calc_aic_bic = TRUE,
  ...
) {
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

  if (isTRUE(calc_aic_bic)) {
    res_df <- add_aic_bic(res_df)
  }

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

#' @describeIn extract_from_summary Extract `data_set` field from `run_details`.
#' @keywords internal
extract_data_set <- function(.s) {
  purrr::map_chr(.s, c(SUMMARY_DETAILS, "data_set"), .default = NA)
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

#' @describeIn extract_from_summary Extract objective function value for the
#'   final estimation method.
#' @param with_constant Whether to extract the objective function value with or
#'   without constant.
#' @importFrom purrr map map_dbl
#' @keywords internal
extract_ofv <- function(.s, with_constant = FALSE) {
  name <- if (with_constant) "ofv_with_constant" else "ofv_no_constant"
  .ofv <- map(.s, function(.x) {
    .x <- .x[["ofv"]]
    if (!is.null(.x)) {
      .x <- .x[[length(.x)]] # take the final estimation method
    }
    return(.x)
  })
  .out <- map_dbl(.ofv, name, .default = NA_real_)
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

get_final_est_method <- function(log_df) {
  purrr::map_chr(
    log_df[[SUMMARY_EST_METHOD]],
    function(xs) {
      n <- length(xs)
      if (n) xs[[n]] else NA_character_
    }
  )
}

#' Are OFV-based calculations enabled for the specified method?
#'
#' @param method Character vector of methods, as reported by NONMEM on its
#'   "#METH:" line.
#' @noRd
allow_ofv_calc <- function(method) {
  !is.na(method) &
    # Select methods for which it is meaningful (based on SME input) to
    # calculate AIC, BIC, and (subject to more conditions) dOFV.
    stringr::str_starts(
      method,
      stringr::regex(
        paste0(
          "\\s*",
          c(
            # Examples:
            #  * First Order (Evaluation)
            #  * First Order Conditional Estimation with Interaction (No Prior) (Evaluation)
            "first order",
            # Examples:
            #  * Importance Sampling
            #  * Importance Sampling assisted by MAP Estimation
            "importance sampling",
            # Examples:
            #  * Iterative Two Stage
            #  * Iterative Two Stage (No Prior)
            "iterative two stage",
            # Examples:
            #  * Laplacian Conditional Estimation
            #  * Laplacian Conditional Estimation (Centered)
            "laplacian conditional estimation",
            # Examples:
            #  * Objective Function Evaluation by Importance Sampling (No Prior)
            #  * Objective Function Evaluation by Importance/MAP Sampling
            "objective function evaluation by importance"
          ),
          collapse = "|"
        ),
        ignore_case = TRUE
      )
    ) &
    !stringr::str_detect(
      method,
      # Examples:
      #  * First Order: BAYESIAN-OPTIMALITY
      #  * First Order: D-OPTIMALITY DIM SCALED
      stringr::fixed("optimality", ignore_case = TRUE)
    ) &
    !stringr::str_detect(
      method,
      stringr::fixed(
        "importance sampling of variance-covariance (sir)",
        ignore_case = TRUE
      )
    )
}

add_aic_bic <- function(log_df) {
  fn <- do_if_bbi_sum(
    function(x) extract_ofv(x, with_constant = TRUE),
    "numeric"
  )
  ofv <- fn(log_df[[SL_SUMMARY]])

  nparams <- log_df[[PARAM_COUNT_COL]]
  aic <- 2 * nparams + ofv
  bic <- nparams * log(log_df[[SUMMARY_NOBS]]) + ofv

  off <- !allow_ofv_calc(get_final_est_method(log_df))
  aic[off] <- NA_real_
  bic[off] <- NA_real_

  tibble::add_column(
    log_df,
    !!AIC_COL := aic,
    !!BIC_COL := bic,
    .after = OFV_COL
  )
}

#' Calculate change in objective function value of models in a run log
#'
#' Extend a run log with a "dofv" column that reports the difference in
#' objective function value between the row's model and a reference model.
#'
#' @param .log_df A `bbi_run_log_df` tibble (as output by [run_log()] or
#'   [summary_log()]).
#' @param .mod_ref The reference model to use when calculating the change in
#'   objective function value for each model in the run log. By default, the
#'   reference model is each model's parent (as identified by the first item
#'   returned by [get_based_on()]). Pass a `bbi_nonmem_model` object to use its
#'   objective function value as the reference value for all calculations.
#' @return The `.log_df` data frame with a new "dofv" column. If `.log_df` is
#'   not a summary log, an "ofv" column is also added for context.
#'
#' @details
#'
#' To help the caller avoid making comparisons that are not meaningful, the
#' "dofv" value is reported as NA if any of the following is true:
#'
#'   * the final estimation methods (as reported in the final "#METH:" line of
#'     the `.lst` file) of the model and reference model are not identical .
#'
#'   * the final estimation method is one for which the objective function value
#'     is not meaningful for model comparison (e.g., SAEM or Bayesian methods).
#'
#'   * the input data sets of the two models differs, as indicated by a
#'     different file path or different number of observations recorded in the
#'     `.lst` files.
#'
#'     The above criteria serve to flag the common cases where the models use
#'     different input data. Note, however, that models may have used different
#'     data despite having a matching data path and number of observations.
#'
#' @seealso [add_summary()]
#' @export
add_dofv <- function(.log_df, .mod_ref = NULL) {
  cols <- names(.log_df)
  is_slog <- OFV_COL %in% cols
  req_cols <- ABS_MOD_PATH
  if (is_slog) {
    # If "ofv" column is found, the downstream code treats .log_df as a summary
    # log and requires additional columns to be present.
    req_cols <- c(req_cols, SL_SUMMARY, SUMMARY_EST_METHOD, SUMMARY_NOBS)
  }
  missing <- setdiff(req_cols, cols)
  if (length(missing)) {
    rlang::abort(c("`.log_df` is missing required columns:", missing))
  }

  res <- make_ofv_pairs(.log_df, .mod_ref)
  off <- !allow_ofv_calc(res[["method_mod"]]) |
    res[["method_mod"]] != res[["method_ref"]] |
    res[["nobs_mod"]] != res[["nobs_ref"]] |
    res[["dataset_mod"]] != res[["dataset_ref"]]
  res[["ofv_ref"]][off] <- NA_real_

  if (!is_slog) {
    .log_df[[OFV_COL]] <- res[["ofv_mod"]]
  }

  tibble::add_column(
    .log_df,
    !!DOFV_COL := res[["ofv_mod"]] - res[["ofv_ref"]],
    .after = OFV_COL
  )
}

#' For each row of summary log `slog`, extract the data file path from summary
#' object and expand it to an absolute path.
#' @noRd
summary_expand_data_path <- function(slog) {
  fn <- do_if_bbi_sum(extract_data_set, "character")
  paths <- fn(slog[[SL_SUMMARY]])
  paths[!fs::is_absolute_path(paths)] <- fs::path(
    slog[[ABS_MOD_PATH]],
    paths
  )
  # Note: path_norm is a pure path computation and doesn't resolve symbolic
  # links. In the context of restricting the dOFV calculation to models with the
  # same path, this means that comparing the result may indicate
  #
  #  * paths are different despite resolving to the same path on disk
  #
  #  * paths are the same despite resolving to different paths on disk. This is
  #    because path_norm may change the meaning of a path with a symlink (e.g.,
  #    it simplifies /a/b/../c to /a/c, but that could resolve to something
  #    other than /a/c if b/ is a symlink).
  #
  # To avoid the above scenarios, we could require that the path exist on disk
  # and resolve it (e.g., with path_real). However, stick with path_norm because
  #
  #  1) other places in bbr already use path_norm, including get_based_on
  #
  #  2) symlinks in ABS_MOD_PATH are already resolved by read_model, which makes
  #     hitting into the above scenarios even less likely
  #
  #  3) this information is used only to decide whether to set dofv to NA
  return(as.character(fs::path_norm(paths)))
}

#' Pair each model OFV with a reference OFV
#'
#' If mod_ref is passed, use it as the reference model. Otherwise, take the
#' first parent of each model as the reference model.
#'
#' Return a data frame with the following columns:
#'
#'  * path_mod, path_ref: The absolute paths of the main model and the reference
#'    model. path_mod matches the absolute_model_path column from the input run
#'    log.
#'
#'  * ofv_mod, ofv_ref: The objective function values for the main model and the
#'    reference model.
#'
#'  * nobs_mod, nobs_ref: The number of observations for the main model and the
#'    reference model.
#'
#'  * dataset_mod, dataset_ref: Paths to the data sets of the main model and the
#'    reference model.
#'
#'  * method_mod, method_ref: The final estimation method for the main model and
#'    the reference model.
#' @noRd
make_ofv_pairs <- function(log_df, mod_ref) {
  if (OFV_COL %in% names(log_df)) {
    info <- log_df[c(ABS_MOD_PATH, OFV_COL, SUMMARY_NOBS)]
    info[["dataset"]] <- summary_expand_data_path(log_df)
    info[["method"]] <- get_final_est_method(log_df)
  } else {
    info <- tibble::tibble(
      !!ABS_MOD_PATH := character(),
      !!OFV_COL := double(),
      !!SUMMARY_NOBS := integer(),
      "dataset" = character(),
      "method" = character()
    )
  }

  mpaths_log <- log_df[[ABS_MOD_PATH]]

  if (is.null(mod_ref)) {
    mods <- purrr::map(mpaths_log, read_model)
    mpaths_ref <- purrr::map_chr(mods, function(m) {
      get_based_on(m, .check_exists = FALSE)[1] %||% NA_character_
    })
  } else {
    mods <- NULL
    mpaths_ref <- mod_ref[[ABS_MOD_PATH]]
  }

  if (any(!is.na(mpaths_ref) & !mpaths_ref %in% info[[ABS_MOD_PATH]])) {
    # ^ Summary information is missing for some models either because 1) log_df
    # was not a summary log or 2) some reference models are not included in
    # log_df.
    mods <- mods %||% purrr::map(mpaths_log, read_model)
    mpaths_extra <- setdiff(mpaths_ref, mpaths_log)
    mods_extra <- purrr::map(mpaths_extra[!is.na(mpaths_extra)], read_model)
    mods_all <- c(mods, mods_extra)
    mpaths_all <- purrr::map_chr(mods_all, ABS_MOD_PATH)
    mods_need <- mods_all[!mpaths_all %in% info[[ABS_MOD_PATH]]]
    if (!length(mods_need)) {
      dev_error("mods_need should never come up empty")
    }

    # Call summary_log_impl rather than model_summaries to reuse its logic for
    # reshaping the result.
    slog <- summary_log_impl(mods_need, calc_aic_bic = FALSE)
    slog <- slog[!slog[[ABS_MOD_PATH]] %in% info[[ABS_MOD_PATH]], ]
    slog[["dataset"]] <- summary_expand_data_path(slog)
    slog[["method"]] <- get_final_est_method(slog)

    info <- dplyr::bind_rows(info, slog[names(info)])
  }

  info_mod <- dplyr::rename(
    info,
    "ofv_mod" = all_of(OFV_COL),
    "nobs_mod" = all_of(SUMMARY_NOBS),
    "dataset_mod" = "dataset",
    "method_mod" = "method"
  )
  info_ref <- dplyr::rename(
    info,
    "ofv_ref" = all_of(OFV_COL),
    "nobs_ref" = all_of(SUMMARY_NOBS),
    "dataset_ref" = "dataset",
    "method_ref" = "method"
  )
  tibble::tibble(path_mod = mpaths_log, path_ref = mpaths_ref) %>%
    dplyr::left_join(info_mod, by = c("path_mod" = ABS_MOD_PATH)) %>%
    dplyr::left_join(info_ref, by = c("path_ref" = ABS_MOD_PATH))
}
