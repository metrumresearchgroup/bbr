#' Add data from `model_summary()` to `bbi_run_log_df`
#'
#' Takes a `bbi_run_log_df` tibble and runs `model_summary()` for each model in the tibble.
#' `summary_log()` will return a new tibble with the `"absolute_model_path"` column as the primary key,
#' and all the columns extracted from `model_summary()` (but none of the other columns from the input tibble).
#' `add_summary()` returns the input tibble, with all the columns extracted from `model_summary()` joined onto it.
#' See details section for fields included.
#'
#' @details
#' @section .fail_flags:
#' The summary call will error if it does not find certain files in the output folder.
#' However, you can override this behavior with the following file-specific flags:
#'  * `no_cor_file`
#'  * `no_cov_file`
#'  * `no_ext_file`
#'  * `no_grd_file`
#'  * `no_shk_file`
#'
#' If some of your runs are using an estimation method that does not produce any of the following files,
#' or they are missing for some other legitimate reason, pass the appropriate flags through the `.fail_flags` argument.
#' For example, if have asked to skip the `$COV` step, you would call `model_summary(..., .fail_flags = list(no_cov_file = TRUE))`.
#'
#' #' @section Included fields:
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
#' @param .log_df `bbi_run_log_df` tibble
#' @param .bbi_args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`.
#' See `print_nonmem_args()` for full list of options.
#' **If you have renamed your `.ext` file, you will need to pass the new name through** here (or `.fail_flags`) like so:
#' `summary_log(..., .bbi_args = list(ext_file = "whatever_you_named_it"))`
#' @param .fail_flags Same as `.bbi_args` except these are used _only_ when a `model_summary()` call fails.
#' In that case, flags are appended to anything in `.bbi_args` and the summary is tried again.
#' Defaults to `list(no_grd_file = TRUE, no_shk_file = TRUE)`.
#' This is a "dumb" way of automatically catching Bayesian or something else that doesn't produce certain output files.
#' See details section for more info on these flags.
#' @param `FALSE` by default. If `TRUE`, a list column will be added containing the full `bbi_nonmem_summary` object for each row.
#' Use this if you would like to extract additional fields from the summary object.
#' @importFrom dplyr transpose mutate
#' @importFrom purrr map
#' @importFrom tibble as_tibble
#' @importFrom glue glue
#' @export
summary_log <- function(
  .log_df,
  .bbi_args = NULL,
  .fail_flags = list(no_grd_file = TRUE, no_shk_file = TRUE),
  .keep_bbi_object = FALSE
) {
  # check input df
  if (!inherits(.log_df, "bbi_run_log_df")) {
    stop(glue("Can only pass an object of class `bbi_run_log_df` to `summary_log()`. Passed object has classes {paste(class(.log_df), collapse = ', ')}"))
  }

  # get model paths
  model_paths <- get_model_path(.log_df)

  # map over paths to get summaries
  res_list <- map(model_paths, function(.mf) {
    .s <- tryCatch(
      {
        rbabylon::model_summary(.mf, .bbi_args = .bbi_args)
      },
      error = function(.e) {
        # if fails, try again with flags
        tryCatch({
          if (!is.null(.bbi_args)) {
            .fail_flags <- rbabylon:::combine_list_objects(.fail_flags, .bbi_args)
          }
          .retry <- rbabylon::model_summary(.mf, .bbi_args = .fail_flags)
          .retry$needed_fail_flags <- TRUE
          return(.retry)
        },
        error = function(.e) {
          # message(glue("Summary failed for {.mf}"))
          .error_msg <- paste(as.character(.e$message), collapse = " -- ")
          return(list(error_msg = .error_msg))
        })
      }
    )
    return(list(
      absolute_model_path = tools::file_path_sans_ext(.mf),
      error_msg = .s$error_msg %||% NA_character_,
      needed_fail_flags = .s$needed_fail_flags %||% FALSE,
      bbi_summary = .s
    ))
  })

  # create tibble from list of lists
  res_df <- res_list %>% transpose() %>% as_tibble() %>%
    mutate(
      absolute_model_path = unlist(absolute_model_path),
      error_msg = unlist(error_msg),
      needed_fail_flags = unlist(needed_fail_flags)
    )

  res_df <- mutate(res_df,
      d =            extract_details(bbi_summary),
      ofv =          extract_ofv(bbi_summary),
      param_count =  extract_param_count(bbi_summary),
      h =            extract_heuristics(bbi_summary)
    ) %>%
    unnest_wider(d) %>% unnest_wider(h) #https://stackoverflow.com/questions/49689927/unnest-a-list-column-directly-into-several-columns

  if (isFALSE(.keep_bbi_object)) {
    res_df <- select(res_df, -bbi_summary)
  }

  return(res_df)
}


#' @describeIn summary_log Create `summary_log()` tibble and join against the input `bbi_run_log_df` tibble.
#' @importFrom dplyr left_join
add_summary <- function(
  .log_df,
  .bbi_args = NULL,
  .fail_flags = list(no_grd_file = TRUE, no_shk_file = TRUE),
  .keep_bbi_object = FALSE
) {
  sum_df <- summary_log(
    .log_df = .log_df,
    .bbi_args = .bbi_args,
    .fail_flags = .fail_flags,
    .keep_bbi_object = .keep_bbi_object
  )

  out_df <- left_join(.log_df, sum_df, by = "absolute_model_path")

  return(out_df)
}


###################
# helper functions
###################

extract_param_count <- function(.s) {

  .pm <- map(.s, "parameters_data")

  .out <- map_int(.pm, function(.x) {
    num_methods <- length(.x)

    .fix <- unlist(.x[[num_methods]]$fixed)
    length(.fix) - sum(.fix)
  })

  return(.out)
}

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

extract_ofv <- function(.s) {
  .ofv <- map(.s, "ofv")
  .out <- map_dbl(.ofv, "ofv_no_constant", .default = NA_real_)
  return(.out)
}


