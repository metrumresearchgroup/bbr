
#' Create log of model summary outputs
#'
#' This should probably be an implementation function for an S3 method that can take a list of model objects or paths etc.
#' Or maybe just the character (default?) dispatch? Need to think on that.
#' @param .models A character vector of paths to models. Can be absolute or relative to working directory, but will come out as absolute. All must exist.
#' @param ... Arguments passed through to model_summary()
#' @importFrom dplyr select everything
#' @importFrom purrr map
#' @rdname summary_log
#' @export
summary_log <- function(.models, ...) {
  # make sure paths are absolute and actually exist
  .models <- normalizePath(.models, mustWork = TRUE)

  # iterate over models and try to run bbi_summary on each one
  res_df <- map(.models, function(.mf) {
    .m <- read_model(.mf)
    .s <- tryCatch(
      {
        model_summary(.m, ...)
      },
      error = function(.e) {
        warning(glue("`model_summary({.mf})` failed"), call. = FALSE)
        .error_msg <- paste(as.character(.e$message), collapse = " -- ")
        return(list(error_msg = .error_msg))
      }
    )

    # construct output list
    res_list <- list(summary = .s)
    res_list[[SUMMARY_ERROR_COL]] <- .s$error_msg %||% NA_character_
    res_list[[ABS_MOD_PATH]] <- .mf
    return(res_list)
  })

  res_df <- as_tibble(transpose(res_df))
  res_df <- mutate(res_df,
                   !!ABS_MOD_PATH      := unlist(.data[[ABS_MOD_PATH]]),
                   !!SUMMARY_ERROR_COL := unlist(.data[[SUMMARY_ERROR_COL]])
  ) %>% select(.data[[ABS_MOD_PATH]], everything())

  return(res_df)
}


#' Joins output of `summary_log()` to a `bbi_run_log_df` object (aka the output of `run_log()`)
#' @param .log_df `bbi_run_log_df` object to add summary to
#' @param ... arguments passed through to `summary_log()`
#' @importFrom dplyr select inner_join mutate
#' @rdname summary_log
#' @export
add_summary <- function(.log_df, ...) {

  if (!inherits(.log_df, "bbi_run_log_df")) {
    stop(glue("Must pass a `bbi_run_log_df` object to `add_summary(.log_df)`. Passed an object with class: `{paste(class(.log_df), collapse=', ')}`"))
  }

  # run summary on each model
  res_df <- summary_log(.log_df[[ABS_MOD_PATH]], ...)

  # parse the summary object and then drop the list column with the raw object
  res_df <- parse_summary_col(res_df)
  res_df <- select(res_df, -summary)

  # join against run_log output
  out_df <- inner_join(
    .log_df,
    res_df,
    by = ABS_MOD_PATH
  )

}


#' Parses the list column with the bbi_nonmem_summary object into separate (wide-format) columns
#' @param .df A tibble with a list column called "summary" containing bbi_nonmem_summary objects
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest_wider
#' @rdname parse_summary_col
parse_summary_col <- function(.df) {
  .df <- mutate(.df,
                ofv =          extract_ofv(summary),
                param_count =  extract_param_count(summary),
                d =            extract_details(summary),
                h =            extract_heuristics(summary)
  )

  # pivot out the details and heuristics
  #https://stackoverflow.com/questions/49689927/unnest-a-list-column-directly-into-several-columns
  .df <- unnest_wider(.df, .data$d)
  .df <- unnest_wider(.df, .data$h)

  return(.df)
}


########
# internal helpers for parsing specific pieces of bbi_nonmem_summary

#' @param .s `bbi_nonmem_summary` object to parse
#' @importFrom purrr map map_dbl
#' @rdname parse_summary_col
extract_ofv <- function(.s) {
  .ofv <- map(.s, "ofv")
  .out <- map_dbl(.ofv, "ofv_no_constant", .default = NA_real_)
  return(.out)
}

#' @param .s `bbi_nonmem_summary` object to parse
#' @importFrom purrr map map_int
#' @rdname parse_summary_col
extract_param_count <- function(.s) {

  .pm <- map(.s, "parameters_data")

  .out <- map_int(.pm, function(.x) {
    num_methods <- length(.x)

    .fix <- unlist(.x[[num_methods]]$fixed)
    length(.fix) - sum(.fix)
  })

  return(.out)
}

#' @param .s `bbi_nonmem_summary` object to parse
#' @importFrom purrr map
#' @rdname parse_summary_col
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

#' @param .s `bbi_nonmem_summary` object to parse
#' @importFrom purrr map
#' @rdname parse_summary_col
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

