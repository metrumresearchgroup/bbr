
############################################
# format NONMEM output to parameter tables
############################################

#' Parses parameter estimates table
#'
#' Returns a tibble containing parameter estimates from a model.
#' Currently can only take a `bbi_{.model_type}_summary` object, as output from `model_summary()`.
#' @seealso `param_labels()` `apply_indices()`
#' @param .summary `bbi_{.model_type}_summary`, `bbi_summary_list`, or `bbi_summary_log_df` object
#' @export
param_estimates <- function(.summary) {
  UseMethod("param_estimates")
}

#' @describeIn param_estimates Takes `bbi_nonmem_summary` object.
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

  param_df[["diag"]] <- map_lgl(param_df[[SUMMARY_PARAM_NAMES]], is_diag)

  return(param_df)
}



param_estimates.bbi_summary_list <- function(.summary) {

  param_df <- map_df(.summary, function(.s) {
    if(!is.na(.s[[SL_ERROR]])){
      warning(paste("Missing summary for", .s[[ABS_MOD_PATH]], "\n", .s[[SL_ERROR]]))
      return(invisible())
    }
    .s[[SL_SUMMARY]] %>%
      param_estimates() %>%
      mutate(!!ABS_MOD_PATH := .s[[ABS_MOD_PATH]]) %>%
      select(.data[[ABS_MOD_PATH]], .data[[SUMMARY_PARAM_NAMES]], .data[["estimate"]])
  })

  # param_df <- param_df %>%
  #   pivot_wider(names_from = .data$names, values_from = .data$estimate)

  return(param_df)
}

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
