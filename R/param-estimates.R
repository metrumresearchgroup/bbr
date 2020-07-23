
############################################
# format NONMEM output to parameter tables
############################################

#' Parses parameter estimates table
#'
#' Returns a tibble containing parameter estimates from a model.
#' Currently can only take a `bbi_{.model_type}_summary` object, as output from `model_summary()`.
#' @seealso `param_labels()` `apply_indices()`
#' @param .summary `bbi_{.model_type}_summary` object
#' @export
param_estimates <- function(.summary) {
  UseMethod("param_estimates")
}

#' @describeIn param_estimates Takes `bbi_nonmem_summary` object.
#' @importFrom tibble tibble
#' @export
param_estimates.bbi_nonmem_summary <- function(.summary) {
  num_methods <- length(.summary[["parameters_data"]])
  param_names <- .summary[["parameter_names"]]

  summary_vars <- with(
    .summary[["parameters_data"]][[num_methods]],
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
    list(names = param_names),
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

  param_df[["diag"]] <- map_lgl(param_df[["names"]], is_diag)

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



### WILL REPLACE THIS WITH LIBRARY FUNCTION IN rbabylon
### needs to become new s3 dispatches:
### * param_estimates.bbi_summary_list
### * param_estimates.bbi_summary_log_df
get_param_estimates_new <- function(.df) {

  # should this be some dplyr rowwise thing?
  # or should I maybe just pull these things back into a bbi_summary_list style list and then pass to that dispatch?
  # this feels awkward having to use abs_paths[.i] and stuff
  sum_list <- .df[[SL_SUMMARY]]
  abs_paths <- .df[[ABS_MOD_PATH]]

  param_df <- imap_dfr(sum_list, function(.mod, .i) {
    if(!is.null(.mod[[SL_ERROR]])){
      warning(paste("Missing summary for", abs_paths[.i], "\n", .mod[[SL_ERROR]]))
      return(invisible())
    }
    .mod %>%
      param_estimates() %>%
      mutate(!!ABS_MOD_PATH := abs_paths[.i]) %>%
      #mutate(i = .i) # haven't tried this yet but if it works can replace ^
      select(.data$ABS_MOD_PATH, .data$names, .data$estimate)

  })

  param_df <- param_df %>%
    pivot_wider(names_from = .data$names, values_from = .data$estimate)

  return(param_df)
}
