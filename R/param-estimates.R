
############################################
# format NONMEM output to parameter tables
############################################

#' Parses parameter estimates table
#'
#' Returns a tibble containing parameter estimates from a `bbi_{.model_type}_summary` object.
#' Details about the tibble that is returned are in the return value section below.
#'
#' @return
#' Returns a tibble with the following columns:
#'
#' * parameter_names -- Parameter name ("THETA1", "THETA2", etc.)
#' * estimate -- Parameter estimate
#' * stderr -- Standard Error
#' * random_effect_sd -- OMEGA and SIGMA elements in standard deviation/correlation format
#' * random_effect_sdse -- Standard errors to the OMEGA and SIGMA elements in standard deviation/correlation format
#' * fixed -- TRUE if parameter is fixed, FALSE otherwise
#' * diag -- TRUE if parameter is a diagonal element in random effect matrix, FALSE if off-diagonal, NA if parameter is a THETA
#'
#'
#' @seealso [param_labels()] [apply_indices()]
#' @param .summary A `bbi_{.model_type}_summary` object.
#' @export
param_estimates <- function(.summary) {
  UseMethod("param_estimates")
}

#' @describeIn param_estimates Takes `bbi_nonmem_summary` object, the output of [model_summary.bbi_nonmem_model()].
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
