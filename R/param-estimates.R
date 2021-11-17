
############################################
# format NONMEM output to parameter tables
############################################

#' Parses parameter estimates table
#'
#' Returns a tibble containing parameter estimates from a
#' `bbi_{.model_type}_summary` object. Details about the tibble that is returned
#' are in the return value section below. Note: if you need to pull in the
#' parameter estimates for a large number of NONMEM models at once, consider
#' using [param_estimates_batch()] instead.
#'
#' @details
#' Note that **Bayesian methods are not yet supported** by this function. Creating a parameter table
#' like this for Bayesian estimation methods requires a different approach, which has not yet been
#' implemented. If the final estimation method of the input model is Bayesian, a `not implemented`
#' error will be thrown.
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
#' * shrinkage -- Shrinkage for final estimation method, using SD parameterization (for NONMEM this is ETAshrinkSD and EPSshrinkSD).
#' The shrinkage using Variance Parameterization, as well as for other estimation methods and multiple subpops if present,
#' can be found in `bbi_nonmem_summary$shrinkage_details`.
#'
#'
#' @seealso [param_estimates_batch()], [model_summary()], [param_labels()], [apply_indices()]
#' @param .summary A `bbi_{.model_type}_summary` object.
#' @export
param_estimates <- function(.summary) {
  UseMethod("param_estimates")
}

#' @describeIn param_estimates Takes `bbi_nonmem_summary` object, the output of [model_summary.bbi_nonmem_model()].
#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map_at map_depth map_lgl
#' @importFrom rlang list2
#' @importFrom stringr str_detect
#' @export
param_estimates.bbi_nonmem_summary <- function(.summary) {
  param_names <- .summary[[SUMMARY_PARAM_NAMES]]

  # if Bayesian method (includes NUTS) do not return df because it is incorrect and misleading
  est_methods <- .summary[[SUMMARY_DETAILS]][[SUMMARY_EST_METHOD]]
  if (any(str_detect(est_methods, "Bayesian"))) {
    stop(glue("{PARAM_BAYES_ERR_MSG} `.summary` has final estimation method: {est_methods}"), call. = FALSE)
  }

  summary_vars <- with(
    .summary[[SUMMARY_PARAM_DATA]][[length(.summary[[SUMMARY_PARAM_DATA]])]],
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
  param_df[[SUMMARY_PARAM_DIAG]] <- map_lgl(param_df[[SUMMARY_PARAM_NAMES]], is_diag)

  param_df <- add_param_shrinkage(param_df, .summary)

  return(param_df)
}


#' Check if diagonal index or not
#'
#' Private helper to unpack a matrix index string like '(3,3)' is for a diagonal (i.e. if the numbers are the same)
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


#' Add shrinkage details to param table
#'
#' Private helper to unpack shrinkage details and create a column on the input tibble
#' with the shrinkage assigned to the relevant parameters.
#' @param .param_df The parameter estimates table containing, at minimum `parameter_names` and `diag` columns.
#' @param .summary A `bbi_nonmem_summary` object.
#' @importFrom dplyr select filter left_join bind_rows
#' @importFrom stringr str_detect
#' @keywords internal
add_param_shrinkage <- function(.param_df, .summary) {

  # extract shrinkage for final estimation method
  shk <- .summary[[SUMMARY_SHRINKAGE]]
  shk <- shk[[length(shk)]]

  if (is.null(shk)) {
    .param_df[[SUMMARY_PARAM_SHRINKAGE]] <- NA_real_
    return(.param_df)
  }

  # check for mixture model with multiple subpops
  if (length(shk) != 1) {
    warning(paste(
      glue("When using `param_estimates()` with a mixture model (multiple subpops) the `{SUMMARY_PARAM_SHRINKAGE}` column will be all `NA`."),
      glue("Users can manually extract shrinkage for each subpop from the `{SUMMARY_SHRINKAGE}` element of the `bbi_nonmem_summary` object."),
      sep = "\n"
      ))
    .param_df[[SUMMARY_PARAM_SHRINKAGE]] <- NA_real_
    return(.param_df)
  }

  # select the first (and only) subpop
  shk <- shk[[1]]

  # filter to only omega and sigma diagonal elements
  diag_df <- .param_df %>%
                filter(.data[[SUMMARY_PARAM_DIAG]]) %>%
                select({{ SUMMARY_PARAM_NAMES }}, {{ SUMMARY_PARAM_DIAG }})

  # parse shrinkage for OMEGA diagonals
  omega_df <- filter(diag_df, str_detect(.data[[SUMMARY_PARAM_NAMES]], "OMEGA"))
  omega_shk <- shk[[SUMMARY_SHRINKAGE_OMEGA]]
  if (nrow(omega_df) != length(omega_shk)) {
    stop(paste(
      glue("Found {nrow(omega_df)} OMEGA diagonals in parameter table and {length(omega_shk)} elements in `.summary[['{SUMMARY_SHRINKAGE}']][['{SUMMARY_SHRINKAGE_OMEGA}']]`."),
      "Summary object may be malformed."
      ))
  }
  omega_df[[SUMMARY_PARAM_SHRINKAGE]] <- omega_shk

  # parse shrinkage for SIGMA diagonals
  sigma_df <- filter(diag_df, str_detect(.data[[SUMMARY_PARAM_NAMES]], "SIGMA"))
  sigma_shk <- shk[[SUMMARY_SHRINKAGE_SIGMA]]
  if (nrow(sigma_df) != length(sigma_shk)) {
    stop(paste(
      glue("Found {nrow(sigma_df)} SIGMA diagonals in parameter table and {length(sigma_shk)} elements in `.summary[['{SUMMARY_SHRINKAGE}']][['{SUMMARY_SHRINKAGE_SIGMA}']]`."),
      "Summary object may be malformed."
    ))
  }
  sigma_df[[SUMMARY_PARAM_SHRINKAGE]] <- sigma_shk

  # combine shrinkage tibbles with original .param_df
  # note the left join because all THETA rows _should_ have NA values for shrinkage
  shk_df <- bind_rows(omega_df, sigma_df)
  out_df <- left_join(.param_df, shk_df, by = c(SUMMARY_PARAM_NAMES, SUMMARY_PARAM_DIAG))

  return(out_df)
}
