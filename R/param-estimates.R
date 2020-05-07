
############################################
# format NONMEM output to parameter tables
############################################

#' S3 generic for parsing parameter estimate table
#' @param .summary generic summary object
#' @export
#' @rdname param_estimates
param_estimates <- function(.summary) {
  UseMethod("param_estimates")
}

#' S3 dispatch for parsing `bbi_nonmem_summary` object into parameter estimate table
#' @param .summary `bbi_nonmem_summary` object
#' @importFrom tibble tibble
#' @export
#' @rdname param_estimates
param_estimates.bbi_nonmem_summary <- function(.summary) {
  num_methods <- length(.summary$parameters_data)
  param_names <- .summary$parameter_names
  param_estimates <- .summary$parameters_data[[num_methods]]$estimates

  param_df <- tibble::tibble(
    names = unlist(param_names),
    estimate = unlist(param_estimates),
    stderr = unlist(.summary$parameters_data[[num_methods]]$std_err) %||% NA_real_,
    random_effect_sd = c(
      rep(NA, length(param_names$theta)),
      unlist(.summary$parameters_data[[num_methods]]$random_effect_sd)) %||% NA_real_,
    random_effect_sdse = c(
      rep(NA, length(param_names$theta)),
      unlist(.summary$parameters_data[[num_methods]]$random_effect_sdse)) %||% NA_real_,
    fixed = unlist(.summary$parameters_data[[num_methods]]$fixed)
  )

  # create boolean column for whether each row is a diagonal
  param_df$diag <- map_lgl(param_df$names, is_diag)

  return(param_df)
}

#' Check if diagonal index or not
#'
#' Private helper to unpack an matrix index string like '(3,3)' is for a diagonal (i.e. if the numbers are the same)
#' @param .name A character scaler containing an index string
#' @importFrom stringr str_replace_all str_split
is_diag <- function(.name) {
  .ind <- .name %>%
    str_replace_all(glue("^.*\\(|\\)"), "") %>%
    str_split(",") %>% unlist()

  if (length(.ind) == 1) {
    return(invisible(NA))
  }

  return(.ind[1] == .ind[2])
}
