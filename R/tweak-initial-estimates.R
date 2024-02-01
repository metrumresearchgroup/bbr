
#' Tweak the initial parameter estimates
#'
#' @param .mod model object to update.
#' @param .p Percent to tweak the initial parameter estimates by. Represented as
#'   a decimal.
#' @param tweak type of estimates to tweak in the model. Defaults to
#'   updating all of THETA, SIGMA, and OMEGA records.
#' @param digits Number of significant digits to round estimates to.
#'
#' @details
#'
#' In the following cases, the initial estimate will *not* be updated:
#'  - **Individual** `FIXED` `THETA` parameters
#'     - e.g., `$THETA 1.2 FIX 1.5 0.2` --> would only skip the first value
#'  - **Individual** `FIXED` `OMEGA` & `SIGMA` parameters for *diagonal* matrices
#'     - e.g., `$OMEGA 0 FIX 1` --> would only skip the first value
#'  - **Full** `FIXED` `OMEGA` & `SIGMA` *block* matrices
#'     - e.g., `$OMEGA BLOCK(2) 0.1 0.001 0.1 FIX` --> would skip the full `OMEGA` record
#'  - `THETA` parameters with no initial estimate
#'     - e.g., `$THETA (0,,1)`
#'
#'  For bounded `THETA` estimates:
#'   - If an initial `THETA` has bounds **and** an initial estimate
#'  (e.g., `(0, 0.5, 1)`, `(0,1)`), the bounds will be respected when sampling
#'  a percent to tweak by. If the tweaked value would fall below the lower bound,
#'  the initial estimate will be set to 90% of the way between the initial value
#'  and lower bound. If *after rounding*, the value would still not be within the
#'  bounds, the difference between the initial value and the bound will be
#'  iteratively reduced  until it is. The same is true for upper bounds.
#'     - e.g., `(0, 0.5, 1)` --> tweak initially, falls outside bound (`(0, 1.2, 1)`)
#'     --> set to value within bounds (first iteration: `(1-0.5)*0.9)`:  (`(0, 0.45, 1)`)
#'
#'
#' @examples
#' \dontrun{
#' base_mod <- read_model(file.path(MODEL_DIR, "1"))
#'
#' mod2 <- copy_model_from(base_mod, "mod2") %>%
#'   tweak_initial_estimates(.p = 0.2)
#'
#' # This function may be paired with `inherit_param_estimates()`:
#' mod2 <- copy_model_from(base_mod, "mod2") %>%
#'   inherit_param_estimates() %>% tweak_initial_estimates(.p = 0.2)
#'
#' # If you want to set the seed for reproducible results:
#' mod2 <- withr::with_seed(1234, {
#'    tweak_initial_estimates(mod2, .p = 0.2, digits = 3)
#' })
#'
#' }
#' @export
tweak_initial_estimates <- function(
    .mod,
    .p = 0.1,
    tweak = c("theta", "omega", "sigma"),
    digits = 3
){

  # Assertions
  test_nmrec_version(.min_version = "0.3.0.8001")
  check_model_object(.mod, "bbi_nonmem_model")
  checkmate::assert_true(all(tweak %in% BBR_ESTIMATES_INHERIT))

  # Get initial estimates
  initial_est <- get_initial_est(.mod, flag_fixed = TRUE)

  # Get matrix types
  mat_types <- get_matrix_types(.mod)

  # Rounding
  fmt_digits <- paste0("%.",digits,"G")

  # nmrec model objects
  mod_path <- get_model_path(.mod)
  mod_lines <- nmrec::read_ctl(mod_path)

  # Tweak each record type
  # Note: the `is_empty` check is mainly for models that dont specify SIGMA
  # records. This allows users to not need to change the `tweak` argument if the
  # record type is missing from the control stream file.
  # It is also useful for testing.

  if("theta" %in% tweak && !rlang::is_empty(initial_est$thetas)){
    # Tweak THETA
    new_thetas <- tweak_thetas(initial_est$thetas, .p, digits)
    # Update THETA Block
    nmrec::set_theta(
      mod_lines, values = new_thetas, bounds = "keep",
      fmt = fmt_digits
    )
  }

  if("omega" %in% tweak && !rlang::is_empty(initial_est$omegas)){
    # Set key attributes
    attr(initial_est$omegas, "record_type") <- "omega"
    attr(initial_est$omegas, "mat_types") <- mat_types %>% filter(record_type == "omega")
    # Tweak OMEGA
    new_omegas <- tweak_matrix(initial_est$omegas, .p, digits)
    # Update OMEGA Block
    nmrec::set_omega(
      mod_lines, values = new_omegas, representation = "reset",
      fmt = fmt_digits
    )
  }

  if("sigma" %in% tweak && !rlang::is_empty(initial_est$sigmas)){
    # Set key attributes
    attr(initial_est$sigmas, "record_type") <- "sigma"
    attr(initial_est$sigmas, "mat_types") <- mat_types %>% filter(record_type == "sigma")
    # Tweak SIGMA
    new_sigmas <- tweak_matrix(initial_est$sigmas, .p, digits)
    # Update SIGMA Block
    nmrec::set_sigma(
      mod_lines, values = new_sigmas, representation = "reset",
      fmt = fmt_digits
    )
  }

  # Write out mod_lines to model
  nmrec::write_ctl(mod_lines, mod_path)

  return(.mod)
}

#' Tweak values
#'
#' @param values vector of values to tweak
#' @inheritParams tweak_initial_estimates
tweak_values <- function(values, .p){

  # Sample percentages & Preserve seed if set
  tweak_perc <- stats::runif(length(values), -.p, .p)

  # Tweak values
  new_values <- values + (values * tweak_perc)

  # Return tweaked values
  return(new_values)
}




#' Tweak theta estimates while respecting bounds
#'
#' @param init_thetas table of theta estimates and bounds
#' @inheritParams tweak_initial_estimates
#'
#' @keywords internal
tweak_thetas <- function(init_thetas, .p, digits){

  init_thetas$new <- withr::with_preserve_seed(tweak_values(init_thetas$init, .p))

  # Ignore fixed values
  init_thetas <- init_thetas %>% dplyr::mutate(
    new = ifelse(fixed == TRUE, .data$init, .data$new)
  )

  fmt_digits <- paste0("%.",digits,"G")

  ### Respect Bounds ###
  # Calculate range of the bounds
  init_thetas$upper_diff <- init_thetas$up - init_thetas$init
  init_thetas$lower_diff <- init_thetas$init - init_thetas$low

  # Initialize formatted version of initial value and bounds
  init_thetas_adj <- init_thetas %>% mutate(
    new_fmt = sprintf(fmt_digits, .data$new),
    up_fmt = sprintf(fmt_digits, .data$up),
    low_fmt = sprintf(fmt_digits, .data$low)
  )

  # Set to `bound_perc` (e.g, 90%) of the way between init and bound
  adjust_theta_bounds <- function(init_thetas, bound_perc = 0.9, fmt_digits){
    init_thetas_adj <- init_thetas_adj %>% mutate(
      new = dplyr::case_when(
        !is.na(up) & new_fmt >= up_fmt ~ (init + upper_diff*bound_perc),
        !is.na(low) & new_fmt <= low_fmt ~ (init - lower_diff*bound_perc),
        TRUE ~ new
      ),
      new_fmt = sprintf(fmt_digits, .data$new),
      up_fmt = sprintf(fmt_digits, .data$up),
      low_fmt = sprintf(fmt_digits, .data$low)
    )
    return(init_thetas_adj)
  }

  check_bounds <- function(init_thetas_adj){
    any(init_thetas_adj$new_fmt <= init_thetas_adj$low_fmt) ||
      any(init_thetas_adj$new_fmt >= init_thetas_adj$up_fmt)
  }

  # Pull back initial value until it's:
  # A) within the bounds
  # B) will stay within the bounds after being rounded
  for(perc.i in c(0.9, 0.7, 0.5, 0.3)){
    if(check_bounds(init_thetas_adj)){
      init_thetas_adj <- adjust_theta_bounds(
        init_thetas_adj, bound_perc = perc.i, fmt_digits = fmt_digits
      )
      # Exit loop when values are within bounds
      if(!check_bounds(init_thetas_adj)) break
    }
  }

  # Final check to see if any initial values still overlap with bounds
  # If so, set to original value
  # Triggering this means the bounded range is small and likely conflicts with
  # the set number of significant figures e.g., $THETA (1.99, 2.0, 2.01)
  if(check_bounds(init_thetas_adj)){
    init_thetas_adj <- init_thetas_adj %>% mutate(
      new = dplyr::case_when(
        !is.na(up) & new_fmt == up_fmt ~ init,
        !is.na(low) & new_fmt == low_fmt ~ init,
        TRUE ~ new
      )
    )
  }

  new_thetas <- init_thetas_adj$new %>% signif(digits)

  return(new_thetas)
}


#' Tweak an `OMEGA` or `SIGMA` record
#'
#' @param init_mat matrix of initial `OMEGA` or `SIGMA` estimates. Should include
#' several attributes, including `nmrec_flags` and `nmrec_record_size`.
#'
#' @keywords internal
tweak_matrix <- function(init_mat, .p, digits){
  fixed_mat <- attr(init_mat, "nmrec_flags")$fixed
  fixed_mat[is.na(fixed_mat)] <- TRUE
  new_values <- init_mat[!fixed_mat]
  # Tweak values
  new_values <- withr::with_preserve_seed(tweak_values(new_values, .p))
  new_mat <- init_mat
  new_mat[!fixed_mat] <- new_values %>% signif(digits)

  # Check for positive-definiteness
  new_mat <- validate_matrix_pd(new_mat, digits)

  # If matrix cannot be made to be positive-definite with specified digits,
  # reset to original matrix and warn
  if(is.null(new_mat)){
    record_type <- attr(full_mat, "record_type")
    rlang::warn(
      c(
        "!" = paste(
          glue("Tweaked {record_type} record(s) could not be made positive-definite"),
          "while respecting the user-specified `digits`."
        ),
        "i" = paste(
          "Resetting to original matrix.",
          "Consider increasing `digits`, or not tweaking this record type."
        )
      )
    )
    new_mat <- init_mat
  }

  return(new_mat)
}
