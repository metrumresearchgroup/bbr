
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
    tweak = c("theta", "sigma", "omega"),
    digits = 3
){

  # Assertions
  test_nmrec_version(.min_version = "0.3.0.8000")
  check_model_object(.mod, "bbi_nonmem_model")
  checkmate::assert_true(all(tweak %in% BBR_ESTIMATES_INHERIT))

  # Get initial estimates
  initial_est <- get_initial_est(.mod, flag_fixed = TRUE)

  # Rounding
  fmt_digits <- paste0("%.",digits,"G")

  # Tweak THETA
  init_thetas <- initial_est$thetas
  init_thetas$new <- withr::with_preserve_seed(tweak_values(init_thetas$init, .p))
  # Ignore fixed values
  init_thetas <- init_thetas %>% dplyr::mutate(
    new = ifelse(fixed == TRUE, init, new)
  )
  # Respect THETA bounds (if any)
  init_thetas <- adjust_tweaked_theta(init_thetas, digits)
  new_thetas <- init_thetas$new %>% signif(digits)


  # Tweak OMEGA
  init_omegas <- initial_est$omegas
  fixed_omegas <- attr(init_omegas, "nmrec_flags")$fixed
  fixed_omegas[is.na(fixed_omegas)] <- TRUE
  new_omegas <- init_omegas[!fixed_omegas]
  # Tweak values
  new_omegas <- withr::with_preserve_seed(tweak_values(new_omegas, .p))
  init_omegas[!fixed_omegas] <- new_omegas %>% signif(digits)


  # Tweak SIGMA
  init_sigmas <- initial_est$sigmas
  fixed_sigmas <- attr(init_sigmas, "nmrec_flags")$fixed
  fixed_sigmas[is.na(fixed_sigmas)] <- TRUE
  new_sigmas <- init_sigmas[!fixed_sigmas]
  # Tweak values
  new_sigmas <- withr::with_preserve_seed(tweak_values(new_sigmas, .p))
  init_sigmas[!fixed_sigmas] <- new_sigmas %>% signif(digits)


  # nmrec model objects
  mod_path <- get_model_path(.mod)
  mod_lines <- nmrec::read_ctl(mod_path)

  # Update THETA Block
  if("theta" %in% tweak && !rlang::is_empty(new_thetas)){
    nmrec::set_theta(
      mod_lines, values = new_thetas, bounds = "keep",
      fmt = fmt_digits
    )
  }

  # Update OMEGA Block
  if("omega" %in% tweak && !rlang::is_empty(init_omegas)){
    nmrec::set_omega(
      mod_lines, values = init_omegas, representation = "reset",
      fmt = fmt_digits
    )
  }

  # Update SIGMA Block
  if("sigma" %in% tweak && !rlang::is_empty(init_sigmas)){
    nmrec::set_sigma(
      mod_lines, values = init_sigmas, representation = "reset",
      fmt = fmt_digits
    )
  }

  # Write out mod_lines to model
  nmrec::write_ctl(mod_lines, mod_path)

  return(.mod)
}


tweak_values <- function(values, .p){

  # Sample percentages & Preserve seed if set
  tweak_perc <- stats::runif(length(values), -.p, .p)

  # Tweak values
  new_values <- values + (values * tweak_perc)

  # Return rounded values
  return(new_values)
}


#' Adjust initial theta estimates to be within bounds
#'
#' @param init_thetas table of theta estimates and bounds
#' @param digits number of significant figures to round to
#'
#' @keywords internal
adjust_tweaked_theta <- function(init_thetas, digits){

  fmt_digits <- paste0("%.",digits,"G")

  # Calculate range of the bounds
  init_thetas$upper_diff <- init_thetas$up - init_thetas$init
  init_thetas$lower_diff <- init_thetas$init - init_thetas$low

  # Initialize formatted version of initial value and bounds
  init_thetas_adj <- init_thetas %>% mutate(
    new_fmt = sprintf(fmt_digits, new),
    up_fmt = sprintf(fmt_digits, up),
    low_fmt = sprintf(fmt_digits, low)
  )

  # Set to `bound_perc` (e.g, 90%) of the way between init and bound
  adjust_theta_bounds <- function(init_thetas, bound_perc = 0.9, fmt_digits){
    init_thetas_adj <- init_thetas_adj %>% mutate(
      new = dplyr::case_when(
        !is.na(up) & new_fmt >= up_fmt ~ (init + upper_diff*bound_perc),
        !is.na(low) & new_fmt <= low_fmt ~ (init - lower_diff*bound_perc),
        TRUE ~ new
      ),
      new_fmt = sprintf(fmt_digits, new),
      up_fmt = sprintf(fmt_digits, up),
      low_fmt = sprintf(fmt_digits, low)
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

  # Remove extra columns
  init_thetas_adj <- init_thetas_adj %>%
    dplyr::select(-tidyselect::ends_with(c("_fmt", "_diff")))

  return(init_thetas_adj)
}
