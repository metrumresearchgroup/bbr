
#' Tweak the initial parameter estimates
#'
#' @param .mod model object to update.
#' @param .p Percent to tweak the initial parameter estimates by. Represented as
#'   a fraction.
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
#'  the initial estimate will be set to the lower bound. The same is true for
#'  upper bounds.
#'     - e.g., `(0, 0.5, 1)` --> tweak initially, falls outside bound (`(0, 1.2, 1)`)
#'     --> set to upper bound (`(0, 1, 1)`)
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
#' }
#' @export
tweak_initial_estimates <- function(
    .mod,
    .p = 0.1,
    tweak = c("theta", "sigma", "omega"),
    digits = 3
){

  # Assertions
  test_nmrec_version(.min_version = "0.4.0")
  check_model_object(.mod, "bbi_nonmem_model")
  checkmate::assert_true(all(tweak %in% BBR_ESTIMATES_INHERIT))

  # Get initial estimates
  initial_est <- get_initial_est(.mod, flag_fixed = TRUE)

  # Rounding
  fmt_digits <- paste0("%.",digits,"G")

  # Tweak THETA
  init_thetas <- initial_est$thetas
  init_thetas$new <- withr::with_preserve_seed(tweak_values(init_thetas$init, .p))

  # Respect THETA bounds (if any) and FIXED estimates
  init_thetas <- init_thetas %>% mutate(
    new = dplyr::case_when(
      fixed == TRUE ~ init,
      !is.na(up) & new > up ~ up,
      !is.na(low) & new < low ~ low,
      TRUE ~ new
    )
  )
  # Round
  new_thetas <- init_thetas$new
  new_thetas[!is.na(new_thetas)] <- new_thetas[!is.na(new_thetas)] %>%
    signif(digits)


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

  # Return new values
  new_values <- values + (values * tweak_perc)

  return(new_values)
}
