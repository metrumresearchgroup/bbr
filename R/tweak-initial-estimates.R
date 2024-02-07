
#' Tweak the initial parameter estimates
#'
#' @param .mod model object to update.
#' @param .p Percent to tweak the initial parameter estimates by. Represented as
#'   a decimal.
#' @param tweak type of estimates to tweak in the model. Only `"theta"` is
#'   currently supported, though `"omega"` and `"sigma"` will be options at a later date.
#' @param digits Number of significant digits to round estimates to.
#'
#' @details
#'
#' In the following cases, the initial estimate will *not* be updated:
#'  - **Individual** `FIXED` `THETA` parameters
#'     - e.g., `$THETA 1.2 FIX 1.5 0.2` --> would only skip the first value
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
#'     --> set to value within bounds (first iteration: `0.5 + (1-0.5)*0.9`:  (`(0, 0.95, 1)`)
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
    tweak = c("theta"),
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

  # Function that attempts to set to `bound_perc` (e.g, 90%) of the way between
  # init and bound, while respecting user-specified `digits` (rounding).
  # This is used when the tweaked value falls outside, or is equal to the bounds.
  adjust_theta_bounds <- function(thetas_df, bound_perc, fmt){
    thetas_df <- thetas_df %>% mutate(
      new = dplyr::case_when(
        !is.na(up) & new_fmt >= up_fmt ~ (init + upper_diff*bound_perc),
        !is.na(low) & new_fmt <= low_fmt ~ (init - lower_diff*bound_perc),
        TRUE ~ new
      ),
      new_fmt = sprintf(fmt, .data$new),
      up_fmt = sprintf(fmt, .data$up),
      low_fmt = sprintf(fmt, .data$low)
    )
    return(thetas_df)
  }

  # Function to check if the tweaked value is within the bounds
  check_bounds <- function(thetas_df){
    any(thetas_df$new_fmt <= thetas_df$low_fmt) ||
      any(thetas_df$new_fmt >= thetas_df$up_fmt)
  }

  # Pull back initial value until it's:
  # A) within the bounds
  # B) will stay within the bounds after being rounded
  for(perc.i in c(0.9, 0.7, 0.5, 0.3)){
    if(check_bounds(init_thetas_adj)){
      init_thetas_adj <- adjust_theta_bounds(
        init_thetas_adj, bound_perc = perc.i, fmt = fmt_digits
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
