
tweak_initial_estimates <- function(
    .mod,
    .p = 0.1,
    tweak = c("theta", "sigma", "omega"),
    digits = 2
){

  test_nmrec_version(.min_version = "0.3.0") # TODO: increment version
  check_model_object(.mod, "bbi_nonmem_model")
  checkmate::assert_true(all(tweak %in% BBR_ESTIMATES_INHERIT))

  # Get initial estimates
  initial_est <- get_initial_est(.mod, flag_fixed = TRUE)

  # Rounding
  fmt_digits <- paste0("%.",digits,"G")

  # Tweak THETA
  init_thetas <- initial_est$thetas
  tweak_perc <- stats::runif(length(init_thetas$init), -.p, .p)
  init_thetas$new <- init_thetas$init + (init_thetas$init * tweak_perc)

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
  tweak_perc <- runif(length(new_omegas), -.p, .p)
  new_omegas <- new_omegas + (new_omegas * tweak_perc)
  init_omegas[!fixed_omegas] <- new_omegas %>% signif(digits)


  # Tweak SIGMA
  init_sigmas <- initial_est$sigmas
  fixed_sigmas <- attr(init_sigmas, "nmrec_flags")$fixed
  fixed_sigmas[is.na(fixed_sigmas)] <- TRUE
  new_sigmas <- init_sigmas[!fixed_sigmas]
  tweak_perc <- runif(length(new_sigmas), -.p, .p)
  new_sigmas <- new_sigmas + (new_sigmas * tweak_perc)
  init_sigmas[!fixed_sigmas] <- new_sigmas %>% signif(digits)


  # nmrec model objects
  mod_path <- get_model_path(.mod)
  mod_lines <- nmrec::read_ctl(mod_path)

  # Update THETA Block
  if("theta" %in% tweak){
    nmrec::set_theta(
      mod_lines, values = new_thetas, bounds = "keep",
      fmt = fmt_digits
    )
  }

  # Update OMEGA Block
  if("omega" %in% tweak){
    nmrec::set_omega(
      mod_lines, values = init_omegas, representation = "reset",
      fmt = fmt_digits
    )
  }

  # Update SIGMA Block
  if("sigma" %in% tweak){
    nmrec::set_sigma(
      mod_lines, values = init_sigmas, representation = "reset",
      fmt = fmt_digits
    )
  }

  # Write out mod_lines to model
  nmrec::write_ctl(mod_lines, mod_path)

  return(.mod)
}
