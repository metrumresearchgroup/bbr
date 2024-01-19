

describe("initial_estimates", {

  it("matrix_to_df", {
    initial_est <- get_initial_est(MOD1)
    omega_df <- matrix_to_df(initial_est$omegas)

    expect_equal(
      omega_df$init,
      initial_est$omegas[lower.tri(initial_est$omegas, diag = TRUE)]
    )
  })

  it("get_theta_inits", {
    skip_if_old_nmrec("0.3.0.8000")
    ctl <- nmrec::read_ctl(get_model_path(MOD1))
    theta_inits <- get_theta_inits(ctl, mark_flags = "fix")

    expect_equal(names(theta_inits), c("init", "low", "up", "fixed"))
    expect_true(isFALSE(all(theta_inits$fixed)))
  })

  it("initial_estimates: integration", {
    skip_if_old_nmrec("0.3.0.8000")
    initial_est_df <- initial_estimates(MOD1, flag_fixed = TRUE)
    initial_est <- get_initial_est(MOD1, flag_fixed = TRUE)

    expect_equal(
      initial_est$thetas$init,
      initial_est_df %>% dplyr::filter(record_type == "theta") %>% dplyr::pull(init)
    )

    lower_tri_omegas <- matrix_to_df(initial_est$omegas, type = "omega") %>%
      dplyr::pull(init)
    expect_equal(
      lower_tri_omegas[!is.na(lower_tri_omegas)], # Filter out non-specified values
      initial_est_df %>% dplyr::filter(record_type == "omega") %>% dplyr::pull(init)
    )

    lower_tri_sigmas <- matrix_to_df(initial_est$sigmas, type = "sigma") %>%
      dplyr::pull(init)
    expect_equal(
      lower_tri_sigmas[!is.na(lower_tri_sigmas)], # Filter out non-specified values
      initial_est_df %>% dplyr::filter(record_type == "sigma") %>% dplyr::pull(init)
    )
  })
})
