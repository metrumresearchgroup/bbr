

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
    skip_if_old_nmrec("0.3.0.8001")
    ctl <- nmrec::read_ctl(get_model_path(MOD1))
    theta_inits <- get_theta_inits(ctl, mark_flags = "fix")

    expect_equal(
      names(theta_inits),
      c("init", "low", "up", "fixed", "record_number")
    )
    expect_true(isFALSE(all(theta_inits$fixed)))
  })

  it("fmt_record_num", {
    skip_if_old_nmrec("0.3.0.8001")
    ctl <- nmrec::read_ctl(get_model_path(MOD1))

    # Test THETA
    theta_inits <- nmrec::extract_theta(ctl)
    rec_nums <- fmt_record_num(theta_inits)
    expect_equal(length(rec_nums), length(theta_inits))
    expect_true(all(rec_nums == 1))

    # Test OMEGA (test multiple cases for added complexity)
    omega_inits <- nmrec::extract_omega(ctl)
    rec_nums <- fmt_record_num(omega_inits)
    expect_equal(length(rec_nums), sum(lower.tri(omega_inits, diag = TRUE)))
    expect_true(all(rec_nums == 1))

    # Test multiple OMEGA records
    mod2 <- read_model(file.path(MODEL_DIR_X, "1001"))
    ctl <- nmrec::read_ctl(get_model_path(mod2))
    omega_inits <- nmrec::extract_omega(ctl)
    rec_nums <- fmt_record_num(omega_inits)
    # Confirm total length
    expect_equal(length(rec_nums), sum(lower.tri(omega_inits, diag = TRUE)))
    # Confirm record numbers
    expect_equal(
      length(which(rec_nums == 1)),
      # the first column
      length(omega_inits[,1])
    )
    expect_equal(
      length(which(rec_nums == 2)),
      # lower triangular matrix minus the first column
      sum(lower.tri(omega_inits, diag = TRUE)[,-1])
    )
  })

  it("initial_estimates: integration", {
    skip_if_old_nmrec("0.3.0.8001")
    initial_est_df <- initial_estimates(MOD1, flag_fixed = TRUE)
    initial_est <- get_initial_est(MOD1, flag_fixed = TRUE)

    expect_equal(
      as.vector(initial_est$thetas$init),
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
