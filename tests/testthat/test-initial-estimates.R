skip_if_old_nmrec("0.4.0")

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
    ctl <- nmrec::read_ctl(get_model_path(MOD1))
    theta_inits <- get_theta_inits(ctl, mark_flags = "fix")

    expect_equal(
      names(theta_inits),
      c("init", "low", "up", "fixed", "record_number")
    )
    expect_true(isFALSE(all(theta_inits$fixed)))
  })

  it("fmt_record_num", {
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
    mod_c <- read_model(file.path(MODEL_DIR_X, "1001"))
    initial_est_df <- initial_estimates(mod_c, flag_fixed = TRUE)
    initial_est <- get_initial_est(mod_c, flag_fixed = TRUE)

    expect_equal(
      as.vector(initial_est$thetas$init),
      initial_est_df %>% dplyr::filter(record_type == "theta") %>% dplyr::pull(init)
    )

    lower_tri_omegas <- matrix_to_df(initial_est$omegas, type = "omega") %>%
      dplyr::arrange(.data$row, .data$col) %>% dplyr::pull(init)
    expect_equal(
      lower_tri_omegas[!is.na(lower_tri_omegas)], # Filter out non-specified values
      initial_est_df %>% dplyr::filter(record_type == "omega") %>% dplyr::pull(init)
    )

    lower_tri_sigmas <- matrix_to_df(initial_est$sigmas, type = "sigma") %>%
      dplyr::arrange(.data$row, .data$col) %>% dplyr::pull(init)
    expect_equal(
      lower_tri_sigmas[!is.na(lower_tri_sigmas)], # Filter out non-specified values
      initial_est_df %>% dplyr::filter(record_type == "sigma") %>% dplyr::pull(init)
    )
  })

  it("initial_estimates: SAME blocks", {
    mod_c <- read_model(file.path(MODEL_DIR_X, "acop-iov"))
    initial_est_df <- initial_estimates(mod_c, flag_fixed = TRUE)
    initial_est <- get_initial_est(mod_c, flag_fixed = TRUE)

    # Check dimensions and NA values
    expect_equal(dim(initial_est$omegas), c(62, 62))
    # only 4 values are actually specified in ctl file
    expect_equal(sum(!is.na(diag(initial_est$omegas))), 4)
    expect_equal(sum(is.na(diag(initial_est$omegas))), 58)
    # same check for exported functions (NAs already filtered out)
    expect_equal(nrow(initial_est_df %>% dplyr::filter(record_type == "omega")), 4)
    # Check record numbers found (skips 3 and 5, which are SAME blocks)
    expect_equal(unique(initial_est_df$record_number), c(1,2,4))

    # Check values
    lower_tri_omegas <- matrix_to_df(initial_est$omegas, type = "omega") %>%
      dplyr::arrange(.data$row, .data$col) %>% dplyr::pull(init)
    expect_equal(
      lower_tri_omegas[!is.na(lower_tri_omegas)], # Filter out non-specified values
      initial_est_df %>% dplyr::filter(record_type == "omega") %>% dplyr::pull(init)
    )
  })

  it("initial_estimates: warns when using old priors", {
    mod_c <- read_model(file.path(MODEL_DIR_X, "example2_saemimp"))
    expect_warning(
      initial_est_df <- initial_estimates(mod_c),
      "This model appears to be using"
    )
    # Check for multiple theta and omega records (second ones are priors)
    thetas_df <- initial_est_df %>% dplyr::filter(record_type == "theta")
    oemgas_df <- initial_est_df %>% dplyr::filter(record_type == "omega")

    # Here, the second theta record is actually equivalent to `OMEGAPD` (not a theta prior)
    # i.e. the two theta records are not equal in length, which would be the case if using `THETAP`
    expect_equal(dplyr::n_distinct(thetas_df$record_number), 2)
    expect_equal(thetas_df %>% filter(record_number == 1) %>% nrow(), 11)
    expect_equal(thetas_df %>% filter(record_number == 2) %>% nrow(), 1)

    # Here, the second omega record is a prior, so the lengths should be equivalent
    expect_equal(dplyr::n_distinct(oemgas_df$record_number), 2)
    expect_equal(oemgas_df %>% filter(record_number == 1) %>% nrow(), 10)
    expect_equal(oemgas_df %>% filter(record_number == 2) %>% nrow(), 10)
  })
})
