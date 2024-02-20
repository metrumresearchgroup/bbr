

# Note that most of the example records came from the OMEGA help page in the
# NONMEM docs: nm-help/nm750/html/$omega.htm
describe("matrix-handling", {
  skip_if_old_nmrec("0.4.0")

  it("supported matrix options", {
    test_case <- list(
      case = "supported matrix options",
      input_ctl = "
      ; The following describe the same block (within rounding errors):
      $OMEGA BLOCK(2) ; or $OMEGA VARIANCE COVARIANCE BLOCK(2)
      0.64
      -0.24 0.581
      $OMEGA STANDARD BLOCK(2)
      0.8
      -0.24 0.762
      $OMEGA STANDARD CORRELATION BLOCK(2)
      0.8
      -0.394 0.762
      $OMEGA VARIANCE CORRELATION BLOCK(2)
      0.64
      -0.394 0.581
      $OMEGA CHOLESKY BLOCK(2)
      0.8
      -0.3 0.7
      "
    )

    # Create fake model
    mod_tweak <- do.call(make_fake_mod, test_case)
    on.exit(delete_models(mod_tweak, .tags = NULL, .force = TRUE))

    # Confirm extracted matrix options
    mat_opts <- get_matrix_opts(mod_tweak) %>% dplyr::filter(record_type == "omega")
    expect_equal(
      mat_opts$diag,
      c("variance", "standard", "standard", "variance", "cholesky")
    )
    expect_equal(
      mat_opts$off_diag,
      c("covariance", "covariance", "correlation", "correlation", "cholesky")
    )

    # Get initial estimates
    initial_est <- get_initial_est(mod_tweak, flag_fixed = FALSE)
    init_omegas <- initial_est$omegas
    mat_opts <- attr(init_omegas, "mat_opts")

    # Separate full matrix into sub-matrices per record
    sub_mats <- get_sub_mat(init_omegas)

    # make sub-matrices variance-covariance
    sub_mats_var <- purrr::imap(sub_mats, function(mat, rec_n){
      mod_matrix(mat, mat_opt = mat_opts[rec_n,]) %>% signif(2)
    })

    # Confirm they all evaluate to the same variance-covariance matrix
    test_mat <- matrix(c(0.64, -0.24, NA, 0.58), nrow = 2)
    purrr::walk(sub_mats_var, function(mat){
      expect_true(all.equal(test_mat, mat))
    })

    # Reverse operation
    sub_mats_inv <- purrr::imap(mat_var_cat, function(mat, rec_n){
      mod_matrix(mat, mat_opt = mat_opts[rec_n,], inverse = TRUE) %>% signif(3)
    })

    # Confirm reverse operation works
    purrr::walk2(sub_mats_inv, sub_mats, function(mat, mat_orig){
      expect_true(all.equal(mat_orig, mat))
    })
  })

  it("supported matrix-type records: same blocks", {
    test_case1 <- list(
      case = "same blocks",
      input_ctl = "
      $OMEGA BLOCK(2)
      0.64
      -0.24 0.58

      $OMEGA BLOCK(2) SAME(3)
      "
    )

    test_case2 <- list(
      case = "same blocks",
      input_ctl = "
      $OMEGA BLOCK(2)
      0.64
      -0.24 0.58

      ; is equivalent to above
      $OMEGA BLOCK(2) SAME
      $OMEGA BLOCK(2) SAME
      $OMEGA BLOCK(2) SAME
      "
    )

    # Create fake model for test_case1
    mod_tweak <- do.call(make_fake_mod, test_case1)
    on.exit(delete_models(mod_tweak, .tags = NULL, .force = TRUE))

    mat_opts <- get_matrix_opts(mod_tweak) %>%
      dplyr::filter(record_type == "omega")

    # Get initial estimates
    initial_est <- get_initial_est(mod_tweak, flag_fixed = FALSE)
  })

  it("supported matrix-type records: value blocks", {
    test_case1 <- list(
      case = "value blocks",
      input_ctl = "
      $OMEGA BLOCK(6) VALUES(0.1,0.01)
      "
    )

    test_case2 <- list(
      case = "value blocks",
      input_ctl = "
      ; is equivalent to above:
      $OMEGA BLOCK(6)
      0.1
      0.01 0.1
      (0.01)x2 0.1
      (0.01)x3 0.1
      (0.01)x4 0.1
      (0.01)x5 0.1
      "
    )

    # Create fake model for test_case1
    mod_tweak <- do.call(make_fake_mod, test_case1)
    on.exit(delete_models(mod_tweak, .tags = NULL, .force = TRUE))

  })

})
