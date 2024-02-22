

parse_case_mats <- function(test_case){
  # Create fake model
  mod_tweak <- do.call(make_fake_mod, test_case)
  on.exit(delete_models(mod_tweak, .tags = NULL, .force = TRUE))

  # Get initial estimates
  initial_est <- get_initial_est(mod_tweak, flag_fixed = TRUE)
  mat_opts <- attr(initial_est$omegas, "mat_opts")

  # Separate full matrix into sub-matrices per record
  sub_mats <- get_sub_mat(initial_est$omegas)

  return(
    list(
      init_omegas = initial_est$omegas,
      sub_mats = sub_mats,
      mat_opts = mat_opts
    )
  )
}

# Note that most of the example records came from the OMEGA help page in the
# NONMEM docs: nm-help/nm750/html/$omega.htm
describe("matrix-handling", {
  skip_if_old_nmrec("0.4.0")

  # This test primarily tests `mod_matrix`, `check_and_modify_pd`, and other
  # helper functions called by those. Edge cases (such as SAME blocks) are tested
  # elsewhere.

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

    # Parse test case
    test_case_results <- parse_case_mats(test_case = test_case)
    sub_mats <- test_case_results$sub_mats
    mat_opts <- test_case_results$mat_opts

    # Confirm extracted matrix options
    expect_equal(
      mat_opts$diag,
      c("variance", "standard", "standard", "variance", "cholesky")
    )
    expect_equal(
      mat_opts$off_diag,
      c("covariance", "covariance", "correlation", "correlation", "cholesky")
    )

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
    sub_mats_inv <- purrr::imap(sub_mats_var, function(mat, rec_n){
      mod_matrix(mat, mat_opt = mat_opts[rec_n,], inverse = TRUE) %>% signif(2)
    })

    purrr::iwalk(sub_mats_inv, function(mat_inv, rec_n){
      mat_orig <- sub_mats[[rec_n]]
      # Confirm reverse operation works
      expect_true(all.equal(signif(mat_orig, 2), mat_inv))
      # Confirm `check_and_modify_pd` returns to original format
      expect_true(
        all.equal(
          check_and_modify_pd(mat_orig, mat_opt = mat_opts[rec_n,], digits = 2),
          mat_inv
        )
      )
    })

    # Remove attributes from initial omegas to compare to final matrix
    init_omegas <- test_case_results$init_omegas
    attributes(init_omegas)[setdiff(names(attributes(init_omegas)), 'dim')] <- NULL

    # Confirm starting matrix is the same as the final one (since nearPD wasnt called)
    expect_equal(
      validate_matrix_pd(test_case_results$init_omegas, digits = 3),
      init_omegas
    )
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

    # Parse test case 1
    test_case_results1 <- parse_case_mats(test_case = test_case1)
    sub_mats1 <- test_case_results1$sub_mats
    mat_opts1 <- test_case_results1$mat_opts
    init_omegas1 <- test_case_results1$init_omegas

    # Parse test case 2
    test_case_results2 <- parse_case_mats(test_case = test_case2)
    sub_mats2 <- test_case_results2$sub_mats
    mat_opts2 <- test_case_results2$mat_opts
    init_omegas2 <- test_case_results2$init_omegas

    # Final matrices should be identical
    expect_true(
      all.equal(
        validate_matrix_pd(init_omegas1, digits = 3),
        validate_matrix_pd(init_omegas2, digits = 3)
      )
    )

    # Confirm dimensions
    expect_true(length(sub_mats1) == 2)
    expect_true(all(purrr::map_lgl(sub_mats1[2], function(.x) all(is.na(.x)))))
    expect_true(length(sub_mats2) == 4)
    expect_true(all(purrr::map_lgl(sub_mats2[2:4], function(.x) all(is.na(.x)))))

    # Confirm SAME identification
    expect_equal(mat_opts1$same, c(FALSE, TRUE))
    expect_equal(mat_opts1$same_n, c(NA, 3))
    expect_equal(mat_opts2$same, c(FALSE, TRUE, TRUE, TRUE))
    expect_equal(mat_opts2$same_n, c(NA, 1, 1, 1))

    # Diagonally concatenate values, while parsing `same` blocks
    expect_equal(
      cat_mat_diag(sub_mats1, mat_opts = mat_opts1),
      cat_mat_diag(sub_mats2, mat_opts = mat_opts2)
    )
  })

  it("supported matrix-type records: value blocks", {
    test_case1 <- list(
      case = "vpair subtype",
      input_ctl = "
      $OMEGA BLOCK(6) VALUES(0.1,0.01)
      "
    )

    test_case2 <- list(
      case = "values subtype",
      input_ctl = "
      ; evaluates to the same as above:
      $OMEGA BLOCK(6)
      0.1
      0.01 0.1
      (0.01)x2 0.1
      (0.01)x3 0.1
      (0.01)x4 0.1
      (0.01)x5 0.1
      "
    )

    # Parse test case 1
    test_case_results1 <- parse_case_mats(test_case = test_case1)
    sub_mats1 <- test_case_results1$sub_mats
    mat_opts1 <- test_case_results1$mat_opts
    init_omegas1 <- test_case_results1$init_omegas

    # Parse test case 2
    test_case_results2 <- parse_case_mats(test_case = test_case2)
    sub_mats2 <- test_case_results2$sub_mats
    mat_opts2 <- test_case_results2$mat_opts
    init_omegas2 <- test_case_results2$init_omegas

    # Confirm matrix specifications
    expect_equal(mat_opts1$subtype, "vpair")
    expect_equal(mat_opts1$param_x[[1]], 1)
    expect_equal(mat_opts2$subtype, "values")
    expect_equal(mat_opts2$param_x[[1]], c(1, 1, 1, 2, 1, 3, 1, 4, 1, 5, 1))

    # Check that expanded matrices are identical
    expect_equal(
      expand_value_matrix(sub_mats1[[1]], mat_opts1),
      expand_value_matrix(sub_mats2[[1]], mat_opts2)
    )

    # Remove attributes from initial omegas to compare to final matrix
    attributes(init_omegas1)[setdiff(names(attributes(init_omegas1)), 'dim')] <- NULL
    attributes(init_omegas2)[setdiff(names(attributes(init_omegas2)), 'dim')] <- NULL

    # Confirm starting matrix is the same as the final one (since nearPD wasnt called)
    expect_equal(
      validate_matrix_pd(test_case_results1$init_omegas, digits = 3),
      init_omegas1
    )
    expect_equal(
      validate_matrix_pd(test_case_results2$init_omegas, digits = 3),
      init_omegas2
    )
  })

})
