skip_if_not_ci_or_metworx("test-cov-cor")

check_mat_dim <- function(.mat, .theta_dim, .dim) {
  expect_equal(length(.mat), 4)
  expect_equal(dim(.mat$cov_theta), c(.theta_dim, .theta_dim))
  expect_equal(dim(.mat$cor_theta), c(.theta_dim, .theta_dim))
  expect_equal(dim(.mat$cov), c(.dim, .dim))
  expect_equal(dim(.mat$cor), c(.dim, .dim))
}

withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {

  test_that("cov_cor() works basic model [BBR-CVCR-001]", {
    .c <- cov_cor(MOD1)
    check_mat_dim(.c, 5, 9)

    # check that all have the right names
    mat_names <- c(
      "THETA1", "THETA2", "THETA3", "THETA4", "THETA5",
      "SIGMA(1,1)", "OMEGA(1,1)", "OMEGA(2,1)", "OMEGA(2,2)"
    )
    walk(names(.c), ~{
      names_to_check <- if (stringr::str_detect(.x, "theta")) {
        stringr::str_subset(mat_names, "THETA")
      } else {
        mat_names
      }
      expect_equal(dimnames(.c[[.x]])[[1]], names_to_check)
      expect_equal(dimnames(.c[[.x]])[[2]], names_to_check)
    })

    # check values are equal
    expect_equal(.c$cov[1:5, 1:5], .c$cov_theta)
    expect_equal(.c$cor[1:5, 1:5], .c$cor_theta)

  })

  test_that("cov_cor() works with two estimation methods [BBR-CVCR-002]", {
    .c <- file.path(MODEL_DIR_X, "example2_saemimp") %>%
      read_model() %>%
      cov_cor()
    check_mat_dim(.c, 11, 22)

    # TODO: add a test that it's actually the second estimation method
  })

  test_that("cov_cor() warns with correlations over threshold [BBR-CVCR-003]", {
    expect_warning({
      .c <- cov_cor(MOD1, .threshold = 0.95)
    }, regexp = "correlations above specified threshold")

    check_mat_dim(.c, 5, 9)
  })

  test_that("cov_cor() errors if no .cov file [BBR-CVCR-004]", {
    expect_error({
      .c <- file.path(MODEL_DIR_X, "acop-fake-bayes") %>%
        read_model() %>%
        cov_cor()
    }, regexp = "[Nn]o file present.*\\.cov")
  })

  test_that("check_cor_threshold() works correctly [BBR-CVCR-005]", {
    .c <- cov_cor(MOD1)
    check_mat_dim(.c, 5, 9)

    expect_warning({
      check_cor_threshold(.c$cor_theta)
    }, regexp = "correlations above specified threshold.+\\(5,4\\)$")

    expect_warning({
      check_cor_threshold(.c$cor_theta, 0.5)
    }, regexp = "correlations above specified threshold.+\\(4,1\\).+\\(5,1\\).+\\(5,4\\)$")
  })
})


