skip_if_not_drone_or_metworx("test-cov-cor")

withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {

  test_that("cov_cor() works basic model", {
    .c <- cov_cor(MOD1, .threshold = NULL)
    expect_equal(length(.c), 2)
    expect_equal(dim(.c$cov_theta), c(5,5))
    expect_equal(dim(.c$cor_theta), c(5,5))
  })

  test_that("cov_cor() works with two estimation methods", {
    .c <- file.path(MODEL_DIR_X, "example2_saemimp") %>%
      read_model() %>%
      cov_cor(.threshold = NULL)
    expect_equal(length(.c), 2)
    expect_equal(dim(.c$cov_theta), c(11,11))
    expect_equal(dim(.c$cor_theta), c(11,11))

    # TODO: add a test that it's actually the second estimation method
  })

  test_that("cov_cor() warns with correlations over threshold", {
    expect_warning({
      .c <- cov_cor(MOD1)
    }, regexp = "correlations above specified threshold")

    expect_equal(length(.c), 2)
    expect_equal(dim(.c$cov_theta), c(5,5))
    expect_equal(dim(.c$cor_theta), c(5,5))
  })

  test_that("cov_cor() errors if no .cov file", {
    expect_error({
      .c <- file.path(MODEL_DIR_X, "acop-fake-bayes") %>%
        read_model() %>%
        cov_cor()
    }, regexp = "[Nn]o file present.*\\.cov")
  })
})


