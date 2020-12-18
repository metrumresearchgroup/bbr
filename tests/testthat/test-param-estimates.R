context("Test param_estimates functions")

skip_if_not_drone_or_metworx("test-param-estimates")

setup({
  cleanup()
})
teardown({
  cleanup()
})

# build reference
ref_df1 <- dget(PARAM_REF_FILE)

withr::with_options(list(rbabylon.bbi_exe_path = read_bbi_path()), {

  test_that("param_estimates.bbi_model_summary gets expected table", {
    par_df <- MOD1 %>% model_summary() %>% param_estimates()
    expect_equal(par_df, ref_df1)
  })

  test_that("param_estimates correctly errors on Bayesian model", {
    mod1 <- read_model(file.path(MODEL_DIR_X, "1001"))
    sum1 <- model_summary(mod1, .bbi_args = list(ext_file = "1001.1.TXT"))
    expect_error(
      param_estimates(sum1),
      regexp = "not currently implemented for Bayesian methods"
    )
  })

  test_that("param_estimates correctly warns on mixture model", {
    mod1 <- read_model(file.path(MODEL_DIR_X, "iovmm"))
    sum1 <- model_summary(mod1)

    expect_warning(
      par_df <- param_estimates(sum1),
      regexp = "mixture model"
    )

    expect_true(all(is.na(par_df[[SUMMARY_PARAM_SHRINKAGE]])))
  })

}) # closing withr::with_options
