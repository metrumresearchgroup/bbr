context("Test param_estimates functions")

if (Sys.getenv("METWORX_VERSION") == "" && Sys.getenv("DRONE") != "true") {
  skip("test-param-estimates only runs on Metworx or Drone")
}

setup({
  cleanup()
})
teardown({
  cleanup()
})

# build reference
ref_df1 <- readRDS(PARAM_REF_FILE)

withr::with_options(list(rbabylon.bbi_exe_path = read_bbi_path(),
                         rbabylon.model_directory = NULL), {

  test_that("param_estimates.bbi_model_summary gets expected table", {
    par_df <- MOD1 %>% model_summary() %>% param_estimates()
    expect_equal(par_df, ref_df1)
  })

  test_that("param_estimates correctly errors on Bayesian model", {
    expect_error(
      par_df <- 1001 %>%
        model_summary(.directory = MODEL_DIR_X, .bbi_args = list(ext_file = "1001.1.TXT")) %>%
        param_estimates(),
      regexp = "not currently implemented for Bayesian methods"
    )
  })

  test_that("param_estimates correctly warns on mixture model", {
    expect_warning(
      par_df <- "iovmm" %>%
        model_summary(.directory = MODEL_DIR_X) %>%
        param_estimates(),
      regexp = "mixture model"
    )

    expect_true(all(is.na(par_df[[SUMMARY_PARAM_SHRINKAGE]])))
  })

}) # closing withr::with_options
