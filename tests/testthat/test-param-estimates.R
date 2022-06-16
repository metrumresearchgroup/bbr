context("Test param_estimates functions")

skip_if_not_drone_or_metworx("test-param-estimates")


# build reference
ref_df1 <- dget(PARAM_REF_FILE)

withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {

  test_that("param_estimates.bbi_model_summary gets expected table [BBR-PEST-001]", {
    clean_test_enviroment()

    par_df <- MOD1 %>% model_summary() %>% param_estimates()
    expect_equal(par_df, ref_df1)
  })

  test_that("param_estimates correctly errors on Bayesian model [BBR-PEST-002]", {
    clean_test_enviroment()

    mod1 <- read_model(file.path(MODEL_DIR_X, "1001"))
    sum1 <- model_summary(mod1, .bbi_args = list(ext_file = "1001.1.TXT"))
    expect_error(
      param_estimates(sum1),
      regexp = "not currently implemented for Bayesian methods"
    )
  })

  test_that("param_estimates correctly errors on Bayesian model with multiple estimation methods [BBR-PEST-003]", {
    clean_test_enviroment()

    sum1 <- file.path(MODEL_DIR_X, "acop-fake-bayes") %>%
      read_model() %>%
      model_summary()
    expect_error(
      param_estimates(sum1),
      regexp = "not currently implemented for Bayesian methods"
    )
  })

  test_that("param_estimates correctly errors on ONLYSIM models [BBR-PEST-003]", {
    clean_test_enviroment()

    skip_if_old_bbi("3.1.0")
    sum1 <- file.path(MODEL_DIR_X, "acop-onlysim") %>%
      read_model() %>%
      model_summary()
    expect_error(
      param_estimates(sum1),
      regexp = "no estimation method \\(ONLYSIM\\)"
    )
  })

  test_that("param_estimates correctly warns on mixture model [BBR-PEST-004]", {
    clean_test_enviroment()

    mod1 <- read_model(file.path(MODEL_DIR_X, "iovmm"))
    sum1 <- model_summary(mod1)

    expect_warning(
      par_df <- param_estimates(sum1),
      regexp = "mixture model"
    )

    expect_true(all(is.na(par_df[[SUMMARY_PARAM_SHRINKAGE]])))
  })

  test_that("param_estimates can optionally return p-value [BBR-PEST-013]", {
    clean_test_enviroment()
    sum1 <- model_summary(MOD1)

    par_df <- sum1 %>% param_estimates(.alpha = 0.05)
    expect_true(all(c("pval", "ETASIG") %in% names(par_df)))

    par_df_pvalue_only <- par_df %>% filter(str_detect(parameter_names, "OMEGA") & diag == TRUE)

    expect_equal(round(par_df_pvalue_only$pval, 3) , c(0.962, 0.919))
    expect_equal(par_df_pvalue_only$ETASIG , rep(FALSE, 2))

    par_df_rest <- par_df %>% filter(!(str_detect(parameter_names, "OMEGA") & diag == TRUE))

    expect_true(all(is.na(par_df_rest[["pval"]])))
    expect_true(all(is.na(par_df_rest[["ETASIG"]])))
  })


}) # closing withr::with_options
