context("Test get_omega, get_sigma, and get_theta functions")

skip_if_not_drone_or_metworx("test-get-param")

mod_complex <- read_model(file.path(MODEL_DIR_X, "acop-fake-bayes"))

withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {

  test_that("get_theta() returns correct values and labels (model_summary) [BBR-PEST-015]", {
    clean_test_enviroment()
    thetas <- MOD1 %>% model_summary() %>% get_theta()
    par_df <- MOD1 %>% model_summary() %>% param_estimates() %>% filter(grepl("THETA", parameter_names))

    expect_equal(par_df$parameter_names, names(thetas))
    expect_equal(par_df$estimate, unname(thetas))
  })

  test_that("get_theta() returns correct values and labels (model_summaries) [BBR-PEST-016]", {
    clean_test_enviroment()

    on.exit({
      fs::dir_delete(NEW_MOD2)
      fs::file_delete(ctl_ext(NEW_MOD2))
      fs::file_delete(yaml_ext(NEW_MOD2))
    })

    # create new model
    mod2 <- MOD1 %>% copy_model_from(basename(NEW_MOD2))

    # copy output directory (to simulate model run)
    fs::dir_copy(MOD1_PATH, NEW_MOD2)

    thetas <- list(MOD1, mod2) %>% model_summaries() %>% get_theta()
    par_df <- MOD1 %>% model_summary() %>% param_estimates() %>% filter(grepl("THETA", parameter_names))
    par_df2 <- mod2 %>% model_summary() %>% param_estimates() %>% filter(grepl("THETA", parameter_names))

    expect_true(length(thetas) == 2)
    expect_equal(par_df$parameter_names, names(thetas[[1]]))
    expect_equal(par_df$estimate, unname(thetas[[1]]))
    expect_equal(par_df2$parameter_names, names(thetas[[2]]))
    expect_equal(par_df2$estimate, unname(thetas[[2]]))
  })


  test_that("format_matrix()  [BBR-PEST-017]", {
    clean_test_enviroment()
    mod <- read_model(file.path(MODEL_DIR_X, "example2_saemimp"))
    sum <- mod %>% model_summary()
    values <- sum$parameters_data[[2]]$estimates$omega
    labels <- sum$parameter_names$omega
    matrix <- format_matrix(values, labels, .type = "OMEGA")

    expect_true(isSymmetric(matrix))

    par_df <- mod %>% model_summary() %>% param_estimates() %>% filter(grepl("OMEGA", parameter_names))

    expect_equal(matrix[lower.tri(matrix, diag = TRUE)], par_df$estimate)

    expect_equal(par_df$parameter_names, names(thetas))
    expect_equal(par_df$estimate, unname(thetas))
  })


  test_that("get_omega() returns correct values and labels [BBR-PEST-018]", {
    clean_test_enviroment()
    omegas <- MOD1 %>% model_summary() %>% get_omega()
    par_df <- MOD1 %>% model_summary() %>% param_estimates() %>% filter(grepl("OMEGA", parameter_names))

    # order is preserved, and duplicate coviarances are dropped
    expect_equal(par_df$estimate, unique(as.vector(omegas)))
  })

  test_that("get_sigma() returns correct values and labels [BBR-PEST-019]", {
    clean_test_enviroment()
    sigmas <- MOD1 %>% model_summary() %>% get_sigma()
    par_df <- MOD1 %>% model_summary() %>% param_estimates() %>% filter(grepl("SIGMA", parameter_names))

    # order is preserved, and duplicate coviarances are dropped
    expect_equal(par_df$estimate, unique(as.vector(sigmas)))
  })
})

