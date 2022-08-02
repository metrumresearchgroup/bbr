context("Test get_omega, get_sigma, and get_theta functions")

skip_if_not_drone_or_metworx("test-get-param")

mod_complex <- read_model(file.path(MODEL_DIR_X, "example2_saemimp"))


# matches values reported in parameter estimates to location in matrix
# and checks symmetry (along the diagonal)
check_matrix <- function(matrix, par_df, .type = "OMEGA"){
  expect_true(isSymmetric(matrix))

  for(name.i in unique(par_df$parameter_names)){
    # message("spot checking ", name.i)
    spot_chk <- par_df$estimate[par_df$parameter_names == name.i]
    # Get positions from labels
    matrix_pos <- gsub(.type, "",name.i)
    matrix_pos <- gsub("[()]", "", matrix_pos)
    matrix_pos <- strsplit(matrix_pos, ",")
    row.i <- matrix_pos[[1]][1] %>% as.numeric()
    col.i <- matrix_pos[[1]][2] %>% as.numeric()

    expect_identical(matrix[row.i,col.i], spot_chk)
  }
}

withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {

  test_that("get_theta() returns correct values and labels: model_summary [BBR-PEST-015]", {
    clean_test_enviroment()
    thetas <- MOD1 %>% model_summary() %>% get_theta()
    par_df <- MOD1 %>% model_summary() %>% param_estimates() %>% filter(grepl("THETA", parameter_names))

    expect_equal(par_df$parameter_names, names(thetas))
    expect_equal(par_df$estimate, unname(thetas))
  })

  test_that("get_theta() returns correct values and labels: model_summaries [BBR-PEST-015]", {
    clean_test_enviroment()

    on.exit({
      fs::dir_delete(NEW_MOD2)
      fs::file_delete(ctl_ext(NEW_MOD2))
      fs::file_delete(yaml_ext(NEW_MOD2))
    })

    # create new model
    mod2 <- MOD1 %>% copy_model_from(basename(NEW_MOD2))

    # copy output directory (to simulate model run)
    copy_output_dir(MOD1, NEW_MOD2)

    thetas <- list(MOD1, mod2) %>% model_summaries() %>% get_theta()
    par_df <- MOD1 %>% model_summary() %>% param_estimates() %>% filter(grepl("THETA", parameter_names))
    par_df2 <- mod2 %>% model_summary() %>% param_estimates() %>% filter(grepl("THETA", parameter_names))

    expect_true(length(thetas) == 2)
    expect_equal(par_df$parameter_names, names(thetas[[1]]))
    expect_equal(par_df$estimate, unname(thetas[[1]]))
    expect_equal(par_df2$parameter_names, names(thetas[[2]]))
    expect_equal(par_df2$estimate, unname(thetas[[2]]))
  })


  test_that("get_omega() returns correct values and labels: model_summary [BBR-PEST-017]", {
    clean_test_enviroment()
    omegas <- MOD1 %>% model_summary() %>% get_omega()
    par_df <- MOD1 %>% model_summary() %>% param_estimates() %>% filter(grepl("OMEGA", parameter_names))

    # order is preserved, and duplicate covariances are dropped
    check_matrix(omegas, par_df, .type = "OMEGA")
  })

  test_that("get_omega() returns correct values and labels: model_summaries [BBR-PEST-017]", {
    clean_test_enviroment()
    mods <- list(MOD1, mod_complex)
    sums <- mods %>% model_summaries()
    omegas <- sums %>% get_omega()

    expect_true(is.list(omegas))
    par_df1 <- MOD1 %>% model_summary() %>% param_estimates() %>% filter(grepl("OMEGA", parameter_names))
    par_df2 <- mod_complex %>% model_summary() %>% param_estimates() %>% filter(grepl("OMEGA", parameter_names))

    # order is preserved, and duplicate covariances are dropped
    check_matrix(omegas[[1]], par_df1, .type = "OMEGA")
    check_matrix(omegas[[2]], par_df2, .type = "OMEGA")
  })

  test_that("get_sigma() returns correct values and labels: model_summary [BBR-PEST-018]", {
    clean_test_enviroment()
    sigmas <- MOD1 %>% model_summary() %>% get_sigma()
    par_df <- MOD1 %>% model_summary() %>% param_estimates() %>% filter(grepl("SIGMA", parameter_names))

    # order is preserved, and duplicate covariances are dropped
    check_matrix(sigmas, par_df, .type = "SIGMA")
  })

  test_that("get_sigma() returns correct values and labels: model_summaries [BBR-PEST-018]", {
    clean_test_enviroment()
    mods <- list(MOD1, mod_complex)
    sums <- mods %>% model_summaries()
    sigmas <- sums %>% get_sigma()

    expect_true(is.list(sigmas))
    par_df1 <- MOD1 %>% model_summary() %>% param_estimates() %>% filter(grepl("SIGMA", parameter_names))
    par_df2 <- mod_complex %>% model_summary() %>% param_estimates() %>% filter(grepl("SIGMA", parameter_names))

    # order is preserved, and duplicate covariances are dropped
    check_matrix(sigmas[[1]], par_df1, .type = "SIGMA")
    check_matrix(sigmas[[2]], par_df2, .type = "SIGMA")
  })
})

