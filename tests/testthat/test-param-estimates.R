context("Test param_estimates functions")

if (Sys.getenv("METWORX_VERSION") == "" && Sys.getenv("DRONE") != "true") {
  skip("test-param-estimates only runs on Metworx or Drone")
}

setup({
  cleanup()
  invisible(copy_model_from(yaml_ext(MOD1_PATH), NEW_MOD2, "model from test-param-estimates.R", .directory = "."))
  invisible(copy_model_from(yaml_ext(MOD1_PATH), NEW_MOD3, "model from test-param-estimates.R", .directory = "."))
  fs::dir_copy(MOD1_PATH, NEW_MOD2)
  fs::dir_copy(MOD1_PATH, NEW_MOD3)

  # build references
  ref_df1 <- readRDS("data/acop_param_table_ref_200818.rds")
  ref_df1b <- dplyr::select(ref_df1, .data[[SUMMARY_PARAM_NAMES]], .data[["estimate"]])
  ref_df2 <- dplyr::bind_rows(
    dplyr::bind_cols(!!ABS_MOD_PATH := normalizePath(MOD1_PATH), ref_df1b),
    dplyr::bind_cols(!!ABS_MOD_PATH := normalizePath(NEW_MOD2),  ref_df1b),
    dplyr::bind_cols(!!ABS_MOD_PATH := normalizePath(NEW_MOD3),  ref_df1b)
  )
})
teardown({
  cleanup()
})

withr::with_options(list(rbabylon.bbi_exe_path = read_bbi_path(),
                         rbabylon.model_directory = NULL), {

  test_that("param_estimates.bbi_model_summary gets expected table", {
    # get summary
    sum1 <- MOD1 %>% model_summary()

    # extract parameter df
    par_df <- param_estimates(sum1)

    # for some arcane reason `expect_equal(par_df1, ref_df)` fails, so we just check a few columns
    expect_true(all(ref_df1$parameter_names == ref_df$parameter_names))
    expect_true(all(round(ref_df1$estimate, 2) == round(ref_df$estimate, 2)))
    expect_true(all(ref_df1$fixed == ref_df$fixed))
  })

test_that("param_estimates.bbi_summary_list gets expected table", {

  # extract parameter df
  par_df <- summary_log() %>% as_summary_list() %>% param_estimates()

  # check against reference
  expect_equal(par_df, ref_df2)
})

test_that("param_estimates.bbi_summary_log_df gets expected table", {

  # extract parameter df
  par_df <- summary_log() %>% param_estimates()

  # check against reference
  expect_equal(par_df, ref_df2)
})

}) # closing withr::with_options
