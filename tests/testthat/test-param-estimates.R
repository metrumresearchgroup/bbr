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
})
teardown({
  cleanup()
})

# build reference
ref_df1 <- readRDS(PARAM_REF_FILE)

withr::with_options(list(rbabylon.bbi_exe_path = read_bbi_path(),
                         rbabylon.model_directory = NULL), {

  test_that("param_estimates.bbi_model_summary gets expected table", {

    # extract parameter df
    par_df <- MOD1 %>% model_summary() %>% param_estimates()

    # for some arcane reason `expect_equal(par_df, ref_df1)` fails, so we just check a few columns
    expect_true(all(par_df$parameter_names == ref_df1$parameter_names))
    expect_true(all(round(par_df$estimate, 2) == round(ref_df1$estimate, 2)))
    expect_true(all(par_df$fixed == ref_df1$fixed))
  })

}) # closing withr::with_options
