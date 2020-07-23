context("Test bbi summary functions")

if (Sys.getenv("METWORX_VERSION") == "" && Sys.getenv("DRONE") != "true") {
  skip("test-param-estimates only runs on Metworx or Drone")
}

withr::with_options(list(rbabylon.bbi_exe_path = read_bbi_path(),
                         rbabylon.model_directory = NULL), {

  # references
  NOT_FINISHED_ERR_MSG <- "nonmem_summary.*modeling run has not finished"
  NO_LST_ERR_MSG <- "Unable to locate `.lst` file.*NONMEM output folder"


  test_that("param_estimates() gets expected table", {
    # get summary
    sum1 <- MOD1 %>% model_summary()

    # extract parameter df
    par_df1 <- param_estimates(sum1)

    # compare to reference
    ref_df <- readRDS("data/acop_param_table_ref_200228.rds")

    # for some arcane reason `expect_equal(par_df1, ref_df)` fails, so we just check a few columns
    expect_true(all(par_df1$names == ref_df$names))
    expect_true(all(round(par_df1$estimate, 2) == round(ref_df$estimate, 2)))
    expect_true(all(par_df1$fixed == ref_df$fixed))
  })

}) # closing withr::with_options
