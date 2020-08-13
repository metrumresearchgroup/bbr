context("Test creating summary logs")

if (Sys.getenv("METWORX_VERSION") == "" && Sys.getenv("DRONE") != "true") {
  skip("test-summary only runs on Metworx or Drone")
}

# references
OFV_REF <- 2636.846
PARAM_COUNT_REF <- 7

SUMMARY_LOG_COLS <- 18
ADD_SUMMARY_COLS <- 25

# helper to run expectations
test_sum_df <- function(sum_df, .paths, .col_count) {
  expect_true(inherits(sum_df, SUM_LOG_CLASS))

  num_mods <- length(.paths)
  expect_equal(nrow(sum_df), num_mods)
  expect_equal(ncol(sum_df), .col_count)

  # check some columns
  expect_equal(sum_df$absolute_model_path, purrr::map_chr(.paths, ~normalizePath(.x)))
  expect_true(all(is.na(sum_df$error_msg)))
  expect_false(any(sum_df$needed_fail_flags))
  expect_equal(sum_df$ofv, rep(OFV_REF, num_mods))
  expect_equal(sum_df$param_count, rep(PARAM_COUNT_REF, num_mods))
  expect_false(any(sum_df$minimization_terminated))
}

setup({
  cleanup()
  create_all_models()
  fs::dir_copy(MOD1_PATH, NEW_MOD2)
  fs::dir_copy(MOD1_PATH, NEW_MOD3)
  fs::dir_copy(MOD1_PATH, LEVEL2_MOD)
})
teardown({
  cleanup()
})

withr::with_options(list(rbabylon.model_directory = NULL), {

  test_that("summary_log() errors with no .base_dir set", {
    log_df <- expect_error(summary_log(), regexp = "`.base_dir` cannot be `NULL`")
  })

  test_that("summary_log() errors with malformed YAML", {
    log_df <- expect_error(summary_log(getwd()), regexp = "Unexpected error.+model_path defined in yaml")
  })

  test_that("summary_log() returns NULL and warns when no YAML found", {
    log_df <- expect_warning(summary_log("data"), regexp = "Found no valid model YAML files in data")
    expect_true(inherits(log_df, "tbl"))
    expect_equal(nrow(log_df), 0)
    expect_equal(ncol(log_df), 0)
  })
}) # closing withr::with_options


withr::with_options(list(rbabylon.bbi_exe_path = read_bbi_path(),
                         rbabylon.model_directory = normalizePath(MODEL_DIR)), {

  #########################################
  # extracting things from summary object
  #########################################

  test_that("summary_log() works correctly with nested dirs", {
    sum_df <- summary_log()
    test_sum_df(sum_df, c(MOD1_PATH, NEW_MOD2, NEW_MOD3, LEVEL2_MOD), SUM_LOG_COLS)
  })

  test_that("summary_log(.recurse = FALSE) works", {
    sum_df <- summary_log(.recurse = FALSE)
    test_sum_df(sum_df, c(MOD1_PATH, NEW_MOD2, NEW_MOD3), SUM_LOG_COLS)
  })

  test_that("add_summary() works correctly", {
    sum_df <- run_log() %>% add_summary()
    expect_true(inherits(sum_df, RUN_LOG_CLASS))
    test_sum_df(sum_df, c(MOD1_PATH, NEW_MOD2, NEW_MOD3, LEVEL2_MOD), RUN_LOG_COLS+SUM_LOG_COLS-1)
    expect_identical(sum_df$model_type, rep("nonmem", RUN_LOG_ROWS+1))
    expect_identical(sum_df$yaml_md5, ALL_MODS_YAML_MD5)
  })

  # THESE TESTS NEEDS TO BE LAST BECAUSE IT DELETES NECESSARY FILES
  fs::file_delete(file.path(LEVEL2_MOD, "1.grd"))

  test_that("summary_log works some failed summaries", {
    sum_df <- summary_log()
    expect_equal(is.na(sum_df$error_msg), c(TRUE, TRUE, TRUE, FALSE))
    expect_equal(ncol(sum_df), SUM_LOG_COLS)

    sum_df <- summary_log(.bbi_args = list(no_grd_file = TRUE))
    test_sum_df(sum_df, c(MOD1_PATH, NEW_MOD2, NEW_MOD3, LEVEL2_MOD), SUM_LOG_COLS)
  })

  test_that("summary_log works all failed summaries", {

    expect_warning({
      sum_df <- summary_log(LEVEL2_DIR)
    }, regexp = "ALL 1 MODEL SUMMARIES FAILED")

    expect_equal(names(sum_df), c(ABS_MOD_PATH, "error_msg"))
    expect_true(all(grepl("--no-grd-file", sum_df$error_msg)))

  })

}) # closing withr::with_options
