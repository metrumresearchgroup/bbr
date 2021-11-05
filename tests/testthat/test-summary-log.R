context("Test creating summary logs")

skip_if_not_drone_or_metworx("test-summary-log")

# helper to run expectations
test_sum_df <- function(sum_df, .paths, .col_count) {
  # check sum_df class
  expect_true(inherits(sum_df, SUM_LOG_CLASS))
  expect_true(inherits(sum_df, LOG_DF_CLASS))

  num_mods <- length(.paths)
  expect_equal(nrow(sum_df), num_mods)
  expect_equal(ncol(sum_df), .col_count)

  # check some columns
  expect_equal(sum_df$absolute_model_path, purrr::map_chr(.paths, ~normalizePath(.x)))
  expect_true(all(is.na(sum_df$error_msg)))
  expect_false(any(sum_df$needed_fail_flags))
  expect_equal(sum_df$ofv, rep(MOD1_OFV_REF, num_mods))
  expect_equal(sum_df$param_count, rep(MOD1_PARAM_COUNT_FIXED, num_mods))
  expect_false(any(sum_df$minimization_terminated))
}

# setup
cleanup()
create_all_models()
copy_all_output_dirs()
# teardown
withr::defer(cleanup())


test_that("summary_log() returns NULL and warns when no YAML found [BBR-SMLG-001]", {
  log_df <- expect_warning(summary_log(file.path(REF_DIR, "read-output-refs")), regexp = "Found no valid model YAML files in")
  expect_true(inherits(log_df, "tbl"))
  expect_equal(nrow(log_df), 0)
  expect_equal(ncol(log_df), 0)
})

withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {

  #########################################
  # extracting things from summary object
  #########################################

  test_that("summary_log() works correctly with nested dirs [BBR-SMLG-002]", {
    sum_df <- summary_log(MODEL_DIR)
    test_sum_df(sum_df, c(MOD1_PATH, NEW_MOD2, NEW_MOD3, LEVEL2_MOD), SUM_LOG_COLS)
  })

  test_that("summary_log(.recurse = FALSE) works [BBR-SMLG-003]", {
    sum_df <- summary_log(MODEL_DIR, .recurse = FALSE)
    test_sum_df(sum_df, c(MOD1_PATH, NEW_MOD2, NEW_MOD3), SUM_LOG_COLS)
  })

  test_that("add_summary() works correctly [BBR-SMLG-004]", {
    sum_df <- run_log(MODEL_DIR) %>% add_summary()
    test_sum_df(sum_df, c(MOD1_PATH, NEW_MOD2, NEW_MOD3, LEVEL2_MOD), RUN_LOG_COLS+SUM_LOG_COLS-2)
    expect_identical(sum_df$model_type, rep("nonmem", RUN_LOG_ROWS+1))
    expect_identical(sum_df$yaml_md5, ALL_MODS_YAML_MD5)
  })

  test_that("add_summary() has correct columns [BBR-SMLG-005]", {
    sum_df <- summary_log(MODEL_DIR)
    log_df <- run_log(MODEL_DIR)
    add_df <- log_df %>% add_summary()

    # should have all columns from both (minus the join key)
    expect_identical(names(add_df), c(names(log_df), names(sum_df)[3:length(names(sum_df))]))

    # check one col to make sure it matches
    col_to_check <- names(sum_df)[3]
    expect_identical(sum_df[[col_to_check]], add_df[[col_to_check]])
  })

  test_that("summary_log() parses heuristics correctly [BBR-SMLG-006]", {
    sum_df2 <- summary_log(MODEL_DIR_X, .fail_flags = list(ext_file = "1001.1.TXT"))

    expect_false(filter(sum_df2, run == "1001") %>% pull(minimization_terminated))
    expect_true(filter(sum_df2, run == "1001") %>% pull(large_condition_number))
    expect_true(filter(sum_df2, run == "1001") %>% pull(prderr))
    expect_true(filter(sum_df2, run == "1001") %>% pull(!!ANY_HEURISTICS))
    expect_true(filter(sum_df2, run == "acop-fake-bayes") %>% pull(eigenvalue_issues))
    expect_true(filter(sum_df2, run == "iovmm") %>% pull(has_final_zero_gradient))
  })

  test_that("summary_log() parses more complex flags and stats [BBR-SMLG-007]", {
    sum_df2 <- summary_log(MODEL_DIR_X, .fail_flags = list(ext_file = "1001.1.TXT"))

    # check fail flag parsed
    expect_equal(sum_df2[[SL_FAIL_FLAGS]], c(TRUE, FALSE, FALSE, FALSE, FALSE))

    # check multiple estimation methods
    num_est_methods <- map_int(sum_df2[["estimation_method"]], length)
    expect_equal(num_est_methods, c(1, 3, 1, 2, 1))
    expect_equal(sum_df2[[OFV_COL]], c(3842.571, 2675.293, 44158.939, -10838.582, 14722.149), tolerance = 0.01)
    expect_equal(sum_df2[[SUMMARY_COND_NUM]], c(4352.941, NA_real_, NA_real_, NA_real_, NA_real_), tolerance = 0.01)
  })

  # THESE TESTS NEEDS TO BE LAST BECAUSE IT DELETES NECESSARY FILES
  fs::file_delete(file.path(LEVEL2_MOD, "1.grd"))

  test_that("summary_log works some failed summaries [BBR-SMLG-008]", {
    sum_df <- summary_log(MODEL_DIR)
    expect_equal(is.na(sum_df$error_msg), c(TRUE, TRUE, TRUE, FALSE))
    expect_equal(ncol(sum_df), SUM_LOG_COLS)

    sum_df <- summary_log(MODEL_DIR, .bbi_args = list(no_grd_file = TRUE))
    test_sum_df(sum_df, c(MOD1_PATH, NEW_MOD2, NEW_MOD3, LEVEL2_MOD), SUM_LOG_COLS)
  })

  test_that("summary_log works all failed summaries [BBR-SMLG-009]", {

    expect_warning({
      sum_df <- summary_log(LEVEL2_DIR)
    }, regexp = "ALL 1 MODEL SUMMARIES FAILED")

    expect_equal(nrow(sum_df), 1)
    expect_equal(names(sum_df), c(ABS_MOD_PATH, RUN_ID_COL, "error_msg"))
    expect_true(all(grepl("--no-grd-file", sum_df$error_msg)))

  })

  test_that("add_summary works all failed summaries [BBR-SMLG-010]", {

    expect_warning({
      sum_df <- run_log(LEVEL2_DIR) %>% add_summary()
    }, regexp = "ALL 1 MODEL SUMMARIES FAILED")

    expect_equal(nrow(sum_df), 1)
    expect_true(all(c(ABS_MOD_PATH, RUN_ID_COL, YAML_TAGS, "error_msg") %in% names(sum_df)))
    expect_true(all(grepl("--no-grd-file", sum_df$error_msg)))

  })

}) # closing withr::with_options
