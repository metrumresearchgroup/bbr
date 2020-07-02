context("Test creating summary logs")

if (Sys.getenv("METWORX_VERSION") == "" && Sys.getenv("DRONE") != "true") {
  skip("test-summary only runs on Metworx or Drone")
}

# constants
MODEL_FILE <- "1.ctl"
MODEL_YAML <- yaml_ext(MODEL_FILE)
MODEL_DIR <- "model-examples"
MOD1_PATH <- file.path(MODEL_DIR, "1")
MOD2_PATH <- file.path(MODEL_DIR, "2")
MOD3_PATH <- file.path(MODEL_DIR, "3")
ALL_PATHS <- c(MOD1_PATH, MOD2_PATH, MOD3_PATH)

# references
NUM_MODS <- length(ALL_PATHS)
MOD_CLASS_LIST <- c("bbi_nonmem_model", "list")
OFV_REF <- 2636.846
PARAM_COUNT_REF <- 7

# helper to run expectations
test_sum_df <- function(sum_df) {
  expect_equal(nrow(sum_df), NUM_MODS)
  expect_true(ncol(sum_df) %in% c(16, 23)) # two options for summary_log() and add_summary()

  # check some columns
  expect_equal(sum_df$absolute_model_path, purrr::map_chr(ALL_PATHS, ~normalizePath(.x)))
  expect_true(all(is.na(sum_df$error_msg)))
  expect_false(any(sum_df$needed_fail_flags))
  expect_equal(sum_df$ofv, rep(OFV_REF, NUM_MODS))
  expect_equal(sum_df$param_count, rep(PARAM_COUNT_REF, NUM_MODS))
  expect_false(any(sum_df$minimization_terminated))
}

setup({
  invisible(copy_model_from(yaml_ext(MOD1_PATH), MOD2_PATH, "model from test-model-summaries.R", .directory = "."))
  invisible(copy_model_from(yaml_ext(MOD1_PATH), MOD3_PATH, "model from test-model-summaries.R", .directory = "."))
  fs::dir_copy(MOD1_PATH, MOD2_PATH)
  fs::dir_copy(MOD1_PATH, MOD3_PATH)
})
teardown({
  if (fs::dir_exists(MOD2_PATH)) fs::dir_delete(MOD2_PATH)
  if (fs::dir_exists(MOD3_PATH)) fs::dir_delete(MOD3_PATH)
  if (fs::file_exists(ctl_ext(MOD2_PATH))) fs::file_delete(ctl_ext(MOD2_PATH))
  if (fs::file_exists(ctl_ext(MOD3_PATH))) fs::file_delete(ctl_ext(MOD3_PATH))
  if (fs::file_exists(yaml_ext(MOD2_PATH))) fs::file_delete(yaml_ext(MOD2_PATH))
  if (fs::file_exists(yaml_ext(MOD3_PATH))) fs::file_delete(yaml_ext(MOD3_PATH))
})

withr::with_options(list(rbabylon.bbi_exe_path = '/data/apps/bbi',
                         rbabylon.model_directory = normalizePath(MODEL_DIR)), {

  #########################################
  # extracting things from summary object
  #########################################

  test_that("summary_log works with list of models input", {
    mods <- purrr::map(c("1", "2", "3"), ~read_model(.x))
    expect_equal(length(mods), NUM_MODS)
    for (.m in mods) {
      expect_equal(class(.m), MOD_CLASS_LIST)
    }

    sum_df <- summary_log(mods)
    test_sum_df(sum_df)

  })

  test_that("summary_log works with character input", {
    sum_df <- summary_log(c("1", "2", "3"))
    test_sum_df(sum_df)
  })

  test_that("summary_log works with numeric input", {
    sum_df <- summary_log(c(1, 2, 3))
    test_sum_df(sum_df)
  })

  test_that("summary_log works with bbi_run_log_df input", {
    sum_df <- run_log() %>% summary_log()
    test_sum_df(sum_df)
  })

  test_that("add_summary works correctly", {
    sum_df <- run_log() %>% add_summary()
    test_sum_df(sum_df)
    expect_identical(sum_df$model_type, rep("nonmem", 3))
    expect_identical(sum_df$yaml_md5, c("ee5a30a015c4e09bc29334188ff28b58", "95df46d60fae0ed80cd9f212f9a6a72d", "912cf4c649bb841322cfd81ad68434ef"))
  })

}) # closing withr::with_options
