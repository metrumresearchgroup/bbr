context("Test creating summary logs")

if (Sys.getenv("METWORX_VERSION") == "" && Sys.getenv("DRONE") != "true") {
  skip("test-summary only runs on Metworx or Drone")
}

# # constants
# MODEL_FILE <- "1.ctl"
# MODEL_YAML <- yaml_ext(MODEL_FILE)
# MODEL_DIR <- "model-examples"
# MOD1_PATH <- file.path(MODEL_DIR, "1")
# MOD2_PATH <- file.path(MODEL_DIR, "2")
# MOD3_PATH <- file.path(MODEL_DIR, "3")
# ALL_PATHS <- c(MOD1_PATH, MOD2_PATH, MOD3_PATH)
source("data/test-workflow-ref.R") ##### NEED to check that all these are correctly mapped in here ^

# references
MOD_CLASS_LIST <- c("bbi_nonmem_model", "list")
OFV_REF <- 2636.846
PARAM_COUNT_REF <- 7

# helper to run expectations
test_sum_df <- function(sum_df, num_mods) {
  expect_equal(nrow(sum_df), num_mods)
  expect_true(ncol(sum_df) %in% c(17, 24)) # two options for summary_log() and add_summary()

  # check some columns
  expect_equal(sum_df$absolute_model_path, purrr::map_chr(ALL_PATHS, ~normalizePath(.x)))
  expect_true(all(is.na(sum_df$error_msg)))
  expect_false(any(sum_df$needed_fail_flags))
  expect_equal(sum_df$ofv, rep(OFV_REF, num_mods))
  expect_equal(sum_df$param_count, rep(PARAM_COUNT_REF, num_mods))
  expect_false(any(sum_df$minimization_terminated))
}

setup({
  invisible(copy_model_from(yaml_ext(MOD1_PATH), MOD2_PATH, "model from test-model-summaries.R", .directory = "."))
  invisible(copy_model_from(yaml_ext(MOD1_PATH), MOD3_PATH, "model from test-model-summaries.R", .directory = "."))
  fs::dir_copy(MOD1_PATH, MOD2_PATH)
  fs::dir_copy(MOD1_PATH, MOD3_PATH)

  # copy model 1 to level deeper
  fs::dir_create(LEVEL2_DIR)
  invisible(copy_model_from(YAML_TEST_FILE, LEVEL2_MOD, "level 2 copy of 1.yaml", .inherit_tags = TRUE))
  fs::dir_copy(tools::file_path_sans_ext(YAML_TEST_FILE), file.path(LEVEL2_DIR, tools::file_path_sans_ext(basename(YAML_TEST_FILE))))

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

  test_that("summary_log works with bbi_summary_list input", {
    mod_sums <- model_summaries(c(1, 2, 3))
    expect_true(inherits(mod_sums, "bbi_summary_list"))

    sum_df <- mod_sums %>% summary_log()
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

  # THESE TESTS NEEDS TO BE LAST BECAUSE IT DELETES NECESSARY FILES
  fs::file_delete(file.path(MOD3_PATH, "1.grd"))
  fs::file_delete(file.path(MOD2_PATH, "1.grd"))

  test_that("summary_log works all failed summaries", {

    expect_warning({
      sum_df <- c(2,3) %>% summary_log()
    }, regexp = "ALL 2 MODEL SUMMARIES FAILED")

    expect_equal(names(sum_df), c(ABS_MOD_PATH, "error_msg"))
    expect_true(all(grepl("--no-grd-file", sum_df$error_msg)))

  })

  test_that("summary_log works some failed summaries", {
    sum_df <- c(1, 2, 3) %>% summary_log()
    expect_equal(is.na(sum_df$error_msg), c(TRUE, FALSE, FALSE))
    expect_true(ncol(sum_df) > 10) # check that extra columns are added, doesn't matter how many so keeping test unbrittle

    sum_df <- c(1, 2, 3) %>% summary_log(.bbi_args = list(no_grd_file = TRUE))
    test_sum_df(sum_df)
  })

}) # closing withr::with_options
