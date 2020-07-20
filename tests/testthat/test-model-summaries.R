context("Test bbi summary on multiple models")

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
SUMMARY_REF_FILE <- "data/acop_summary_obj_ref_200616.rds"
SUM_CLASS_LIST <- c("bbi_nonmem_summary", "list")
MOD_CLASS_LIST <- c("bbi_nonmem_model", "list")
RES_NAMES_LIST <- c("absolute_model_path", "bbi_summary", "error_msg", "needed_fail_flags")
NOT_FINISHED_ERR_MSG <- "nonmem_summary.*modeling run has not finished"
NO_LST_ERR_MSG <- "Unable to locate `.lst` file.*NONMEM output folder"

# helper to run expectations
test_mod_sums <- function(mod_sums) {
  expect_equal(length(mod_sums), NUM_MODS)

  ref_sum <- readRDS(SUMMARY_REF_FILE)

  for (.s in mod_sums) {
    expect_equal(names(.s), RES_NAMES_LIST)
    expect_identical(class(.s$bbi_summary), SUM_CLASS_LIST)
    expect_equal(ref_sum, .s$bbi_summary)
  }
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

  test_that("model_summaries.list produces expected output", {
    mods <- purrr::map(c("1", "2", "3"), ~read_model(.x))
    expect_equal(length(mods), NUM_MODS)
    for (.m in mods) {
      expect_equal(class(.m), MOD_CLASS_LIST)
    }

    mod_sums <- model_summaries(mods)
    test_mod_sums(mod_sums)

  })

  test_that("model_summaries.list fails with bad list", {
    bad_mods <- list(read_model(1), list(naw = "dawg"))
    expect_equal(length(bad_mods), 2)
    expect_equal(class(bad_mods[[1]]), MOD_CLASS_LIST)

    expect_error(model_summaries(bad_mods), regexp = "must contain only model objects")
  })

  test_that("model_summaries.character produces expected output", {
    mod_sums <- model_summaries(c("1", "2", "3"))
    test_mod_sums(mod_sums)
  })

  test_that("model_summaries.numeric produces expected output", {
    mod_sums <- model_summaries(c(1, 2, 3))
    test_mod_sums(mod_sums)
  })



  test_that("model_summaries.bbi_run_log_df produces expected output", {
    mod_sums <- run_log() %>% model_summaries()
    test_mod_sums(mod_sums)
  })

}) # closing withr::with_options
