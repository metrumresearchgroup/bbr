context("Test bbi summary on multiple models")

if (Sys.getenv("METWORX_VERSION") == "" && Sys.getenv("DRONE") != "true") {
  skip("test-summary only runs on Metworx or Drone")
}

source("data/test-workflow-ref.R")

# references
NUM_MODS <- 3
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
  cleanup()
  invisible(copy_model_from(yaml_ext(MOD1_PATH), NEW_MOD2, "model from test-model-summaries.R", .directory = "."))
  invisible(copy_model_from(yaml_ext(MOD1_PATH), NEW_MOD3, "model from test-model-summaries.R", .directory = "."))
  fs::dir_copy(MOD1_PATH, NEW_MOD2)
  fs::dir_copy(MOD1_PATH, NEW_MOD3)
})
teardown({
  cleanup()
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
      expect_equal(class(.m), MODEL_CLASS_LIST)
    }

    mod_sums <- model_summaries(mods)
    test_mod_sums(mod_sums)

  })

  test_that("model_summaries.list fails with bad list", {
    bad_mods <- list(read_model(1), list(naw = "dawg"))
    expect_equal(length(bad_mods), 2)
    expect_equal(class(bad_mods[[1]]), MODEL_CLASS_LIST)

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
