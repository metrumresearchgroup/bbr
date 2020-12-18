context("Test bbi summary on multiple models")

skip_if_not_drone_or_metworx("test-model-summaries")

# references
NUM_MODS <- 3

# helper to run expectations
test_mod_sums <- function(mod_sums) {
  expect_equal(length(mod_sums), NUM_MODS)

  ref_sum <- dget(SUMMARY_REF_FILE)

  for (.s in mod_sums) {
    ref_sum[[ABS_MOD_PATH]] <- .s$bbi_summary[[ABS_MOD_PATH]]
    expect_equal(ref_sum, .s$bbi_summary)
  }
}

setup({
  cleanup()
  create_rlg_models()
  fs::dir_copy(MOD1_PATH, NEW_MOD2)
  fs::dir_copy(MOD1_PATH, NEW_MOD3)
})
teardown({
  cleanup()
})

withr::with_options(list(rbabylon.bbi_exe_path = read_bbi_path()), {

  #########################################
  # extracting things from summary object
  #########################################

  test_that("model_summaries.list produces expected output", {
    mods <- purrr::map(file.path(MODEL_DIR, seq(3)), read_model)
    expect_equal(length(mods), NUM_MODS)
    for (.m in mods) {
      expect_equal(class(.m), MOD_CLASS_LIST)
    }

    mod_sums <- model_summaries(mods)
    test_mod_sums(mod_sums)

  })

  test_that("model_summaries.list fails with bad list", {
    bad_mods <- list(read_model(file.path(MODEL_DIR, 1)), list(naw = "dawg"))
    expect_equal(length(bad_mods), 2)
    expect_equal(class(bad_mods[[1]]), MOD_CLASS_LIST)

    expect_error(model_summaries(bad_mods), regexp = "must contain only model objects")
  })

  test_that("model_summaries.bbi_run_log_df produces expected output", {
    mod_sums <- run_log(MODEL_DIR) %>% model_summaries()
    test_mod_sums(mod_sums)
  })


  test_that("as_summary_list.bbi_summary_log_df works", {
    mod_sums <- summary_log(MODEL_DIR) %>% as_summary_list()
    test_mod_sums(mod_sums)
  })

}) # closing withr::with_options
