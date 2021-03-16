context("testing submitting Stan models")

skip_if_no_stan("skipping Stan submit_model tests because no Stan")
skip_long_tests("skipping long-running Stan submit_model tests")

# define constants
MODEL_DIR_STAN_TEST <- file.path(dirname(STAN_ABS_MODEL_DIR), "test-workflow-stan-models")

# cleanup function
cleanup_bbi <- function(.recreate_dir = FALSE) {
  if (fs::dir_exists(MODEL_DIR_STAN_TEST)) fs::dir_delete(MODEL_DIR_STAN_TEST)
  if (isTRUE(.recreate_dir)) fs::dir_create(MODEL_DIR_STAN_TEST)
}
cleanup_bbi(.recreate_dir = TRUE)


# cleanup when done
on.exit({
  Sys.sleep(3) # wait for some Stan mess to delete itself
  cleanup_bbi()
})

# copy model file into new model dir
fs::dir_copy(STAN_MOD1_PATH, file.path(MODEL_DIR_STAN_TEST, "fxa"))
fs::file_copy(yaml_ext(STAN_MOD1_PATH), MODEL_DIR_STAN_TEST)

#######################
# create model from R
#######################

test_that("step by step create_model to submit_model to model_summary works", {
  # create model
  mod1 <- read_model(file.path(MODEL_DIR_STAN_TEST, STAN_MOD_ID))
  mod2 <- copy_model_from(mod1, STAN_MOD_ID2, .add_tags = "child")

  # submit model
  res_output <- capture.output(
    res2 <- submit_model(
      mod2,
      .mode = "local",
      .overwrite = TRUE,
      iter_warmup = 100,
      iter_sampling = 100
    )
  )
  expect_true(inherits(res2, "CmdStanMCMC"))

  # check sampler diagnostics returns draws array
  smp2 <- res2$sampler_diagnostics()
  expect_true(inherits(smp2, "draws_array"))
  expect_equal(dim(smp2), c(100, 4, 6))

  # check the output for mention of all the chains
  expect_true(all(
    purrr::map_lgl(paste("Chain", 1:4), ~any(str_detect(res_output, .x)))
  ))

  # saves a fit object
  expect_true(fs::file_exists(build_path_from_model(mod2, STAN_MODEL_FIT_RDS)))

  # saves a config with some md5 hashes in it
  cfg_res <- jsonlite::fromJSON(file.path(get_output_dir(mod2), "bbi_config.json"))
  expect_true(any(str_detect(names(cfg_res), "md5$")))

})

test_that("cmdstanr fit object can be reloaded", {
  res2 <- read_model(file.path(MODEL_DIR_STAN_TEST, STAN_MOD_ID2)) %>%
    build_path_from_model(STAN_MODEL_FIT_RDS) %>%
    readRDS()
  expect_true(inherits(res2, "CmdStanMCMC"))

  # verify the sampler_diagnostics() method works
  smp2 <- res2$sampler_diagnostics()
  expect_true(inherits(smp2, "draws_array"))
  expect_equal(dim(smp2), c(100, 4, 6))
})

test_that("run_log() captures runs correctly", {
  # check run log for all models
  log_df <- run_log(MODEL_DIR_STAN_TEST)
  expect_equal(nrow(log_df), 2)
  expect_equal(ncol(log_df), RUN_LOG_COLS)
  expect_identical(basename(log_df[[ABS_MOD_PATH]]), c(STAN_MOD_ID, STAN_MOD_ID2))
})
