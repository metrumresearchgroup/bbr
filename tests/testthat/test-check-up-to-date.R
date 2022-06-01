context("checking if models are up to date")

ALL_GOOD <- c(model = TRUE, data = TRUE)
ALL_BAD <- c(model = FALSE, data = FALSE)
MODEL_BAD <- c(model = FALSE, data = TRUE)
DATA_BAD <- c(model = TRUE, data = FALSE)

test_that("check_up_to_date.bbi_nonmem_model() happy path [BBR-CUTD-001]", {
  expect_equal(check_up_to_date(MOD1), ALL_GOOD)
})

test_that("check_up_to_date.bbi_nonmem_model() with mismatched model [BBR-CUTD-002]", {
  perturb_file(CTL_TEST_FILE)
  expect_message(
    res <- check_up_to_date(MOD1),
    regexp = "The following files have changed.+ctl"
  )
  expect_equal(res, MODEL_BAD)
})

test_that("check_up_to_date.bbi_nonmem_model() with mismatched data [BBR-CUTD-003]", {
  perturb_file(get_data_path(MOD1))
  expect_message(
    res <- check_up_to_date(MOD1),
    regexp = "The following files have changed.+csv"
  )
  expect_equal(res, DATA_BAD)
})

test_that("check_up_to_date.bbi_nonmem_model() with missing data [BBR-CUTD-004]", {
  temp_mod_path <- create_temp_model()
  new_mod <- read_model(temp_mod_path)
  fs::dir_copy(get_output_dir(MOD1), get_output_dir(new_mod, .check_exists = F))
  on.exit(fs::dir_delete(get_output_dir(new_mod)))
  expect_message(
    res <- check_up_to_date(new_mod),
    regexp = "ARE NO LONGER PRESENT.+csv"
  )
  expect_equal(res, ALL_BAD)
})

test_that("check_up_to_date.bbi_nonmem_model() with mismatched both [BBR-CUTD-005]", {
  perturb_file(CTL_TEST_FILE)
  perturb_file(get_data_path(MOD1))
  expect_message(
    res <- check_up_to_date(MOD1),
    regexp = "The following files have changed.+ctl.+csv"
  )
  expect_equal(res, ALL_BAD)
})

test_that("check_up_to_date.bbi_nonmem_summary() with mismatched model [BBR-CUTD-006]", {
  skip_if_not_drone_or_metworx("check_up_to_date.bbi_nonmem_summary")
  perturb_file(CTL_TEST_FILE)
  expect_message(
    res <- check_up_to_date(SUM1),
    regexp = "The following files have changed.+ctl"
  )
  expect_equal(res, MODEL_BAD)
})


test_that("check_up_to_date.bbi_log_df() works as expected [BBR-CUTD-007]", {
  # create models for the run log
  create_all_models()
  copy_all_output_dirs()
  fs::dir_delete(get_output_dir(mod3)) # for testing missing output dir
  on.exit(cleanup())

  log_df <- run_log(MODEL_DIR)
  expect_message(
    res <- check_up_to_date(log_df),
    regexp = "Cannot check"
  )

  ref <- list(
    `1` = ALL_GOOD,
    `2` = MODEL_BAD,
    `3` = as.logical(c(model = NA, data = NA)),
    `1` = ALL_BAD
  )

  expect_equal(ref, res)
})

test_that("check_up_to_date.bbi_stan_model() happy path", {
  skip_if_no_stan("check_up_to_date.bbi_stan_model() happy path")
  expect_equal(check_up_to_date(STAN_MOD1), ALL_GOOD)
})

test_that("check_up_to_date.bbi_stan_model() with mismatched model", {
  skip_if_no_stan("check_up_to_date.bbi_stan_model() with mismatched model")
  perturb_file(build_path_from_model(STAN_MOD1, STANMOD_SUFFIX))
  expect_message(
    res <- check_up_to_date(STAN_MOD1),
    regexp = "The following files have changed.+\\.stan"
  )
  expect_equal(res, MODEL_BAD)
})

test_that("check_up_to_date.bbi_stan_model() with mismatched data .build_data=TRUE", {
  skip_if_no_stan("check_up_to_date.bbi_stan_model() with mismatched data .build_data=TRUE")
  perturb_file(
    system.file("extdata", "fxa.data.csv", package = "bbr"),
    txt = paste(rep(99, 8), collapse = ",")
  )

  expect_message(
    res <- check_up_to_date(STAN_MOD1),
    regexp = "The following files have changed.+standata\\.R.+different results"
  )
  expect_equal(res, DATA_BAD)
})

test_that("check_up_to_date.bbi_stan_model() with mismatched data .build_data=F", {
  skip_if_no_stan("check_up_to_date.bbi_stan_model() with mismatched data .build_data=F")
  perturb_file(build_path_from_model(STAN_MOD1, STANDATA_JSON_SUFFIX))
  expect_message(
    res <- check_up_to_date(STAN_MOD1, .build_data = FALSE),
    regexp = "The following files have changed.+standata\\.json"
  )
  expect_equal(res, DATA_BAD)
})
