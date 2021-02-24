context("checking if models are up to date")

ALL_GOOD <- c(model = TRUE, data = TRUE)
ALL_BAD <- c(model = FALSE, data = FALSE)
MODEL_BAD <- c(model = FALSE, data = TRUE)
DATA_BAD <- c(model = TRUE, data = FALSE)

test_that("check_up_to_date() happy path nonmem", {
  expect_equal(check_up_to_date(MOD1), ALL_GOOD)
})

test_that("check_up_to_date() with mismatched nonmem model", {
  perturb_file(CTL_TEST_FILE)
  expect_equal(check_up_to_date(MOD1), MODEL_BAD)
})

test_that("check_up_to_date() with mismatched nonmem data", {
  perturb_file(get_data_path(MOD1))
  expect_equal(check_up_to_date(MOD1), DATA_BAD)
})

test_that("check_up_to_date() with mismatched nonmem both", {
  perturb_file(CTL_TEST_FILE)
  perturb_file(get_data_path(MOD1))
  expect_equal(check_up_to_date(MOD1), ALL_BAD)
})

test_that("check_up_to_date() with mismatched nonmem summary", {
  skip_if_not_drone_or_metworx("check_up_to_date.bbi_nonmem_summary")
  perturb_file(CTL_TEST_FILE)
  expect_equal(check_up_to_date(SUM1), MODEL_BAD)
})
