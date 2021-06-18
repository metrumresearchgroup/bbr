context("reading fit model objects from disk")

test_that("read_fit_model.character works correctly for Stan", {
  skip_if_no_stan("read_fit_model.character works correctly for Stan")
  res <- read_fit_model(STAN_MOD1_PATH)
  expect_true(inherits(res, STAN_FIT_CLASS))

  # verify the sampler_diagnostics() method works
  smp <- res$sampler_diagnostics()
  expect_true(inherits(smp, STAN_SMP_DIAG_CLASS))
  expect_equal(dim(smp), STAN_SMP_DIAG_DIM)
})

test_that("read_fit_model.bbi_stan_model works correctly", {
  skip_if_no_stan("read_fit_model.bbi_stan_model works correctly")
  res <- read_fit_model(STAN_MOD1)
  expect_true(inherits(res, STAN_FIT_CLASS))

  # verify the sampler_diagnostics() method works
  smp <- res$sampler_diagnostics()
  expect_true(inherits(smp, STAN_SMP_DIAG_CLASS))
  expect_equal(dim(smp), STAN_SMP_DIAG_DIM)
})
