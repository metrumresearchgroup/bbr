
# WIP: STILL NEED TO TEST
# * nm_tab()
# * nm_par_tab()

test_that("nm_file() works with .cov", {
  .d <- nm_file(MOD1, ".cov")
  expect_equal(ncol(.d), MOD1_PARAM_COUNT+1)
  expect_equal(nrow(.d), MOD1_PARAM_COUNT)
})

test_that("nm_ext() works", {
  .d <- nm_ext(MOD1)
  expect_equal(ncol(.d), MOD1_PARAM_COUNT+2)
  expect_true(nrow(.d) > 5) # this changes enough, not worth testing exactly
})

test_that("nm_grd() works", {
  .d <- nm_grd(MOD1)
  expect_equal(ncol(.d), MOD1_PARAM_COUNT_FIXED+1)
  expect_true(nrow(.d) > 5) # this changes enough, not worth testing exactly
})

test_that("nm_file(.est_method) works", {
  .m <- read_model(file.path(MODEL_DIR_X,"example2_saemimp"))
  .d <- nm_file(.m, ".ext")
  .d1 <- nm_file(.m, ".ext", 1)
  .d2 <- nm_file(.m, ".ext", 2)

  expect_equal(.d, .d2)
  expect_true(ncol(.d) == ncol(.d1))
  expect_false(nrow(.d) == nrow(.d1))
})

test_that("nm_ext(.est_method) works", {
  .m <- read_model(file.path(MODEL_DIR_X,"example2_saemimp"))
  .d <- nm_ext(.m)
  .d1 <- nm_ext(.m, 1)
  .d2 <- nm_ext(.m, 2)

  expect_equal(.d, .d2)
  expect_true(ncol(.d) == ncol(.d1))
  expect_false(nrow(.d) == nrow(.d1))
})

test_that("nm_file(.est_method) errors with invalid .est_method", {
  expect_error(nm_ext(.m, 3))
})

test_that("nm_data() works", {
  expect_message({
    .d <- nm_data(MOD1)
  }, regexp = "Reading.+acop")

  expect_equal(ncol(.d), 10)
  expect_equal(nrow(.d), 799)
})
