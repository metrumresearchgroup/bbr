
test_that("nm_file() works [BBR-NMF-001]", {
  .d <- nm_file(MOD1, ".cov")
  expect_equal(ncol(.d), MOD1_PARAM_COUNT+1)
  expect_equal(nrow(.d), MOD1_PARAM_COUNT)
})

test_that("nm_ext() works [BBR-NMF-002]", {
  .d <- nm_ext(MOD1)
  expect_equal(ncol(.d), MOD1_PARAM_COUNT+2)
  expect_true(nrow(.d) > 5) # this changes enough, not worth testing exactly
})

test_that("nm_grd() works [BBR-NMF-003]", {
  .d <- nm_grd(MOD1)
  expect_equal(ncol(.d), MOD1_PARAM_COUNT_FIXED+1)
  expect_true(nrow(.d) > 5) # this changes enough, not worth testing exactly
  expect_true(all(
    names(.d)[1] == "ITERATION",
    stringr::str_detect(names(.d)[2:ncol(.d)], "THETA|SIGMA|OMEGA")
  ))
})

test_that("nm_grd() works .rename=FALSE [BBR-NMF-003]", {
  .d <- nm_grd(MOD1, .rename = FALSE)
  expect_equal(ncol(.d), MOD1_PARAM_COUNT_FIXED+1)
  expect_true(nrow(.d) > 5) # this changes enough, not worth testing exactly
  expect_true(all(
    names(.d)[1] == "ITERATION",
    stringr::str_detect(names(.d)[2:ncol(.d)], "GRD")
  ))
})

test_that("nm_file(.est_method) works [BBR-NMF-004]", {
  .m <- read_model(file.path(MODEL_DIR_X,"example2_saemimp"))
  .d <- nm_file(.m, ".ext")
  .d1 <- nm_file(.m, ".ext", 1)
  .d2 <- nm_file(.m, ".ext", 2)

  expect_equal(.d, .d2)
  expect_true(ncol(.d) == ncol(.d1))
  expect_false(nrow(.d) == nrow(.d1))
})

test_that("nm_file(.est_method) works for nm_ext() [BBR-NMF-004]", {
  .m <- read_model(file.path(MODEL_DIR_X,"example2_saemimp"))
  .d <- nm_ext(.m)
  .d1 <- nm_ext(.m, 1)
  .d2 <- nm_ext(.m, 2)

  expect_equal(.d, .d2)
  expect_true(ncol(.d) == ncol(.d1))
  expect_false(nrow(.d) == nrow(.d1))
})

test_that("nm_file(.est_method) errors with invalid .est_method [BBR-NMF-004]", {
  expect_error(nm_ext(.m, 3))
})

test_that("nm_data() works [BBR-NMF-005]", {
  expect_message({
    .d <- nm_data(MOD1)
  }, regexp = "Reading.+acop")

  expect_equal(ncol(.d), DATA_TEST_COLS)
  expect_equal(nrow(.d), DATA_TEST_ROWS)
})


test_that("nm_tab() works [BBR-NMF-006]", {
  .d <- nm_tab(MOD1)
  expect_equal(ncol(.d), 8)
  expect_equal(nrow(.d), DATA_TEST_ROWS_IGNORE)
})

test_that("nm_par_tab() works [BBR-NMF-007]", {
  .d <- nm_par_tab(MOD1)
  expect_equal(ncol(.d), 6)
  expect_equal(nrow(.d), DATA_TEST_ROWS_IGNORE)
})
