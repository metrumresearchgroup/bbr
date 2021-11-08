skip_if_not_drone_or_metworx("test-nm-join")

withr::local_options(list(
  bbr.bbi_exe_path = read_bbi_path(),
  bbr.verbose = FALSE
))

test_that("nm_join() defaults work correctly", {
  withr::with_options(list(bbr.verbose = TRUE), {
    expect_message({
      test_df <- nm_join(MOD1)
    }, regexp = "join stats")
    expect_equal(nrow(test_df), DATA_TEST_ROWS_IGNORE)
    expect_equal(ncol(test_df), DATA_TEST_COLS + TAB_NEW_COLS + PARTAB_NEW_COLS + 1)
  })
})

test_that("nm_join() works correctly with file path", {
  test_df <- nm_join(MOD1_ABS_PATH, .files = TAB_FILE)
  expect_equal(nrow(test_df), DATA_TEST_ROWS_IGNORE)
  expect_equal(ncol(test_df), DATA_TEST_COLS + TAB_NEW_COLS)
})

test_that("nm_join(.superset) works correctly", {
  test_df <- nm_join(MOD1, .files = TAB_FILE, .superset = TRUE)
  expect_equal(nrow(test_df), DATA_TEST_ROWS)
  expect_equal(ncol(test_df), DATA_TEST_COLS + TAB_NEW_COLS)
  expect_equal(sum(is.na(test_df$IPRED)), 20)
})

####################
# first only tests

test_that("nm_join(.files) works correctly FIRSTONLY with ID col", {
  test_df <- nm_join(
    MOD1,
    .files = "1first1.tab"
  )
  expect_equal(nrow(test_df), DATA_TEST_ROWS_IGNORE)
  expect_equal(ncol(test_df), DATA_TEST_COLS + 1)
})

test_that("nm_join(.files) works correctly FIRSTONLY with no ID col", {
  test_df <- nm_join(
    MOD1,
    .files = "1first2.tab"
  )
  expect_equal(nrow(test_df), DATA_TEST_ROWS_IGNORE)
  expect_equal(ncol(test_df), DATA_TEST_COLS + 1)
})

test_that("nm_join(.files) works correctly FIRSTONLY with both NUM and ID cols", {
  test_df <- nm_join(
    MOD1,
    .files = "1first3.tab"
  )
  expect_equal(nrow(test_df), DATA_TEST_ROWS_IGNORE)
  expect_equal(ncol(test_df), DATA_TEST_COLS + 1)
})

test_that("nm_join(.files) works correctly FIRSTONLY then full table", {
  test_df <- nm_join(
    MOD1,
    .files = c(
      "1first1.tab",
      "1.tab"
    )
  )
  expect_equal(nrow(test_df), DATA_TEST_ROWS_IGNORE)
  expect_equal(ncol(test_df), DATA_TEST_COLS + 1 + TAB_NEW_COLS)
})

######################
# duplicate columns tests

test_that("nm_join() works correctly duplicate cols", {
  test_df <- nm_join(
    MOD1,
    .files = c(
      "1.tab",
      "1par.tab",
      "1dups.tab"
    )
  )
  expect_equal(nrow(test_df), DATA_TEST_ROWS_IGNORE)
  expect_equal(ncol(test_df), DATA_TEST_COLS + TAB_NEW_COLS + PARTAB_NEW_COLS + 1)
  expect_equal(test_df$DV.DATA, test_df$DV)
})

test_that("nm_join(.join_col) works correctly", {
  expect_true(1 == 1)
  # TODO: what other col could we test joining on? ID?
})

########################
# warnings and messages

test_that("nm_join() warns on skipping table with wrong number of rows", {
  .tf <- tempfile()
  withr::defer(fs::file_delete(.tf))
  readr::write_lines("TABLE NO 1\na,b\n1,2\n3,4\n", .tf)

  expect_warning({
    test_df <- nm_join(MOD1, .files = .tf)
  }, regexp = "skipped because number of rows")
  expect_equal(nrow(test_df), DATA_TEST_ROWS)
  expect_equal(ncol(test_df), DATA_TEST_COLS)
})
