
withr::local_options(list("bbr.verbose" = FALSE))

test_that("nm_tables() works", {
  res <- nm_tables(MOD1)
  expect_equal(length(res), length(MOD1_TABLE_FILES) + 1)
  expect_equal(res$data, nm_data(MOD1))
  expect_equal(res$tab, nm_tab(MOD1))
  expect_equal(res$par.tab, nm_par_tab(MOD1))
})

test_that("nm_tables() works with .files argument [BBR-NMT-001]", {
  res <- nm_tables(MOD1, .files = "1.tab")
  expect_equal(length(res), 2)
  expect_equal(res$data, nm_data(MOD1))
  expect_equal(res$tab, nm_tab(MOD1))
})

test_that("nm_table_files() works", {
  res <- nm_table_files(MOD1)
  expect_true(all(fs::is_absolute_path(res)))
  expect_equal(basename(res), MOD1_TABLE_FILES)
})

test_that("nm_table_files() works with multiline table statement", {
  perturb_file(get_model_path(MOD1), txt = "\n\n$TABLE\n\nFILE=./fake\n")

  res <- nm_table_files(MOD1, .check_exists = FALSE)
  expect_true(all(fs::is_absolute_path(res)))
  expect_equal(basename(res), c(MOD1_TABLE_FILES, "fake"))
})

test_that("nm_table_files() errors with non-existent file", {
  perturb_file(get_model_path(MOD1), txt = "\n$TABLE FILE=./fake\n")
  expect_error(
    nm_table_files(MOD1),
    regexp = "files do not exist.+fake"
  )
})

