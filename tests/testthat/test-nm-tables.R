
withr::local_options(list("bbr.verbose" = FALSE))

test_that("nm_tables() works: model object [BBR-NMT-001]", {
  res <- nm_tables(MOD1)
  expect_equal(length(res), length(MOD1_TABLE_FILES) + 1)
  expect_equal(res$data, nm_data(MOD1))
  expect_equal(res$tab, nm_tab(MOD1))
  expect_equal(res$par.tab, nm_par_tab(MOD1))
})

test_that("nm_tables() works: summary object [BBR-NMT-001]", {
  skip_if_not_drone_or_metworx("nm_tables() summary object")
  res <- nm_tables(SUM1, .files = TAB_FILE)
  expect_equal(length(res), 2)
  expect_equal(res$data, nm_data(MOD1))
  expect_equal(res$tab, nm_tab(MOD1))
})

test_that("nm_tables() works: file path [BBR-NMT-001]", {
  res <- nm_tables(MOD1_ABS_PATH, .files = TAB_FILE)
  expect_equal(length(res), 2)
  expect_equal(res$data, nm_data(MOD1))
  expect_equal(res$tab, nm_tab(MOD1))
})

test_that("nm_tables() works: .files argument [BBR-NMT-001]", {
  # this test is duplicative because others use the .files arg
  # but none test it explicitly
  res <- nm_tables(MOD1, .files = TAB_FILE)
  expect_equal(length(res), 2)
  expect_equal(res$data, nm_data(MOD1))
  expect_equal(res$tab, nm_tab(MOD1))
})

test_that("nm_tables() naming works correctly [BBR-NMT-002]", {
  # construct fake tables with test case names
  table_lines <- c(
    "TABLE NO. 1",
    "NUM VAR",
    "1 2"
  )
  table_names <- file.path(
    get_output_dir(MOD1),
    c("satab1", "catab.1", "par-tab")
  )
  purrr::walk(table_names, ~readr::write_lines(table_lines, .x))
  withr::defer({
    purrr::walk(table_names, ~ {if (fs::file_exists(.x)) fs::file_delete(.x)})
  })

  res <- nm_tables(MOD1, .files = c("1.tab", table_names))
  expect_equal(length(res), 2 + length(table_names))
  expect_equal(names(res), c("data", "tab", "satab", "catab", "par.tab"))
})

test_that("nm_table_files() works [BBR-NMT-003]", {
  res <- nm_table_files(MOD1)
  expect_true(all(fs::is_absolute_path(res)))
  expect_equal(basename(res), MOD1_TABLE_FILES)
})

test_that("nm_table_files() works: multiline table statement [BBR-NMT-003]", {
  perturb_file(get_model_path(MOD1), txt = "\n\n$TABLE\n\nFILE=./fake\n")

  res <- nm_table_files(MOD1, .check_exists = FALSE)
  expect_true(all(fs::is_absolute_path(res)))
  expect_equal(basename(res), c(MOD1_TABLE_FILES, "fake"))
})

test_that("nm_table_files() errors with non-existent file [BBR-NMT-003]", {
  perturb_file(get_model_path(MOD1), txt = "\n$TABLE FILE=./fake\n")
  expect_error(
    nm_table_files(MOD1),
    regexp = "files do not exist.+fake"
  )
})

test_that("nm_table_files() works with indirect filename set (no equals sign) [BBR-NMT-003]", {
  # This tests that "TABLE FILE {filename}" is valid, rather than "TABLE FILE={filename}"
  # Support for this was added via the nmrec dependency
  perturb_file(get_model_path(MOD1), txt = "\n$TABLE NUM CL KA FILE ./fake\n")

  res <- nm_table_files(MOD1, .check_exists = FALSE)
  expect_true(all(fs::is_absolute_path(res)))
  expect_equal(basename(res), c(MOD1_TABLE_FILES, "fake"))
})

test_that("nm_table_files() works if no filename is present [BBR-NMT-003]", {
  # Table records with no specified filename will be filtered out
  perturb_file(get_model_path(MOD1), txt = "\n$TABLE NUM CL KA\n")

  res <- nm_table_files(MOD1, .check_exists = FALSE)
  expect_true(all(fs::is_absolute_path(res)))
  expect_equal(basename(res), MOD1_TABLE_FILES)
})

test_that("nm_table_files() works with quoted file paths [BBR-NMT-003]", {
  new_files <- c("'\"1fake.tab\"'", "\"'2fake.tab'\"")
  new_tables <- paste0(
    glue::glue("\n\n$TABLE NUM CL KA FILE={new_files[1]}\n\n"),
    glue::glue("$TABLE NUM CL KA FILE={new_files[2]}\n\n")
    )
  perturb_file(get_model_path(MOD1), txt = new_tables)

  res <- nm_table_files(MOD1, .check_exists = FALSE)
  expect_true(all(fs::is_absolute_path(res)))
  new_paths <- res[-grep(paste(MOD1_TABLE_FILES, collapse = "|"), res)]

  expect_equal(
    basename(new_paths),
    c("\"1fake.tab\"", "'2fake.tab'")
  )

})
