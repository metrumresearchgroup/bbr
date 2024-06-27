
withr::local_options(list("bbr.verbose" = FALSE))

test_that("nm_tables() works: model object [BBR-NMT-001]", {
  res <- nm_tables(MOD1)
  expect_equal(length(res), length(MOD1_TABLE_FILES) + 1)
  expect_equal(res$data, nm_data(MOD1))
  expect_equal(res$tab, nm_tab(MOD1))
  expect_equal(res$par.tab, nm_par_tab(MOD1))
})

test_that("nm_tables() works: summary object [BBR-NMT-001]", {
  skip_if_not_ci_or_metworx("nm_tables() summary object")
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

test_that("nm_file_multi_tab() works with multiple tables", {
  ## same rows and columns - reflects a simulation ##
  # Duplicate 1.tab multiple times in 2.tab
  tab1_path <- nm_table_files(MOD1)[1]
  new_tab_path <- file.path(dirname(tab1_path), "2.tab")
  on.exit(fs::file_delete(new_tab_path), add = TRUE)
  fs::file_copy(tab1_path, new_tab_path)
  base_tab_lines <- readLines(new_tab_path)
  perturb_file(new_tab_path, txt = rep(base_tab_lines, 5))

  multi_tab <- nm_file_multi_tab(new_tab_path) %>% suppressMessages()
  expect_equal(dplyr::n_distinct(multi_tab$nn), 6)
  expect_equal(unique(multi_tab$table_name), "TABLE NO.  1")
  expect_equal(names(multi_tab), c(names(nm_tab(MOD1)), "nn", "table_name"))

  ## Test when different types of tables are present ##

  # Rows differ - simplifies to dataframe at the end, but each table has to be
  # read in one at at time
  perturb_file(new_tab_path, txt = base_tab_lines[1:(length(base_tab_lines) - 1)])
  multi_tab2 <- nm_file_multi_tab(new_tab_path) %>% suppressMessages()
  tab_rows <- multi_tab2 %>% dplyr::count(nn) %>% dplyr::pull("n")
  expect_true(inherits(multi_tab2, "data.frame"))
  expect_equal(tab_rows[1:6], rep(779, 6))
  expect_equal(tab_rows[7], 778)

  # Columns differ - returns a list of tables
  tab2_path <- nm_table_files(MOD1)[2]
  perturb_file(new_tab_path, txt = readLines(tab2_path))
  multi_tab3 <- nm_file_multi_tab(new_tab_path) %>% suppressMessages()
  expect_true(inherits(multi_tab3, "list"))
  expect_equal(length(multi_tab3), 8)

  # The first 6 tables should be the same as the original 1.tab table (tab1)
  tab1 <- nm_tab(MOD1) %>% suppressMessages()
  purrr::walk(multi_tab3[1:6], function(lst_tab){
    expect_true(all.equal(tab1, lst_tab %>% dplyr::select(-c(table_name))))
  })

  # 7th table only differs in rows, and doesnt need to be tested again
  # 8th table is identical to the original 1par.tab table (tab2)
  tab2 <- nm_par_tab(MOD1) %>% suppressMessages()
  expect_true(all.equal(tab2, multi_tab3[[8]] %>% dplyr::select(-c(table_name))))
})
