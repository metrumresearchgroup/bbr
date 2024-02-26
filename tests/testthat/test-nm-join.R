skip_if_not_drone_or_metworx("test-nm-join")
withr::local_options(list(
  bbr.bbi_exe_path = read_bbi_path(),
  bbr.verbose = FALSE
))

test_that("nm_join() works correctly: defaults and model object [BBR-NMJ-001]", {
  withr::with_options(list(bbr.verbose = TRUE), {
    expect_message({
      test_df <- nm_join(MOD1)
    }, regexp = "join stats")
    expect_equal(nrow(test_df), DATA_TEST_ROWS_IGNORE)
    expect_equal(ncol(test_df), DATA_TEST_COLS + TAB_NEW_COLS + PARTAB_NEW_COLS + 1)
  })
})

test_that("nm_join() works correctly: summary object [BBR-NMJ-001]", {
  test_df <- nm_join(SUM1, .files = TAB_FILE)
  expect_equal(nrow(test_df), DATA_TEST_ROWS_IGNORE)
  expect_equal(ncol(test_df), DATA_TEST_COLS + TAB_NEW_COLS)
})

test_that("nm_join() works correctly: file path [BBR-NMJ-001]", {
  test_df <- nm_join(MOD1_ABS_PATH, .files = TAB_FILE)
  expect_equal(nrow(test_df), DATA_TEST_ROWS_IGNORE)
  expect_equal(ncol(test_df), DATA_TEST_COLS + TAB_NEW_COLS)
})

test_that("nm_join works correctly: .join_col is in the same order as original data [BBR-NMJ-001]", {
  test_df <- nm_join(MOD1, .files = TAB_FILE)
  left_df <- nm_tables(MOD1, .files = TAB_FILE)$tab
  expect_equal(test_df$NUM, left_df$NUM)
})

test_that("nm_join(.superset) works correctly [BBR-NMJ-002]", {
  test_df <- nm_join(MOD1, .files = TAB_FILE, .superset = TRUE)
  expect_equal(nrow(test_df), DATA_TEST_ROWS)
  expect_equal(ncol(test_df), DATA_TEST_COLS + TAB_NEW_COLS)
  expect_equal(sum(is.na(test_df$IPRED)), 20)
})

test_that("nm_join(.superset) works correctly: .join_col is in the same order as original data [BBR-NMJ-002]", {
  test_df <- nm_join(MOD1, .files = TAB_FILE, .superset = TRUE)
  left_df <- nm_tables(MOD1, .files = TAB_FILE)$data
  expect_equal(test_df$NUM, left_df$NUM)
})
####################
# first only tests

test_that("nm_join(.files) works correctly FIRSTONLY: with ID col [BBR-NMJ-003]", {
  test_df <- nm_join(
    MOD1,
    .files = "1first1.tab"
  )
  expect_equal(nrow(test_df), DATA_TEST_ROWS_IGNORE)
  expect_equal(ncol(test_df), DATA_TEST_COLS + 1)
})

test_that("nm_join(.files) works correctly FIRSTONLY: no ID col [BBR-NMJ-003]", {
  test_df <- nm_join(
    MOD1,
    .files = "1first2.tab"
  )
  expect_equal(nrow(test_df), DATA_TEST_ROWS_IGNORE)
  expect_equal(ncol(test_df), DATA_TEST_COLS + 1)
})

test_that("nm_join(.files) works correctly FIRSTONLY: both NUM and ID cols [BBR-NMJ-003]", {
  test_df <- nm_join(
    MOD1,
    .files = "1first3.tab"
  )
  expect_equal(nrow(test_df), DATA_TEST_ROWS_IGNORE)
  expect_equal(ncol(test_df), DATA_TEST_COLS + 1)
})

test_that("nm_join(.files) works correctly FIRSTONLY: then full table [BBR-NMJ-003]", {
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

test_that("nm_join() works correctly with duplicate cols [BBR-NMJ-004]", {
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

test_that("nm_join() records origin of columns in attribute", {
  res <- nm_join(MOD1, .files = c("1.tab", "1par.tab", "1dups.tab"))
  origin <- attr(res, "nm_join_origin")
  sources <- c("data", "tab", "par.tab", "dups.tab")
  for (source in sources) {
    expect_true("NUM" %in% origin[[source]])
  }

  data <- nm_data(MOD1)
  expect_identical(setdiff(origin[["data"]], names(data)), "DV.DATA")

  expect_identical(origin[["par.tab"]], c("NUM", "CL", "V", "KA", "ETA1", "ETA2"))
  expect_identical(origin[["dups.tab"]], c("NUM", "FAKE"))
})

test_that("nm_join(.join_col) works correctly with duplicate cols  [BBR-NMJ-005]", {
  # this test is annoyingly complex to set up because of the
  # mechanics of how the data is pulled and the internal checks
  # on row number. Just an explanation of why it's so long.
  new_mod <- copy_model_from(MOD1)
  new_mod_out <- get_output_dir(new_mod, .check_exists = F)
  copy_output_dir(MOD1, new_mod_out)

  data_path <- "fake_data.csv"
  full_data_path <- file.path(dirname(get_data_path(MOD1)), data_path)

  withr::defer({
    cleanup()
    if(fs::file_exists(full_data_path)) fs::file_delete(full_data_path)
  })

  # create fake data
  fake_data_df <- new_mod %>%
    nm_data() %>%
    mutate(BUM = .data$NUM)
  readr::write_csv(
    fake_data_df,
    full_data_path
  )

  # Rewrite ctl and json to point to fake data
  path_rel <- get_data_path_from_ctl(new_mod, normalize = FALSE)
  path_new <- file.path(dirname(path_rel), basename(full_data_path))
  modify_data_path_ctl(new_mod, path_new)
  modify_data_path_json(new_mod, path_new)

  # create fake table
  new_tab <- "fake.tab"
  readr::write_lines(c(
    "TABLE NO. 1",
    "BUM TUM",
    paste(1:DATA_TEST_ROWS_IGNORE, "A")
  ), file.path(new_mod_out, new_tab))

  # join and check
  test_df <- nm_join(new_mod, .files = new_tab, .join_col = "bum")
  expect_equal(nrow(test_df), DATA_TEST_ROWS_IGNORE)
  expect_equal(ncol(test_df), DATA_TEST_COLS + 2)
  expect_equal(test_df$NUM, test_df$BUM)
})

########################
# warnings and messages

test_that("nm_join() warns on skipping table with wrong number of rows [BBR-NMJ-006]", {
  .tf <- tempfile()
  withr::defer(fs::file_delete(.tf))
  readr::write_lines("TABLE NO 1\na,b\n1,2\n3,4\n", .tf)

  expect_warning({
    test_df <- nm_join(MOD1, .files = .tf)
  }, regexp = "skipped because number of rows")
  expect_equal(nrow(test_df), DATA_TEST_ROWS)
  expect_equal(ncol(test_df), DATA_TEST_COLS)
})

test_that("Confirming unduplicates rows on .join_col [BBR-NMJ-007]",{
  withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {
    withr::with_tempdir({
      temp_dir <- file.path(tempdir(), "basic")
      new_mod_path <- file.path(temp_dir, "1")
      fs::dir_copy(system.file("model","nonmem", "basic", package = "bbr"), temp_dir)
      on.exit(if(fs::dir_exists(temp_dir)) fs::dir_delete(temp_dir))
      fs::file_copy(system.file("extdata", "acop.csv", package = "bbr"), temp_dir, overwrite = TRUE)

      # Test duplicate rows in .tab file
      # Edit Table File to Create Duplicate NUM Row
      file.path(new_mod_path, "1.tab") %>% read_lines() %>%
        str_replace("2.0000E","1.0000E") %>% write_lines(file.path(new_mod_path, "1.tab"))

      # Rewrite ctl and json to point to new data
      # Rewriting both to silence the warning that paths are different
      modify_data_path_json(read_model(new_mod_path), "../acop.csv")
      modify_data_path_ctl(read_model(new_mod_path), "../acop.csv")

      # expect duplicate rows in .tab file
      expect_error(new_mod_path %>% nm_join(.files = "1.tab"), "Duplicate rows")

      # Test duplicate rows in input data
      fs::file_copy(system.file("model","nonmem", "basic","1", "1.tab", package = "bbr"),
                    file.path(new_mod_path, "1.tab"), overwrite = TRUE)
      .d <- readr::read_csv(file.path(temp_dir, "acop.csv"))
      .d$num[2] <- 1
      readr::write_csv(.d, file.path(temp_dir, "acop.csv"))

      # expect duplicate rows in input data
      expect_error(new_mod_path %>% nm_join(.files = "1.tab"), "Duplicate rows")
    })
  })
})
