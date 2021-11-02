context("Testing nm_join()")

skip_if_not_drone_or_metworx("test-model-summary")

withr::with_options(list(
  bbr.bbi_exe_path = read_bbi_path(),
  bbr.verbose = FALSE
), {

  test_that("nm_join() defaults work correctly", {
    withr::with_options(list(bbr.verbose = TRUE), {
      expect_message({
        test_df <- nm_join(MOD1)
      }, regexp = "data file")
      expect_equal(nrow(test_df), 779)
      expect_equal(ncol(test_df), 20)
    })
  })

  test_that("nm_join() works correctly with file path", {
    test_df <- nm_join(MOD1_ABS_PATH)
    expect_equal(nrow(test_df), 779)
    expect_equal(ncol(test_df), 20)
  })

  test_that("nm_join(.superset) works correctly", {
    test_df <- nm_join(MOD1, .superset = TRUE)
    expect_equal(nrow(test_df), 799)
    expect_equal(ncol(test_df), 20)
    expect_equal(sum(is.na(test_df$PRED)), 20)
  })

  #### first only tests

  test_that("nm_join(.files) works correctly FIRSTONLY with ID col", {
    test_df <- nm_join(
      MOD1,
      .files = build_path_from_model(.mod, "first1.tab")
    )
    expect_equal(nrow(test_df), 779)
    expect_equal(ncol(test_df), 11)
  })

  test_that("nm_join(.files) works correctly FIRSTONLY with no ID col", {
    test_df <- nm_join(
      MOD1,
      .files = build_path_from_model(.mod, "first2.tab")
    )
    expect_equal(nrow(test_df), 779)
    expect_equal(ncol(test_df), 11)
  })

  test_that("nm_join(.files) works correctly FIRSTONLY with both NUM and ID cols", {
    test_df <- nm_join(
      MOD1,
      .files = build_path_from_model(.mod, "first3.tab")
    )
    expect_equal(nrow(test_df), 779)
    expect_equal(ncol(test_df), 11)
  })

  test_that("nm_join(.files) works correctly FIRSTONLY then full table", {
    test_df <- nm_join(
      MOD1,
      .files = c(
        build_path_from_model(.mod, "first1.tab"),
        build_path_from_model(.mod, ".tab")
      )
    )
    expect_equal(nrow(test_df), 779)
    expect_equal(ncol(test_df), 18)
  })

  #####

  test_that("nm_join(.files) works correctly duplicate cols", {
    test_df <- nm_join(
      MOD1,
      .files = build_path_from_model(MOD1, "cl.tab")
    )
    expect_equal(nrow(test_df), 779)
    expect_equal(ncol(test_df), 11) # TODO: is this right?
  })

  test_that("nm_join(.more) works correctly", {
    test_df <- nm_join(
      MOD1,
      .files = build_path_from_model(MOD1, ".tab"),
      .more = build_path_from_model(MOD1, "cl.tab")
    )
    expect_equal(nrow(test_df), 779)
    expect_equal(ncol(test_df), 17) # TODO: is this right?
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
    readr::write_lines("a,b\n1,2\n3,4\n", .tf)

    expect_warning({
      test_df <- nm_join(MOD1, .files = .tf)
    }, regexp = "skipped because nrow")
    expect_equal(nrow(test_df), 799)
    expect_equal(ncol(test_df), 10)
  })

  test_that("nm_join() warns on missing file", {
    expect_warning({
      test_df <- nm_join(MOD1, .more = "naw")
    }, regexp = "table files do not exist")
    expect_equal(nrow(test_df), 779)
    expect_equal(ncol(test_df), 20)
  })

  test_that("nm_join() warns on no files found", {
    expect_warning({
      test_df <- nm_join(MOD1, .files = "naw")
    }, regexp = "[Zz]ero table files found")
    expect_equal(nrow(test_df), 799)
    expect_equal(ncol(test_df), 10)
  })
})
