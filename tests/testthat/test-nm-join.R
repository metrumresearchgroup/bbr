context("Testing nm_join()")

skip_if_not_drone_or_metworx("test-model-summary")

withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {

  test_that("nm_join() defaults work correctly", {
    expect_message({
      test_df <- nm_join(MOD1)
    }, regexp = "data file")
    expect_equal(nrow(test_df), 799)
    expect_equal(ncol(test_df), 20)
  })

  test_that("nm_join(.superset) works correctly", {
    test_df <- nm_join(MOD1, .superset = TRUE, .verbose = FALSE)
    expect_equal(nrow(test_df), 799) # TODO: should this be different than ^?
    expect_equal(ncol(test_df), 20)

    # TODO: should we test something like `sum(is.na(test_df$PRED)) > 0`? (it's currently not)
  })

  test_that("nm_join(.files) works correctly FIRSTONLY", {
    test_df <- nm_join(
      MOD1,
      .files = build_path_from_model(MOD1, "first.tab"),
      .verbose = FALSE
    )
    expect_equal(nrow(test_df), 799)
    expect_equal(ncol(test_df), 10) # TODO: is this right?
  })

  test_that("nm_join(.files) works correctly duplicate cols", {
    test_df <- nm_join(
      MOD1,
      .files = build_path_from_model(MOD1, "cl.tab"),
      .verbose = FALSE
    )
    expect_equal(nrow(test_df), 799)
    expect_equal(ncol(test_df), 11) # TODO: is this right?
  })

  test_that("nm_join(.more) works correctly", {
    test_df <- nm_join(
      MOD1,
      .files = build_path_from_model(MOD1, ".tab"),
      .more = build_path_from_model(MOD1, "cl.tab"),
      .verbose = FALSE
    )
    expect_equal(nrow(test_df), 799)
    expect_equal(ncol(test_df), 17) # TODO: is this right?
  })


  test_that("nm_join(.join_col) works correctly", {
    expect_true(1 == 1)
    # TODO: what other col could we test joining on?
  })

})
