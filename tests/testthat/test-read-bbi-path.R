context("read_bbi_path() helper function")

test_that("read_bbi_path() looks for environment variable [BBR-RBP-001]", {
  withr::with_envvar(
    c("BBI_EXE_PATH" = "foo"),
    expect_equal(read_bbi_path(), "foo")
  )

  # and uses default if unset
  withr::with_envvar(
    c("BBI_EXE_PATH" = NA_character_),
    {
      expect_equal(read_bbi_path(), BBI_DEFAULT_PATH)
      expect_equal(read_bbi_path(default = "bar"), "bar")
    }
  )

  # check for an error if the arguments are not strings
  expect_error(read_bbi_path(1))
  expect_error(read_bbi_path(default = 1))
})
