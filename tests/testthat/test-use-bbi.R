context("test-use-bbi")

skip_if_not(R.version$os == "linux-gnu")
skip_if_offline()

tdir <- tempdir()
bbi_tmp_path <- file.path(tdir, "bbi")
teardown({
  unlink(bbi_tmp_path)
})

test_that("use-bbi works on linux", {
  skip_if_over_rate_limit()

  withr::with_options(c('rbabylon.suppress_interactivity' = TRUE), {
    use_bbi(tdir, .force = TRUE, .quiet = TRUE)
  })
  f_info <- file.info(bbi_tmp_path)
  expect_equal(as.character(f_info$mode), '755')
})


test_that("bbi_version and bbi_current_version match", {
  skip_if_over_rate_limit()

  withr::with_options(list("rbabylon.bbi_exe_path" = bbi_tmp_path), {
    expect_equal(bbi_version(), bbi_current_release())
  })
})


test_that("bbi_version returns nothing with fake bbi", {
  withr::with_options(list("rbabylon.bbi_exe_path" = "/fake/path/bbi"), {
    expect_equal(bbi_version(), "")
  })
})


