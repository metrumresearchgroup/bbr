context("test-use-bbi")

tdir <- tempdir()
bbi_tmp_path <- file.path(tdir, "bbi")
teardown({
  unlink(bbi_tmp_path)
})

test_that("use-bbi works on linux", {
  skip_if_not(R.version$os == "linux-gnu")
  skip_if_offline()
  withr::with_options(c('rbabylon.suppress_interactivity' = TRUE), {
    use_bbi(tdir)
  })
  f_info <- file.info(bbi_tmp_path)
  expect_equal(as.character(f_info$mode), '755')
})
