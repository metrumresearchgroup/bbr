context("test-use-bbi")

skip_if_not(R.version$os == "linux-gnu")
skip_if_offline()

tdir <- normalizePath(tempdir())

test_that("use-bbi works on linux pulling from options", {
  bbi_tmp_path <- file.path(tdir, "bbi1")
  on.exit(unlink(bbi_tmp_path))
  skip_if_over_rate_limit()

  withr::with_options(c(
    'bbr.suppress_interactivity' = TRUE,
    'bbr.bbi_exe_path' = bbi_tmp_path
  ),
  {

    use_bbi(.force = TRUE,
            .quiet = TRUE)
  })
  f_info <- file.info(bbi_tmp_path)
  expect_equal(as.character(f_info$mode), '755')
})

test_that("use-bbi works on linux with path specified", {
  bbi_tmp_path <- file.path(tdir, "bbi2")
  on.exit(unlink(bbi_tmp_path))
  skip_if_over_rate_limit()

  withr::with_options(c('bbr.suppress_interactivity' = TRUE), {
    use_bbi(.path = bbi_tmp_path, .force = TRUE, .quiet = TRUE)
  })
  f_info <- file.info(bbi_tmp_path)
  expect_equal(as.character(f_info$mode), '755')
})


test_that("bbi_version and bbi_current_version match", {
  skip_if_over_rate_limit()

  withr::with_options(list("bbr.bbi_exe_path" = read_bbi_path()), {
    expect_equal(bbi_version(), bbi_current_release())
  })
})


test_that("bbi_version returns nothing with fake bbi", {
  withr::with_options(list("bbr.bbi_exe_path" = "/fake/path/bbi"), {
    expect_equal(bbi_version(), "")
  })
})


test_that("use_bbi .version argument works", {

  skip_if_over_rate_limit()
  bbi_tmp_path <- file.path(tdir, "bbi3")
  on.exit(unlink(bbi_tmp_path))

  test_version <- "v2.1.2"

  withr::with_options(c('bbr.suppress_interactivity' = TRUE), {
    use_bbi(bbi_tmp_path, .version = test_version, .force = TRUE, .quiet = TRUE)
  })
  f_info <- file.info(bbi_tmp_path)
  expect_equal(as.character(f_info$mode), '755')

  v_res <- bbi_version(bbi_tmp_path)
  expect_identical(v_res, sub("^v", "", test_version))

})

