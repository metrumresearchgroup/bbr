context("test-use-bbi")

skip_if_not(R.version$os == "linux-gnu")
skip_if_offline()

withr::local_options(list(
  bbr.verbose = FALSE,
  bbr.suppress_interactivity = TRUE
))

tdir <- normalizePath(tempdir())

test_that("use-bbi works on linux pulling from options [BBR-UBI-001]", {
  bbi_tmp_path <- file.path(tdir, "bbi1")
  on.exit(unlink(bbi_tmp_path))
  skip_if_over_rate_limit()

  withr::with_options(
    list('bbr.bbi_exe_path' = bbi_tmp_path),
    use_bbi(.force = TRUE)
  )
  f_info <- file.info(bbi_tmp_path)
  expect_equal(as.character(f_info$mode), '755')
})

test_that("use-bbi works on linux with path specified [BBR-UBI-002]", {
  bbi_tmp_path <- file.path(tdir, "to_be_created","bbi2")
  on.exit(unlink(bbi_tmp_path))
  skip_if_over_rate_limit()

  use_bbi(.path = bbi_tmp_path, .force = TRUE)
  f_info <- file.info(bbi_tmp_path)
  expect_equal(as.character(f_info$mode), '755')
})


test_that("bbi_version returns nothing with fake bbi [BBR-UBI-003]", {
  withr::with_options(
    list("bbr.bbi_exe_path" = "/fake/path/bbi"),
    expect_equal(bbi_version(), "")
  )
})


test_that("use_bbi .version argument works [BBR-UBI-004]", {

  skip_if_over_rate_limit()
  bbi_tmp_path <- file.path(tdir, "bbi3")
  on.exit(unlink(bbi_tmp_path))

  test_version <- "v2.1.2"
  use_bbi(bbi_tmp_path, .version = test_version, .force = TRUE)
  f_info <- file.info(bbi_tmp_path)
  expect_equal(as.character(f_info$mode), '755')

  v_res <- bbi_version(bbi_tmp_path)
  expect_identical(v_res, sub("^v", "", test_version))

})

test_that("use-bbi and bbi_version handle path with spaces [BBR-UBI-005]", {
  skip_if_over_rate_limit()

  dir_with_space <- tempfile(pattern = "foo bar")
  dir.create(dir_with_space, recursive = TRUE)
  on.exit(fs::dir_delete(dir_with_space))

  bbi_path <- file.path(dir_with_space, "bbi")
  v <- "3.1.0"

  use_bbi(.path = bbi_path, .version = paste0("v", v))
  expect_true(file.exists(bbi_path))
  expect_equal(bbi_version(bbi_path), v)
})

test_that("use_bbi errors when passed a directory [BBR-UBI-006]", {
  expect_error(use_bbi(.path = REF_DIR), regexp = "existing directory")

  withr::with_options(
    list('bbr.bbi_exe_path' = REF_DIR),
    expect_error(use_bbi(.path = REF_DIR), regexp = "bbi_exe_path")
  )
})

test_that("add_to_path_message() reports needed setup [BBR-UBI-007]", {
  expect_error(add_to_path_message("idontexist"),
               "unexpectedly does not exist")
  withr::with_tempdir({
    fs::file_touch("bbi")
    fs::file_chmod("bbi", mode = "755")
    path <- fs::path_abs("bbi")

    # Target path in effect via option.
    withr::with_options(
      list(bbr.bbi_exe_path = path),
      expect_silent(add_to_path_message(path)))
    withr::with_options(
      list(bbr.bbi_exe_path = "./bbi"),
      expect_silent(add_to_path_message(path)))
    withr::with_options(list(bbr.bbi_exe_path = "foo"), {
      # Target path not in effect via option or PATH.
      expect_message(add_to_path_message(path), "Please either set")
      # Target path in effect via PATH.
      withr::with_envvar(new = c("PATH" = paste0(getwd(), ":")),
                         action = "prefix",
                         expect_silent(add_to_path_message(path)))
    })
  })
})
