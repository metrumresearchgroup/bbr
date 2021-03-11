context("building data objects for modeling")

test_that("build_data.bbi_stan_model returns correct list", {
  skip_if_no_stan("build_data.bbi_stan_model returns correct list")

  res <- suppressMessages(build_data(STAN_MOD1))
  ref <- jsonlite::fromJSON(get_data_path(STAN_MOD1))
  expect_equal(res, ref)
})

test_that("build_data.bbi_stan_model write to disk", {
  skip_if_no_stan("build_data.bbi_stan_model returns correct list")

  tmp_path <- tempfile()

  # check for json error first
  expect_error(
    suppressMessages(build_data(STAN_MOD1, .out_path = tmp_path)),
    regexp = "a JSON file will be written"
  )

  # now check for real
  tmp_path <- fs::path_ext_set(tmp_path, ".json")
  suppressMessages(build_data(STAN_MOD1, .out_path = tmp_path))
  on.exit(fs::file_delete(tmp_path))

  expect_equal(
    as.character(tools::md5sum(tmp_path)),
    as.character(tools::md5sum(get_data_path(STAN_MOD1)))
  )
})
