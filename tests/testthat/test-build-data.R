context("building data objects for modeling")

test_that("build_data.bbi_stan_model returns correct list", {
  skip_if_no_stan("build_data.bbi_stan_model returns correct list")

  res <- suppressMessages(build_data(STAN_MOD1))
  ref <- jsonlite::fromJSON(get_data_path(STAN_MOD1))
  expect_equal(res, ref)
})

test_that("build_data.bbi_stan_model write to disk", {
  skip_if_no_stan("build_data.bbi_stan_model write to disk")

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

test_that("build_data.bbi_stan_model errors with malformed -standata.R", {
  skip_if_no_stan("build_data.bbi_stan_model returns correct list")

  new_mod <- copy_model_from(STAN_MOD1, tempfile())
  on.exit(cleanup_model(new_mod))

  # replace -standata.R with only the first 10 lines
  standata_r_path <- build_path_from_model(new_mod, STANDATA_R_SUFFIX)
  func_string <- readLines(standata_r_path)
  writeLines(func_string[1:10], standata_r_path)

  expect_error(
    build_data(new_mod),
    regexp = "Loading.+FAILED.+unexpected end of input"
  )
})

test_that("build_data.bbi_stan_model errors with -standata.R that doesn't run", {
  skip_if_no_stan("build_data.bbi_stan_model returns correct list")

  new_mod <- copy_model_from(STAN_MOD1, tempfile())
  on.exit(cleanup_model(new_mod))

  # fails because it can't find the relative path to the data from the temp dir
  expect_error(
    build_data(new_mod),
    regexp = "Calling.+FAILED.+fxa.data.csv' does not exist"
  )
})
