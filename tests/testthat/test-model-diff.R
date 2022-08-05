context("model_diff() comparing models")

# setup
cleanup()
create_all_models()
# teardown
withr::defer(cleanup())

test_that("model_diff.bbi_nonmem_model happy path based_on [BBR-MDF-001]", {
  res_object <- model_diff(read_model(MOD2_ABS_PATH))
  expect_true(inherits(res_object, "Diff"))

  res_string <- capture.output(print(res_object))
  expect_true(grepl(
    "PROBLEM.+2.yaml.+PROBLEM.+base",
    paste(res_string, collapse = "\n")
  ))
})

test_that("model_diff.bbi_nonmem_model happy path .mod2 arg [BBR-MDF-002]", {
  res_object <- model_diff(
    read_model(MOD2_ABS_PATH),
    read_model(MOD3_ABS_PATH)
  )
  expect_true(inherits(res_object, "Diff"))

  res_string <- capture.output(print(res_object))
  expect_true(grepl(
    "PROBLEM.+2.yaml.+PROBLEM.+3.yaml",
    paste(res_string, collapse = "\n")
  ))
})


test_that("model_diff.bbi_nonmem_model errors with no based_on [BBR-MDF-003]", {
  expect_error(
    model_diff(MOD1),
    regexp = paste0("no models.+", MODEL_DIFF_ERR_MSG)
  )
})

test_that("model_diff.bbi_nonmem_model errors with multiple based_on [BBR-MDF-004]", {
  mod4 <- read_model(MOD4_ABS_PATH) %>%
    add_based_on("../3")

  expect_error(
    model_diff(mod4),
    regexp = paste0("multiple models.+", MODEL_DIFF_ERR_MSG)
  )
})

test_that("model_diff() hides content of identical models [BBR-MDF-005]", {
  mod <- read_model(MOD1_ABS_PATH)
  expect_message(model_diff(mod, mod), "identical")
})

test_that("model_diff.bbi_stan_model happy path based_on", {
  skip_if_no_stan("model_diff.bbi_stan_model happy path based_on")
  new_mod_name <- "model_diff_stan1"
  mod2 <- copy_model_from(STAN_MOD1, new_mod_name)
  on.exit(cleanup_model(mod2))
  expect_message(res_object <- model_diff(mod2),
                 "identical")
  expect_null(res_object)
})

test_that("model_diff.bbi_stan_model happy path .mod2 arg", {
  skip_if_no_stan("model_diff.bbi_stan_model happy path .mod2 arg")
  new_mod_name <- "model_diff_stan2"
  mod2 <- copy_model_from(STAN_MOD1, new_mod_name)
  on.exit(cleanup_model(mod2))
  expect_message(res_object <- model_diff(STAN_MOD1, .mod2 = mod2),
                 "identical")
  expect_null(res_object)
})

test_that("model_diff.bbi_stan_model works with other files", {
  skip_if_no_stan("model_diff.bbi_stan_model works with other files")
  new_mod_name <- "model_diff_stan3"
  mod2 <- copy_model_from(STAN_MOD1, new_mod_name)
  on.exit(cleanup_model(mod2))
  fs::file_copy(
    build_path_from_model(STAN_MOD1, STANARGS_SUFFIX),
    build_path_from_model(mod2, STANARGS_SUFFIX)
  )

  .test_cases <- list(
    list(file = "standata", content = "make_standata"),
    list(file = "init", content = "make_init"),
    list(file = "stanargs", content = "list")
  )

  for (.tc in .test_cases) {
    expect_message(res_object <- model_diff(mod2, .file = .tc$file),
                   "identical")
    expect_null(res_object)
  }
})
