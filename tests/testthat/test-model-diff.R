context("model_diff() comparing models")

# setup
cleanup()
create_all_models()
# teardown
withr::defer(cleanup())

test_that("model_diff.bbi_nonmem_model happy path based_on", {
  res_object <- model_diff(read_model(MOD2_ABS_PATH))
  expect_true(inherits(res_object, "Diff"))

  res_string <- capture.output(print(res_object))
  expect_true(grepl(
    "PROBLEM.+2.yaml.+PROBLEM.+base",
    paste(res_string, collapse = "\n")
  ))
})

test_that("model_diff.bbi_nonmem_model happy path .mod2 arg", {
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


test_that("model_diff.bbi_nonmem_model errors with no based_on", {
  expect_error(
    model_diff(MOD1),
    regexp = paste0("no models.+", MODEL_DIFF_ERR_MSG)
  )
})

test_that("model_diff.bbi_nonmem_model errors with multiple based_on", {
  mod4 <- read_model(MOD4_ABS_PATH) %>%
    add_based_on("../3")

  expect_error(
    model_diff(mod4),
    regexp = paste0("multiple models.+", MODEL_DIFF_ERR_MSG)
  )
})
