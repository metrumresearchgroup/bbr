context("checking Stan model integrity")

skip_if_no_stan("check_stan_model() tests")

test_that("check_stan_object messages missing files", {
  mod_name <- "testmod_check_stan_model1"
  suppressMessages(
    .m <- new_model(mod_name, .model_type = "stan")
  )
  on.exit(cleanup_model(.m))

  fs::file_delete(get_model_path(.m))
  fs::file_delete(build_path_from_model(.m, STANDATA_R_SUFFIX))

  for (.message in c(MISSING_STAN_FILES_ERR_MSG, STAN_MODEL_REQ_FILES)) {
    expect_message(
      check_stan_model(.m),
      regexp = .message
    )
  }
})

test_that("check_stan_object messages scaffold files", {
  mod_name <- "testmod_check_stan_model2"
  suppressMessages(
    .m <- new_model(mod_name, .model_type = "stan")
  )
  on.exit(cleanup_model(.m))

  for (.message in c(STAN_SCAFFOLD_ERR_MSG, STAN_MODEL_REQ_FILES)) {
    expect_message(
      check_stan_model(.m),
      regexp = .message
    )
  }
})
