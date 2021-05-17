context("adding files to Stan models")

skip_if_no_stan("add_stan_...() tests")

test_that("add_stan_file() works correctly for scaffold", {
  mod_name <- "testmod_add_stan_file1"
  suppressMessages(
    .m <- new_model(mod_name, .model_type = "stan")
  )
  on.exit(cleanup_model(.m))

  # check default
  fs::file_delete(get_model_path(.m))
  .m <- add_stan_file(.m)
  expect_equal(
    STANMOD_SCAFFOLD_MD5,
    as.character(tools::md5sum(get_model_path(.m)))
  )

  # check passing .source_file
  .sf <- get_model_path(STAN_MOD1)
  .m <- add_stan_file(.m, .source_file = .sf)
  expect_equal(
    as.character(tools::md5sum(.sf)),
    as.character(tools::md5sum(get_model_path(.m)))
  )
})

test_that("add_standata_file() works correctly for scaffold", {
  mod_name <- "testmod_add_standata_file1"
  suppressMessages(
    .m <- new_model(mod_name, .model_type = "stan")
  )
  on.exit(cleanup_model(.m))

  # check default
  fs::file_delete(build_path_from_model(.m, STANDATA_R_SUFFIX))
  .m <- add_standata_file(.m)
  expect_equal(
    STANDATA_SCAFFOLD_MD5,
    as.character(tools::md5sum(build_path_from_model(.m, STANDATA_R_SUFFIX)))
  )

  # check passing .source_file
  .sf <- build_path_from_model(STAN_MOD1, STANDATA_R_SUFFIX)
  .m <- add_standata_file(.m, .source_file = .sf)
  expect_equal(
    as.character(tools::md5sum(.sf)),
    as.character(tools::md5sum(build_path_from_model(.m, STANDATA_R_SUFFIX)))
  )
})

test_that("add_stan_init() works correctly for scaffold", {
  mod_name <- "testmod_add_stan_init1"
  suppressMessages(
    .m <- new_model(mod_name, .model_type = "stan")
  )
  on.exit(cleanup_model(.m))

  # check default
  .m <- add_stan_init(.m)
  expect_equal(
    STANINIT_SCAFFOLD_MD5,
    as.character(tools::md5sum(build_path_from_model(.m, STANINIT_SUFFIX)))
  )

  # check passing .source_file
  .sf <- build_path_from_model(STAN_MOD1, STANINIT_SUFFIX)
  .m <- add_stan_init(.m, .source_file = .sf)
  expect_equal(
    as.character(tools::md5sum(.sf)),
    as.character(tools::md5sum(build_path_from_model(.m, STANINIT_SUFFIX)))
  )
})
