context("Build paths from model object")

########################################
# file path and file name manipulation
########################################

withr::with_options(list(rbabylon.model_directory = NULL), {

  #######################
  # get_path_from_object
  #######################

  test_that("get_model_path() builds the right path", {
    expect_identical(get_model_path(MOD1), normalizePath(CTL_TEST_FILE))
  })

  test_that("get_output_dir() builds the right path", {
    expect_identical(get_output_dir(MOD1), normalizePath(OUTPUT_DIR))
  })

  test_that("get_yaml_path() builds the right path", {
    expect_identical(get_yaml_path(MOD1), normalizePath(YAML_TEST_FILE))
  })


  # TODO: consider adding a test for logs with more than one model
  test_that("get_model_path() works with bbi_*_log_df", {
    expect_identical(get_model_path(run_log(MODEL_DIR)), normalizePath(CTL_TEST_FILE))
  })

  test_that("get_output_dir() works with bbi_*_log_df", {
    expect_identical(get_output_dir(run_log(MODEL_DIR)), normalizePath(OUTPUT_DIR))
  })

  test_that("get_yaml_path() works with bbi_*_log_df", {
    expect_identical(get_yaml_path(run_log(MODEL_DIR)), normalizePath(YAML_TEST_FILE))
  })

  ##################
  # other functions
  ##################

  .test_cases <- c(
    OUTPUT_DIR,
    YAML_TEST_FILE,
    CTL_TEST_FILE,
    MOD_TEST_FILE,
    LST_TEST_FILE
  )
  for (.tc in .test_cases) {
    test_that(glue::glue("get_model_id parses {.tc}"), {
      expect_identical(get_model_id(.tc), MOD_ID)
    })
  }

  test_that(glue::glue("get_model_id parses model object"), {
    expect_identical(get_model_id(MOD1), MOD_ID)
  })

  test_that("is_valid_nonmem_extension() works", {
    expect_true(is_valid_nonmem_extension(MOD_TEST_FILE))
    expect_true(is_valid_nonmem_extension(CTL_TEST_FILE))
    expect_false(is_valid_nonmem_extension(YAML_TEST_FILE))
  })

  test_that("is_valid_yaml_extension() works", {
    expect_true(is_valid_yaml_extension(YAML_TEST_FILE))
    expect_true(is_valid_yaml_extension("naw.yaml"))
    expect_false(is_valid_yaml_extension(MOD_TEST_FILE))
  })

  .test_cases <- c(
    OUTPUT_DIR,
    YAML_TEST_FILE,
    MOD_TEST_FILE
  )
  for (.tc in .test_cases) {
    test_that(glue::glue("ctl_ext parses {.tc}"), {
      expect_identical(ctl_ext(.tc), CTL_TEST_FILE)
    })
  }

  .test_cases <- c(
    OUTPUT_DIR,
    YAML_TEST_FILE,
    CTL_TEST_FILE
  )
  for (.tc in .test_cases) {
    test_that(glue::glue("mod_ext parses {.tc}"), {
      expect_identical(mod_ext(.tc), MOD_TEST_FILE)
    })
  }

  .test_cases <- c(
    OUTPUT_DIR,
    MOD_TEST_FILE,
    CTL_TEST_FILE
  )
  for (.tc in .test_cases) {
    test_that(glue::glue("yaml_ext parses {.tc}"), {
      expect_identical(yaml_ext(.tc), YAML_TEST_FILE)
    })
  }

  test_that("find_model_file_path returns correct ctl path", {
    expect_identical(find_model_file_path(CTL_TEST_FILE), CTL_TEST_FILE)
  })

  test_that("find_model_file_path prefers ctl path", {
    expect_identical(find_model_file_path(MOD_TEST_FILE), CTL_TEST_FILE)
  })

  test_that("find_model_file_path returns ctl path when no path found", {
    mod_file <- "data/1.mod"
    expect_identical(
      suppressSpecificWarning(find_model_file_path(mod_file), .regexpr = "No model file found"),
      ctl_ext(mod_file)
    )
  })

  test_that("find_model_file_path returns mod path when only path found", {
    mod_file <- "data/1.mod"
    withr::with_file(mod_file, {
      readr::write_lines(c("naw", "dawg"), mod_file)
      expect_identical(find_model_file_path(mod_file), mod_file)
    })
  })

  test_that("combine_directory_path() builds the expected path .directory", {
    res_path <- combine_directory_path(MODEL_DIR, ctl_ext(MOD_ID))
    expect_identical(res_path, ABS_CTL_PATH)
  })

  test_that("combine_directory_path() builds the expected path with NULL .directory", {
    res_path <- combine_directory_path(.directory = NULL, CTL_TEST_FILE)
    expect_identical(res_path, ABS_CTL_PATH)
  })

  test_that("combine_directory_path() builds fake .path in real .directory", {
    res_path <- combine_directory_path(MODEL_DIR, CTL_TEST_FILE)
    expect_identical(res_path, FAKE_CTL_PATH)
  })

  test_that("combine_directory_path() errors with fake .directory", {
    expect_error(combine_directory_path("aaa", CTL_TEST_FILE), regexp = "No such file or directory")
  })

}) # closing withr::with_options
