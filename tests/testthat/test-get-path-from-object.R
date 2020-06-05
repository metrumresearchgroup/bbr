context("Build paths from model object")

# source constants and reference objects
source("data/utils-reference.R")

########################################
# file path and file name manipulation
########################################

withr::with_options(list(rbabylon.model_directory = NULL), {

  #######################
  # get_path_from_object
  #######################

  test_that("get_path_from_object.default() builds the right path", {
    expect_identical(get_path_from_object(MOD1 , YAML_MOD_PATH), normalizePath(CTL_TEST_FILE))
  })

  test_that("get_path_from_object.character() builds the right path", {
    .mod_path <- file.path(MOD1[[WORKING_DIR]], MOD1[[YAML_MOD_PATH]])
    expect_identical(get_path_from_object(.mod_path , YAML_MOD_PATH), normalizePath(CTL_TEST_FILE))
  })

  test_that("get_path_from_object.character() fails with vector", {
    expect_error(get_path_from_object(c("naw", "dawg") , YAML_MOD_PATH), regexp = "only scaler values are permitted")
  })

  test_that("get_path_from_object.character() .check_exists works", {
    # copy YAML but _not_ model file
    YAML2 <- stringr::str_replace(YAML_TEST_FILE, "/1\\.", "/2.")
    fs::file_copy(YAML_TEST_FILE, YAML2)
    on.exit({ fs::file_delete(YAML2) })

    # should error because no file is there
    expect_error(
      suppressSpecificWarning(get_path_from_object(ctl_ext(YAML2) , YAML_MOD_PATH), .regexpr = "No model file found"),
      regexp = "nothing exists at that location"
    )

    # should work with .check_exists = FALSE
    expect_identical(
      suppressSpecificWarning(get_path_from_object(ctl_ext(YAML2) , YAML_MOD_PATH, .check_exists = FALSE), .regexpr = "No model file found"),
      stringr::str_replace(normalizePath(CTL_TEST_FILE), "/1\\.", "/2.")
    )
  })

  test_that("get_path_from_object.bbi_run_log_df() builds the right paths", {
    # copy the model files and create a fake run log
    YAML2 <- stringr::str_replace(YAML_TEST_FILE, "/1\\.", "/2.")
    CTL2 <- ctl_ext(YAML2)
    fs::file_copy(YAML_TEST_FILE, YAML2)
    fs::file_copy(CTL_TEST_FILE, CTL2)
    on.exit({
      fs::file_delete(YAML2)
      fs::file_delete(CTL2)
    })

    .log_df <- run_log(MODEL_DIR)

    # check extracted paths
    expect_identical(
      get_path_from_object(.log_df , YAML_MOD_PATH),
      c(normalizePath(CTL_TEST_FILE), stringr::str_replace(normalizePath(CTL_TEST_FILE), "/1\\.", "/2."))
    )
  })


  # testing error handling

  test_that("get_path_from_object() errors on missing keys", {
    .test_list <- list(naw = 1)
    expect_error(get_path_from_object(.test_list, "naw"), regexp = "must contain keys")

    .test_list[[WORKING_DIR]] <- "/fake/path"
    expect_error(get_path_from_object(.test_list, "dawg"), regexp = "must contain keys")
  })

  test_that("get_path_from_object() errors on vector field", {
    .test_list <- list(naw = c(1,2))
    .test_list[[WORKING_DIR]] <- "/fake/path"
    expect_error(get_path_from_object(.test_list, "naw"), regexp = "Expected a scaler value")
  })

  test_that("get_path_from_object() errors on fake path", {
    .test_list <- list(naw = 1)
    .test_list[[WORKING_DIR]] <- "/fake/path"
    expect_error(get_path_from_object(.test_list, "naw"), regexp = "nothing exists at that location")
    expect_identical(get_path_from_object(.test_list, "naw", .check_exists = FALSE), "/fake/path/1")
  })

  #########################################
  # helpers that call get_path_from_object
  #########################################

  test_that("get_model_path() builds the right path", {
    expect_identical(get_model_path(MOD1), normalizePath(CTL_TEST_FILE))
  })

  test_that("get_output_dir() builds the right path", {
    expect_identical(get_output_dir(MOD1), normalizePath(OUTPUT_DIR))
  })

  test_that("get_yaml_path() builds the right path", {
    expect_identical(get_yaml_path(MOD1), normalizePath(YAML_TEST_FILE))
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
    expect_true(is_valid_yaml_extension("naw.yml"))
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


  test_that("find_yaml_file_path returns correct yaml path", {
    expect_identical(find_yaml_file_path(YAML_TEST_FILE), YAML_TEST_FILE)
  })

  test_that("find_yaml_file_path returns correct yml path", {
    new_yaml <- paste0(NEW_MOD2, '.yml')
    fs::file_copy(YAML_TEST_FILE, new_yaml)
    on.exit({ fs::file_delete(new_yaml) })
    expect_identical(find_yaml_file_path(NEW_MOD2), new_yaml)

  })

  test_that("find_yaml_file_path errors when no file found", {
    expect_error(find_yaml_file_path(NEW_MOD2), regexp = FIND_YAML_ERR_MSG)
  })

  test_that("find_yaml_file_path errors when two files found", {
    new_yaml <- paste0(tools::file_path_sans_ext(YAML_TEST_FILE), ".yml")
    fs::file_copy(YAML_TEST_FILE, new_yaml)
    on.exit({ fs::file_delete(new_yaml) })
    expect_error(find_yaml_file_path(YAML_TEST_FILE), regexp = "Files found at BOTH")
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
