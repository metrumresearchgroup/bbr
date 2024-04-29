context("Build paths from model object")

########################################
# file path and file name manipulation
########################################

#######################
# get_path_from_object
#######################

test_that("get_model_path() builds the right path [BBR-GPFO-001]", {
  expect_identical(get_model_path(MOD1), normalizePath(CTL_TEST_FILE))
})

test_that("get_output_dir() builds the right path [BBR-GPFO-002]", {
  expect_identical(get_output_dir(MOD1), normalizePath(OUTPUT_DIR))
})

test_that("get_config_path() builds the right path [BBR-GPFO-026]", {
  expect_identical(get_config_path(MOD1), normalizePath(file.path(OUTPUT_DIR, "bbi_config.json")))
})

test_that("get_config_path: .check_exists=FALSE works [BBR-GPFO-028]", {
  ctl_file <- fs::path_abs(CTL_TEST_FILE)
  withr::with_tempdir({
    fs::file_copy(ctl_file, "mod.ctl")
    mod_path <- file.path(getwd(), "mod")
    mod <- new_model(mod_path)

    expect_identical(get_config_path(mod, .check_exists = FALSE),
                     file.path(normalizePath(getwd()),
                               "mod", "bbi_config.json"))

    fs::dir_create(mod_path)
    expect_identical(get_config_path(mod, .check_exists = FALSE),
                     file.path(normalizePath(mod_path), "bbi_config.json"))
  })
})

test_that("get_yaml_path() builds the right path [BBR-GPFO-003]", {
  expect_identical(get_yaml_path(MOD1), normalizePath(YAML_TEST_FILE))
})

test_that("get_model_path() builds the right path from summary object [BBR-GPFO-004]", {
  skip_if_not_ci_or_metworx("get_model_path.bbi_nonmem_summary")
  expect_identical(get_model_path(SUM1), normalizePath(CTL_TEST_FILE))
})

test_that("get_output_dir() builds the right path from summary object [BBR-GPFO-005]", {
  skip_if_not_ci_or_metworx("get_output_dir.bbi_nonmem_summary")
  expect_identical(get_output_dir(SUM1), normalizePath(OUTPUT_DIR))
})

test_that("get_config_path() builds the right path from summary object [BBR-GPFO-026]", {
  skip_if_not_ci_or_metworx("get_config_path.bbi_nonmem_summary")
  expect_identical(get_config_path(SUM1), normalizePath(file.path(OUTPUT_DIR, "bbi_config.json")))
})

test_that("get_yaml_path() builds the right path from summary object [BBR-GPFO-006]", {
  skip_if_not_ci_or_metworx("get_yaml_path.bbi_nonmem_summary")
  expect_identical(get_yaml_path(SUM1), normalizePath(YAML_TEST_FILE))
})

test_that("get_model_path() works with bbi_*_log_df [BBR-GPFO-007]", {
  create_all_models()
  on.exit(cleanup())

  ref_mod_paths <- as.character(fs::path_ext_set(c(MOD1_ABS_PATH, MOD2_ABS_PATH, MOD3_ABS_PATH, MOD4_ABS_PATH), ".ctl"))
  res_mod_paths <- get_model_path(run_log(MODEL_DIR, .recurse = TRUE))

  expect_identical(ref_mod_paths, res_mod_paths)
})

test_that("get_output_dir() works with bbi_*_log_df [BBR-GPFO-008]", {
  expect_identical(get_output_dir(run_log(MODEL_DIR)), normalizePath(OUTPUT_DIR))
})

test_that("get_config_path() works with bbi_*_log_df [BBR-GPFO-026]", {
  expect_identical(get_config_path(run_log(MODEL_DIR)), normalizePath(file.path(OUTPUT_DIR, "bbi_config.json")))
})

test_that("get_yaml_path() works with bbi_*_log_df [BBR-GPFO-009]", {
  expect_identical(get_yaml_path(run_log(MODEL_DIR)), normalizePath(YAML_TEST_FILE))
})

test_that("get_model_path() finds .mod path [BBR-GPFO-010]", {
  temp_mod_path <- create_temp_model(mod_ext = "mod")
  temp_mod <- mod_ext(temp_mod_path)
  mod <- read_model(temp_mod_path)
  expect_identical(get_model_path(mod), temp_mod)
})

test_that("get_model_path() errors with both .ctl and .mod paths [BBR-GPFO-011]", {
  on.exit(fs::file_delete(MOD_TEST_FILE))
  fs::file_copy(CTL_TEST_FILE, MOD_TEST_FILE)
  expect_error(get_model_path(MOD1), "Both.+files found")
})

test_that("get_model_path() works no paths found [BBR-GPFO-012]", {
  temp_mod_path <- create_temp_model(delete_mod = FALSE)
  mod <- read_model(temp_mod_path)
  # save path to model file and then delete the file
  former_ctl_path <- get_model_path(mod)
  fs::file_delete(former_ctl_path)
  expect_error(get_model_path(mod), "No model file found")
  expect_equal(
    get_model_path(mod, .check_exists = FALSE),
    former_ctl_path
  )
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
  test_that(glue::glue("get_model_id parses {.tc} [BBR-GPFO-013]"), {
    expect_identical(get_model_id(.tc), MOD_ID)
  })
}

test_that("get_model_id parses model object [BBR-GPFO-014]", {
  expect_identical(get_model_id(MOD1), MOD_ID)
})

test_that("get_model_id parses summary object [BBR-GPFO-015]", {
  skip_if_not_ci_or_metworx("get_model_id.bbi_nonmem_summary")
  expect_identical(get_model_id(SUM1), MOD_ID)
})


test_that("get_data_path parses model object [BBR-GPFO-016]", {
  res_data_path <- get_data_path(MOD1)
  expect_identical(res_data_path, DATA_TEST_FILE)
  expect_identical(readLines(res_data_path, n = 1), DATA_TEST_FIRST_LINE)
})

test_that("get_data_path parses summary object [BBR-GPFO-017]", {
  skip_if_not_ci_or_metworx("get_data_path.bbi_nonmem_summary")
  res_data_path <- get_data_path(SUM1)
  expect_identical(res_data_path, DATA_TEST_FILE)
  expect_identical(readLines(res_data_path, n = 1), DATA_TEST_FIRST_LINE)
})

test_that("get_data_path parses bbi_*_log_df object", {
  clean_test_enviroment(create_rlg_models)
  log_df <- run_log(MODEL_DIR)

  res_data_path <- get_data_path(log_df)
  expect_identical(res_data_path, rep(DATA_TEST_FILE, nrow(log_df)))
  expect_identical(readLines(res_data_path[1], n = 1), DATA_TEST_FIRST_LINE)
})

test_that("get_data_path works with both model extensions", {
  clean_test_enviroment(create_rlg_models)
  mod <- read_model(NEW_MOD3)

  res_data_path <- get_data_path(mod)
  expect_identical(res_data_path, DATA_TEST_FILE)

  # Change to `.mod` extension
  fs::file_move(ctl_ext(NEW_MOD3), mod_ext(NEW_MOD3))
  expect_error(
    get_data_path(mod),
    "Input data file does not exist or cannot be opened"
  )

  # Set $DATA file path to be correct for a `.mod` extension
  data_path_ctl <- get_data_path_from_ctl(mod, normalize = FALSE)
  data_path_ctl_adj <- adjust_data_path_ext(data_path_ctl, mod_path = ctl_ext(NEW_MOD3))
  modify_data_path_ctl(mod, data_path_ctl_adj)

  res_data_path <- get_data_path(mod)
  expect_identical(res_data_path, DATA_TEST_FILE)
})

test_that("get_data_path_from_ctl works with absolute paths", {
  clean_test_enviroment(create_rlg_models)
  mod <- read_model(NEW_MOD3)

  # Store path as normalized to model directory
  data_path_res <- get_data_path(mod)

  # Set $DATA file path to be be an absolute path
  modify_data_path_ctl(mod, DATA_TEST_FILE)

  # Confirm path was not normalized and nothing changed
  expect_true(
    data_path_res == get_data_path_from_ctl(mod, normalize = FALSE) &&
      data_path_res == get_data_path(mod)
  )
})

test_that("get_data_path parses errors informatively", {
  clean_test_enviroment(create_rlg_models)
  mod <- read_model(NEW_MOD3)

  # Change to some other directory to get normal error
  new_data_path <- "../../../../directory/doesnt/exist/acop.csv"
  modify_data_path_ctl(mod, new_data_path)
  expect_error(
    get_data_path(mod),
    "Input data file does not exist or cannot be opened"
  )
})

test_that("get_data_path can pull from config file", {
  on.exit(cleanup())

  temp_dir_main <- file.path(tempfile(pattern = "get_data_path-"))
  temp_dir <- file.path(temp_dir_main, "basic")
  fs::dir_create(temp_dir, recurse = TRUE)
  fs::dir_copy(system.file("model","nonmem", "basic", package = "bbr"), temp_dir_main)
  on.exit(fs::dir_delete(temp_dir_main))

  # Move model and run directory to another location
  # Here, the absolute data path as determined by the control stream file
  # should match the one created from the `bbi.json` file since the relative
  # paths remain consistent
  new_mod_path <- file.path(temp_dir, basename(MOD1_PATH))
  mod <- read_model(new_mod_path)
  # Absolute paths
  expect_equal(get_data_path_from_ctl(mod), get_data_path_from_json(mod))
  # Defined paths (equivalent because of .ctl extension)
  expect_equal(
    get_data_path_from_ctl(mod, normalize = FALSE),
    get_data_path_from_json(mod, normalize = FALSE)
  )

  # Overwrite $DATA record of new model to cause mismatch
  data_path_real <- get_data_path(MOD1)
  modify_data_path_ctl(mod, basename(data_path_real))
  # Expected path defined in ctl
  expect_equal(get_data_path_from_ctl(mod, normalize = FALSE), basename(data_path_real))

  # Expect mismatch now that the relative/defined paths are different
  expect_false(
    isTRUE(
      all.equal(get_data_path_from_ctl(mod), get_data_path_from_json(mod))
    )
  )
  expect_warning(
    data_path_res <- get_data_path(mod, .check_exists = FALSE),
    "does not match the one defined in the control stream"
  )

  # Check that json path was the one used
  expect_equal(data_path_res, get_data_path_from_json(mod))

  # Copy over data and modify json
  fs::file_copy(
    get_data_path(MOD1),
    file.path(temp_dir, basename(data_path_real))
  )
  json_data_path <- "../acop.csv"
  modify_data_path_json(mod, json_data_path)

  # expected json path
  expect_equal(get_data_path_from_json(mod, normalize = FALSE), json_data_path)

  # Data paths are equivalent and valid again
  expect_equal(get_data_path_from_ctl(mod), get_data_path_from_json(mod))
})

.test_cases <- c(
  LST_TEST_FILE,
  GRD_TEST_FILE,
  EXT_TEST_FILE
)
for (.tc in .test_cases) {
  ext <- tools::file_ext(.tc)
  test_that(glue::glue("build_path_from_model returns correct file from model object: {ext} [BBR-GPFO-018]"), {
    expect_identical(build_path_from_model(MOD1, paste0(".", ext)),
                     normalizePath(.tc))
  })

  test_that(glue::glue("build_path_from_model returns correct file from summary object: {ext} [BBR-GPFO-019]"), {
    skip_if_not_ci_or_metworx(glue::glue("build_path_from_model.bbi_nonmem_summary {ext}"))
    expect_identical(build_path_from_model(SUM1, paste0(".", ext)),
                     normalizePath(.tc))
  })
}

test_that("build_path_from_model works with period in extension [BBR-GPFO-020]", {
  expect_identical(
    build_path_from_model(MOD1, "par.tab"),
    as.character(glue::glue("{MOD1_ABS_PATH}/{MOD_ID}par.tab"))
  )
})

test_that("build_path_from_model works when output directory is missing [BBR-GPFO-027]", {
  ctl_file <- fs::path_abs(CTL_TEST_FILE)
  withr::with_tempdir({
    fs::file_copy(ctl_file, "mod.ctl")
    mod <- new_model("mod")
    expect_identical(build_path_from_model(mod, "-foo"),
                     file.path(normalizePath(getwd()), "mod", "mod-foo"))
  })
})

test_that("is_valid_nonmem_extension() works [BBR-GPFO-021]", {
  expect_true(is_valid_nonmem_extension(MOD_TEST_FILE))
  expect_true(is_valid_nonmem_extension(CTL_TEST_FILE))
  expect_false(is_valid_nonmem_extension(YAML_TEST_FILE))
})

test_that("is_valid_yaml_extension() works [BBR-GPFO-022]", {
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
  test_that(glue::glue("ctl_ext parses {.tc}  [BBR-GPFO-023]"), {
    expect_identical(ctl_ext(.tc), CTL_TEST_FILE)
  })
}

.test_cases <- c(
  OUTPUT_DIR,
  YAML_TEST_FILE,
  CTL_TEST_FILE
)
for (.tc in .test_cases) {
  test_that(glue::glue("mod_ext parses {.tc} [BBR-GPFO-024]"), {
    expect_identical(mod_ext(.tc), MOD_TEST_FILE)
  })
}

.test_cases <- c(
  OUTPUT_DIR,
  MOD_TEST_FILE,
  CTL_TEST_FILE
)
for (.tc in .test_cases) {
  test_that(glue::glue("yaml_ext parses {.tc} [BBR-GPFO-025]"), {
    expect_identical(yaml_ext(.tc), YAML_TEST_FILE)
  })
}
