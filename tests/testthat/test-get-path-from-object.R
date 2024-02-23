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
  skip_if_not_drone_or_metworx("get_model_path.bbi_nonmem_summary")
  expect_identical(get_model_path(SUM1), normalizePath(CTL_TEST_FILE))
})

test_that("get_output_dir() builds the right path from summary object [BBR-GPFO-005]", {
  skip_if_not_drone_or_metworx("get_output_dir.bbi_nonmem_summary")
  expect_identical(get_output_dir(SUM1), normalizePath(OUTPUT_DIR))
})

test_that("get_config_path() builds the right path from summary object [BBR-GPFO-026]", {
  skip_if_not_drone_or_metworx("get_config_path.bbi_nonmem_summary")
  expect_identical(get_config_path(SUM1), normalizePath(file.path(OUTPUT_DIR, "bbi_config.json")))
})

test_that("get_yaml_path() builds the right path from summary object [BBR-GPFO-006]", {
  skip_if_not_drone_or_metworx("get_yaml_path.bbi_nonmem_summary")
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
  skip_if_not_drone_or_metworx("get_model_id.bbi_nonmem_summary")
  expect_identical(get_model_id(SUM1), MOD_ID)
})


test_that("get_data_path parses model object [BBR-GPFO-016]", {
  res_data_path <- get_data_path(MOD1)
  expect_identical(res_data_path, DATA_TEST_FILE)
  expect_identical(readLines(res_data_path, n = 1), DATA_TEST_FIRST_LINE)
})

test_that("get_data_path parses summary object [BBR-GPFO-017]", {
  skip_if_not_drone_or_metworx("get_data_path.bbi_nonmem_summary")
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

  # Errors if no config file found and pull_from_config = TRUE
  # First one executes properly, expect error on second model
  expect_error(
    get_data_path(log_df, pull_from_config = TRUE),
    "Cannot extract data path from config file"
  )

  # Copy output dirs and re-check
  copy_output_dir(MOD1, NEW_MOD2)
  copy_output_dir(MOD1, NEW_MOD3)
  expect_equal(
    get_data_path(log_df,  pull_from_config = FALSE),
    get_data_path(log_df,  pull_from_config = TRUE)
  )
})

test_that("get_data_path parses errors informatively", {
  clean_test_enviroment(create_rlg_models)
  mod <- read_model(NEW_MOD3)

  # Change relative data path to be one directory off
  # This would be the correct relative directory if using a `.mod` extension
  data_path_rel <- get_data_path_from_ctl(mod)
  path_elements <- unlist(strsplit(data_path_rel, "/"))
  data_path_rel <- paste(path_elements[-1], collapse = "/")

  # Set $DATA file path to be correct for a `.mod` extension
  modify_data_path_ctl(ctl_ext(NEW_MOD3), data_path_rel)
  expect_error(
    get_data_path(mod),
    "Your model ends with a `.ctl` extension"
  )

  # Change to `.mod` extension and check that it now works
  fs::file_move(ctl_ext(NEW_MOD3), mod_ext(NEW_MOD3))
  res_data_path <- get_data_path(mod)
  expect_identical(res_data_path, DATA_TEST_FILE)
  expect_identical(readLines(res_data_path, n = 1), DATA_TEST_FIRST_LINE)

  # Change to some other directory to get normal error
  new_data_path <- "directory/doesnt/exist/acop.csv"
  modify_data_path_ctl(mod_ext(NEW_MOD3), new_data_path)
  expect_error(
    get_data_path(mod),
    "Input data file does not exist or cannot be opened"
  )

  # Confirm expected data path
  expect_equal(
    file.path(MODEL_DIR, new_data_path),
    as.character(fs::path_rel(get_data_path(mod, .check_exists = FALSE)))
  )
})


test_that("get_data_path can pull from config file", {
  on.exit(cleanup())

  withr::with_tempdir({
    temp_dir <-file.path(tempdir(),"basic")
    fs::dir_copy(system.file("model","nonmem", "basic", package = "bbr"), temp_dir)
    on.exit(if( fs::dir_exists(file.path(tempdir(), "basic"))) fs::dir_delete(temp_dir))

    # Move model and run directory to another location
    # Here, the absolute data path as determined by the control stream file
    # should match the one created from the `bbi.json` file since the relative
    # paths remain consistent
    new_mod_path <- file.path(temp_dir, basename(MOD1_PATH))
    mod <- read_model(new_mod_path)
    expect_equal(
      get_data_path(mod, .check_exists = FALSE),
      get_data_path(mod, .check_exists = FALSE, pull_from_config = TRUE)
    )

    # overwrite $DATA record of new model
    data_path_real <- get_data_path(MOD1)
    modify_data_path_ctl(
      mod_path = file.path(temp_dir, ctl_ext(basename(MOD1_ABS_PATH))),
      data_path = basename(data_path_real)
    )

    # expect mismatch now that the relative paths are different
    expect_equal(
      get_data_path(mod, .check_exists = FALSE),
      file.path(temp_dir, basename(data_path_real))
    )
    expect_equal(
      get_data_path(mod, .check_exists = FALSE, pull_from_config = TRUE),
      "/extdata/acop.csv"
    )

    # Copy over MOD1 run files to test with .check_exists (so the json can be found)
    copy_output_dir(mod, file.path(temp_dir, basename(MOD3_ABS_PATH)))
    expect_error(
      get_data_path(mod, pull_from_config = TRUE),
      "Input data file does not exist or cannot be opened"
    )

    # Copy over data and modify json
    fs::file_copy(
      get_data_path(MOD1),
      file.path(temp_dir, basename(data_path_real))
    )
    json <- jsonlite::read_json(file.path(temp_dir, "1", "bbi_config.json"))
    json$data_path <- "../acop.csv"
    jsonlite::write_json(json, file.path(temp_dir, "1", "bbi_config.json"))

    # Data paths are equivalent and valid
    expect_equal(
      get_data_path(mod, pull_from_config = FALSE),
      get_data_path(mod, pull_from_config = TRUE)
    )
  })
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
    skip_if_not_drone_or_metworx(glue::glue("build_path_from_model.bbi_nonmem_summary {ext}"))
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
