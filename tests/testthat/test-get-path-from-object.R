context("Build paths from model object")

########################################
# file path and file name manipulation
########################################

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

test_that("get_model_path() builds the right path from summary object", {
  skip_if_not_drone_or_metworx("get_model_path.bbi_nonmem_summary")
  expect_identical(get_model_path(SUM1), normalizePath(CTL_TEST_FILE))
})

test_that("get_output_dir() builds the right path from summary object", {
  skip_if_not_drone_or_metworx("get_output_dir.bbi_nonmem_summary")
  expect_identical(get_output_dir(SUM1), normalizePath(OUTPUT_DIR))
})

test_that("get_yaml_path() builds the right path from summary object", {
  skip_if_not_drone_or_metworx("get_yaml_path.bbi_nonmem_summary")
  expect_identical(get_yaml_path(SUM1), normalizePath(YAML_TEST_FILE))
})

test_that("get_model_path() works with bbi_*_log_df", {
  create_all_models()
  on.exit(cleanup())

  ref_mod_paths <- as.character(fs::path_ext_set(c(MOD1_ABS_PATH, MOD2_ABS_PATH, MOD3_ABS_PATH, MOD4_ABS_PATH), ".ctl"))
  res_mod_paths <- get_model_path(run_log(MODEL_DIR))

  expect_identical(ref_mod_paths, res_mod_paths)
})

test_that("get_output_dir() works with bbi_*_log_df", {
  expect_identical(get_output_dir(run_log(MODEL_DIR)), normalizePath(OUTPUT_DIR))
})

test_that("get_yaml_path() works with bbi_*_log_df", {
  expect_identical(get_yaml_path(run_log(MODEL_DIR)), normalizePath(YAML_TEST_FILE))
})

test_that("get_model_path() finds .mod path", {
  temp_mod_path <- create_temp_model(mod_ext = "mod")
  temp_mod <- mod_ext(temp_mod_path)
  mod <- read_model(temp_mod_path)
  expect_identical(get_model_path(mod), temp_mod)
})

test_that("get_model_path() errors with both .ctl and .mod paths", {
  on.exit(fs::file_delete(MOD_TEST_FILE))
  fs::file_copy(CTL_TEST_FILE, MOD_TEST_FILE)
  expect_error(get_model_path(MOD1), "Both.+files found")
})

test_that("get_model_path() works no paths found", {
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
  test_that(glue::glue("get_model_id parses {.tc}"), {
    expect_identical(get_model_id(.tc), MOD_ID)
  })
}

test_that("get_model_id parses model object", {
  expect_identical(get_model_id(MOD1), MOD_ID)
})

test_that("get_model_id parses summary object", {
  skip_if_not_drone_or_metworx("get_model_id.bbi_nonmem_summary")
  expect_identical(get_model_id(SUM1), MOD_ID)
})


test_that("get_data_path parses model object", {
  res_data_path <- get_data_path(MOD1)
  expect_identical(res_data_path, DATA_TEST_FILE)
  expect_identical(readLines(res_data_path, n = 1), DATA_TEST_FIRST_LINE)
})

test_that("get_data_path parses summary object", {
  skip_if_not_drone_or_metworx("get_data_path.bbi_nonmem_summary")
  res_data_path <- get_data_path(SUM1)
  expect_identical(res_data_path, DATA_TEST_FILE)
  expect_identical(readLines(res_data_path, n = 1), DATA_TEST_FIRST_LINE)
})


.test_cases <- c(
  LST_TEST_FILE,
  GRD_TEST_FILE,
  EXT_TEST_FILE
)
for (.tc in .test_cases) {
  test_that(glue::glue("build_path_from_model returns correct {tools::file_ext(.tc)} from model object"), {
    expect_identical(build_path_from_model(MOD1, paste0(".", tools::file_ext(.tc))),
                     normalizePath(.tc))
  })

  test_that(glue::glue("build_path_from_model returns correct {tools::file_ext(.tc)} from summary object"), {
    skip_if_not_drone_or_metworx(glue::glue("build_path_from_model.bbi_nonmem_summary {tools::file_ext(.tc)}"))
    expect_identical(build_path_from_model(SUM1, paste0(".", tools::file_ext(.tc))),
                     normalizePath(.tc))
  })
}

test_that("build_path_from_model works with period in extension", {
  expect_identical(
    build_path_from_model(MOD1, "par.tab"),
    as.character(glue::glue("{MOD1_ABS_PATH}/{MOD_ID}par.tab"))
  )
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
