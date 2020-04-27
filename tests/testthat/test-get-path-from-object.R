context("Build paths from model object")

# source constants and reference objects
source("data/utils-reference.R")

########################################
# file path and file name manipulation
########################################

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

.test_cases <- c(
  LST_TEST_FILE,
  GRD_TEST_FILE,
  EXT_TEST_FILE
)
for (.tc in .test_cases) {
  test_that(glue::glue("build_path_from_mod_obj returns correct {tools::file_ext(.tc)}"), {
    expect_identical(build_path_from_mod_obj(MOD1, tools::file_ext(.tc)),
                     normalizePath(.tc))
  })
}

test_that("find_model_file_path returns correct ctl path", {
  expect_identical(find_model_file_path(CTL_TEST_FILE), basename(CTL_TEST_FILE))
})

test_that("find_model_file_path prefers ctl path", {
  expect_identical(find_model_file_path(MOD_TEST_FILE), basename(CTL_TEST_FILE))
})

test_that("find_model_file_path returns ctl path when no path found", {
  expect_identical(suppressWarnings(find_model_file_path("data/1.mod")), basename(CTL_TEST_FILE))
})

test_that("find_model_file_path returns mod path when only path found", {
  mod_file <- "data/1.mod"
  withr::with_file(mod_file, {
    readr::write_lines(c("naw", "dawg"), mod_file)
    expect_identical(find_model_file_path(mod_file), basename(mod_file))
  })
})


test_that("get_path_from_object() builds the right path", {
  expect_identical(get_path_from_object(MOD1 , YAML_MOD_PATH), normalizePath(CTL_TEST_FILE))
})

test_that("get_model_path() builds the right path", {
  expect_identical(get_model_path(MOD1), normalizePath(CTL_TEST_FILE))
})

test_that("get_output_dir() builds the right path", {
  expect_identical(get_output_dir(MOD1), normalizePath(OUTPUT_DIR))
})

test_that("get_yaml_path() builds the right path from model object", {
  expect_identical(get_yaml_path(MOD1), normalizePath(YAML_TEST_FILE))
})

test_that("get_yaml_path() builds the right path from model file", {
  expect_identical(get_yaml_path(CTL_TEST_FILE), YAML_TEST_FILE)
})

test_that("get_yaml_path() builds the right path from output folder", {
  expect_identical(get_yaml_path(OUTPUT_DIR), YAML_TEST_FILE)
})

test_that("get_yaml_path() builds the right path", {
  # make a new yaml
  new_yaml <- "model-examples/2.yaml"
  fs::file_copy(YAML_TEST_FILE, new_yaml)
  full_new_yaml_path <- normalizePath(new_yaml) # store the full path

  # make a model from it
  suppressSpecificWarning({
    new_mod <- read_model(new_yaml, .directory = NULL)
  }, .regexpr = "No model file found at.+\\.ctl")

  # delete the underlying yaml
  fs::file_delete(new_yaml)

  # errors because it can't find the YAML
  expect_error(get_yaml_path(new_mod))

  # passes if you tell it not to look
  expect_identical(get_yaml_path(new_mod, .check_exists = FALSE), full_new_yaml_path)
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

