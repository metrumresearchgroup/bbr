context("Utility functions for building args, etc.")

# source constants and reference objects
source("data/utils-reference.R")

# tests

################
# parsing args
################

test_that("check_nonmem_args parses correctly", {
  # check some that should parse correctly
  .arg_list <- list(
    list(list("json" = T, "threads" = 4, "nm_version" = "nm74"), c("--json", "--threads=4", "--nm_version=nm74")), # check flag conversion
    list(list("json" = T, "threads" = 4, debug=F), c("--json", "--threads=4")), # check bool=F not passed through
    list(list("json" = T, "threads" = 4, debug=T), c("--json", "--threads=4", "--debug"))  # check same bool=T is passed through
  )

  for (.a in .arg_list) {
    expect_identical(check_nonmem_args(.a[[1]]), .a[[2]])
  }

  # check some that should error
  .arg_list <- list(
    list("json" = T, "threads" = 4, "json" = F), # need to pass unique keys (json passed twice)
    list("json" = T, "threads" = 4, "naw" = "naw"), # trying to pass a key that doesn't exist in NONMEM_ARGS
    list("json" = T, "threads" = 4, "debug" = "naw") # passing the wrong type of value (char instead of bool)
  )

  for (.a in .arg_list) {
    expect_error(check_nonmem_args(.a))
  }
})


test_that("format_cmd_args parses correctly", {
  # check some that should parse correctly
  .arg_list <- list(
    list(list("json" = T, "threads" = 4), c("json", "threads=4")), # check basic nonmem args
    list(list("json" = T, "threads" = 4, "naw" = "naw"), c("json", "threads=4", "naw=naw")), # check one that's not a nonmem arg
    list(list("json" = T, "threads" = 4, debug=F), c("json", "threads=4")), # check bool=F not passed through
    list(list("json" = T, "threads" = 4, debug=T), c("json", "threads=4", "debug")) # check same bool=T is passed through
  )

  for (.a in .arg_list) {
    expect_identical(format_cmd_args(.a[[1]]), .a[[2]])
  }

  # check some that should error
  .arg_list <- list(
    list(T, 4), # need to pass named list
    list("json" = T, "threads" = 4, "json" = F) # need to pass unique keys (json passed twice)
  )

  for (.a in .arg_list) {
    expect_error(format_cmd_args(.a))
  }

  # check with .collapse=T
  .arg_list <- list(
    list(list("json" = T, "threads" = 4, "naw" = "naw"), "json threads=4 naw=naw"),
    list(list("--json" = T, "--threads" = 4, "--naw" = "naw"), "--json --threads=4 --naw=naw")
  )

  for (.a in .arg_list) {
    expect_identical(format_cmd_args(.a[[1]], .collapse=T), .a[[2]])
  }
})


#####################
# list manipulation
#####################

test_that("parse_args_list() merges lists as expected", {
  # override `naw` with .func_args
  expect_identical(parse_args_list(.func_args = LIST1, .yaml_args = LIST2), list(naw=4, saw="hey", paw=6))
})

test_that("parse_args_list() handles NULL as expected", {
  expect_identical(parse_args_list(NULL, LIST2), LIST2)
  expect_identical(parse_args_list(LIST1, NULL), LIST1)
  expect_identical(parse_args_list(NULL, NULL), list())
})

test_that("parse_args_list() correctly fails if .func_args isn't named", {
  # correctly fails if .func_args isn't named
  expect_error(parse_args_list(list(4,5,6), LIST2))
})


test_that("combine_list_objects() merges lists as expected", {
  expect_identical(combine_list_objects(.new_list = LIST1, .old_list = LIST2), list(naw=4, paw=6, saw="hey"))
})

test_that("combine_list_objects() merges with append=TRUE", {
  expect_identical(combine_list_objects(.new_list = LIST1, .old_list = LIST2, .append = TRUE), list(naw=c(4, 5), paw=6, saw="hey"))
})

test_that("combine_list_objects() correctly fails if .func_args isn't named", {
  # correctly fails if .func_args isn't named
  expect_error(combine_list_objects(list(4,5,6), LIST2))
  expect_error(combine_list_objects(LIST1, list(4,5,6)))
})


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


######################
# assorted utilities
######################

test_that("check_required_keys() works correctly", {
  req_keys <- c("hey", "aww", "naw")
  expect_true(check_required_keys(list(hey = 1, aww = 2, naw = 3), req_keys))
  expect_false(check_required_keys(list(hey = 1, aww = 2), req_keys))
})


test_that("strict_mode_error() works correctly", {
  withr::with_options(list(rbabylon.strict = TRUE), {
    expect_error(strict_mode_error("hello"))
  })
  withr::with_options(list(rbabylon.strict = FALSE), {
    expect_warning(strict_mode_error("hello"))
  })
  withr::with_options(list(rbabylon.strict = "oops"), {
    expect_warning(strict_mode_error("hello"))
  })
})


test_that("suppressSpecificWarning() works", {
  # make a new yaml
  new_yaml <- "model-examples/2.yaml"
  fs::file_copy(YAML_TEST_FILE, new_yaml)

  # make a model from it and suppress the warning
  suppressSpecificWarning({
    new_mod <- read_model(new_yaml, .directory = NULL)
  }, .regexpr = "No model file found at.+\\.ctl")
  expect_true(check_required_keys(new_mod, .req = MODEL_REQ_INPUT_KEYS))

  # make a model from it and expect the warning
  expect_warning({
    suppressSpecificWarning({
      new_mod <- read_model(new_yaml, .directory = NULL)
    }, .regexpr = "No model file found at.+\\.cl") # deleted the 't' so it won't catch it
  }, .regexpr = "No model file found at.+\\.ctl")
  expect_true(check_required_keys(new_mod, .req = MODEL_REQ_INPUT_KEYS))

  # delete the underlying yaml
  fs::file_delete(new_yaml)
})
