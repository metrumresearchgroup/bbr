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


withr::with_options(list(rbabylon.model_directory = NULL), {

  test_that("build_bbi_param_list happy path single set", {
    # read first model
    mod1 <- read_model("model-examples/1")

    # use three copies of the same thing
    .mods <- list(mod1, mod1, mod1)
    param_list <- build_bbi_param_list(.mods)

    # check that there is only one distinct arg set
    expect_equal(length(param_list), 1)

    # check args
    expect_equal(
      param_list[[1]][[YAML_BBI_ARGS]],
      c("--overwrite", "--threads=4")
    )

    # check paths
    expect_equal(
      param_list[[1]][[YAML_MOD_PATH]],
      rep("1.ctl", 3)
    )
  })

  test_that("build_bbi_param_list happy path two sets", {
    # read first model
    mod1 <- read_model("model-examples/1")

    # change one of the args
    mod2 <- mod1
    mod2[[YAML_BBI_ARGS]][["clean_lvl"]] <- 1
    .mods <- list(mod1, mod1, mod2)
    param_list <- build_bbi_param_list(.mods)

    # check that there is only one distinct arg set
    expect_equal(length(param_list), 2)

    # check args
    expect_equal(
      param_list[[1]][[YAML_BBI_ARGS]],
      c("--overwrite", "--threads=4")
    )
    expect_equal(
      param_list[[2]][[YAML_BBI_ARGS]],
      c("--clean_lvl=1", "--overwrite", "--threads=4")
    )

    # check paths
    expect_equal(
      param_list[[1]][[YAML_MOD_PATH]],
      rep("1.ctl", 2)
    )
    expect_equal(
      param_list[[2]][[YAML_MOD_PATH]],
      "1.ctl"
    )
  })

  test_that("build_bbi_param_list .bbi_args works", {
    # read first model
    mod1 <- read_model("model-examples/1")

    # use three copies of the same thing
    .mods <- list(mod1, mod1, mod1)
    param_list <- build_bbi_param_list(.mods, .bbi_args = list(clean_lvl=1))

    # check args
    expect_equal(
      param_list[[1]][[YAML_BBI_ARGS]],
      c("--clean_lvl=1", "--overwrite", "--threads=4")
    )
  })

  test_that("build_bbi_param_list dies with a non model", {
    # read first model
    mod1 <- read_model("model-examples/1")

    # third object is not a model
    .mods <- list(mod1, mod1, list(naw=1))

    # check args
    expect_error(
      param_list <- build_bbi_param_list(.mods),
      regexp = "must contain only model objects"
    )
  })
}) # closing withr::with_options

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



# testing find_config_file_path

BBI_FILE <- file.path(MODEL_DIR, "babylon.yaml")
BBI_DIR <- file.path(MODEL_DIR, "babylon_yaml_test")

readr::write_file("created_by: test-utils", BBI_FILE)
fs::dir_create(BBI_DIR)

tryCatch(
  {
    # cases that should work
    .test_cases <- list(
      list(md = normalizePath("."), ref = "model-examples/babylon.yaml"),
      list(md = normalizePath(MODEL_DIR), ref = "babylon.yaml"),
      list(md = normalizePath(BBI_DIR), ref = "../babylon.yaml")
    )
    for (i in 1:length(.test_cases)) {
      .tc <- .test_cases[[i]]
      test_that(paste("find_config_file_path() parses correctly", i), {
        expect_equal(find_config_file_path(BBI_FILE, .tc$md), .tc$ref)
        expect_equal(find_config_file_path(MODEL_DIR, .tc$md), .tc$ref)
      })
    }

    # cases that should fail
    .test_cases <- list(
      list(bb = BBI_FILE, md = ".", err = "is not absolute"),
      list(bb = basename(BBI_FILE), md = normalizePath("."), err = "No babylon.yaml file exists at")
    )
    for (i in 1:length(.test_cases)) {
      .tc <- .test_cases[[i]]
      test_that(paste("find_config_file_path() errors correctly", i), {
        expect_error(find_config_file_path(.tc$bb, .tc$md), regexp = .tc$err)
      })
    }

  },
  finally = {
    fs::file_delete(BBI_FILE)
    fs::dir_delete(BBI_DIR)
  }
)

