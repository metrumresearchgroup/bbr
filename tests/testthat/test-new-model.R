context("Testing function to create or read in model object")

bad_keys <- c(
  ABS_MOD_PATH
)

test_that("read_model() returns expected object", {
  expect_equal(read_model(MOD1_PATH), REF_LIST_1)
})

test_that("read_model() returns expected object from no ext specified", {
  temp_path <- file.path(ABS_MODEL_DIR, "temp.yaml")
  ctl_path <- fs::path_ext_set(temp_path, "ctl")

  fs::file_copy(YAML_TEST_FILE, temp_path)
  readr::write_file("foo", ctl_path)
  on.exit({
    fs::file_delete(temp_path)
    fs::file_delete(ctl_path)
  })

  # check against ref, reading with no extension
  mod2 <- read_model(tools::file_path_sans_ext(temp_path))
  expect_equal(mod2, REF_LIST_TMP)
})

test_that("read_model() can read a model whose path has a period", {
  temp_ctl <- tempfile(pattern = "file.", fileext = ".ctl")
  # ensure that `temp_ctl` exists
  readr::write_file("foo", temp_ctl)

  temp_yaml <- fs::path_ext_set(temp_ctl, ".yaml")
  fs::file_copy(YAML_TEST_FILE, temp_yaml)
  on.exit(fs::file_delete(c(temp_ctl, temp_yaml)))

  mod <- read_model(fs::path_ext_remove(temp_yaml))
  expect_identical(class(mod), MOD_CLASS_LIST)
})

test_that("new_model() creates new YAML file", {
  temp_mod_path <- create_temp_model()
  fs::file_delete(fs::path_ext_set(temp_mod_path, "yaml"))

  mod1a <- new_model(temp_mod_path, .description = "new model test")

  # read model from YAML
  mod1b <- read_model(temp_mod_path)

  # check class and keys are right
  expect_identical(class(mod1a), MOD_CLASS_LIST)
  expect_identical(class(mod1b), MOD_CLASS_LIST)

  expect_true(all(MODEL_REQ_KEYS %in% names(mod1a)))
  expect_true(all(MODEL_REQ_KEYS %in% names(mod1b)))
})

test_that("new_model() throws an error if the model file does not exist", {
  expect_error(new_model("foo.yaml", "bar"), "No model file found")
})

test_that("compare read_model() and new_model() objects", {
  temp_mod_path <- create_temp_model(YAML_TEST_FILE)
  fs::file_delete(fs::path_ext_set(temp_mod_path, "yaml"))

  # create a new model with arguments known to match the reference model at
  # YAML_TEST_FILE
  mod1a <- new_model(
    temp_mod_path,
    .description = "original acop model",
    .tags = c("acop tag", "other tag"),
    .bbi_args = list(overwrite = TRUE, threads = 4)
  )

  # read in the reference model
  mod1b <- read_model(temp_mod_path)

  # check class and keys are right
  expect_identical(class(mod1a), MOD_CLASS_LIST)
  expect_identical(class(mod1b), MOD_CLASS_LIST)

  expect_true(all(MODEL_REQ_KEYS %in% names(mod1a)))
  expect_true(all(MODEL_REQ_KEYS %in% names(mod1b)))

  # also check that some of the required keys have the same value
  for (k in MODEL_REQ_KEYS) {
    # TODO: eventually we will only exclude ABS_MOD_PATH, because that should
    # not in general be equal
    if (!(k %in% bad_keys)) {
      expect_equal(mod1a[[k]], mod1b[[k]])
    }
  }
})

test_that("new_model() .overwrite arg works", {
  temp_mod_path <- create_temp_model(YAML_TEST_FILE)

  # error if file exists
  expect_error(
    new_model(temp_mod_path, .description = "fake model"),
    regexp = "File already exists at"
  )

  # overwrite the description
  new_model(temp_mod_path, .description = "fake model", .overwrite = TRUE)

  new_mod <- read_model(temp_mod_path)
  expect_equal(new_mod$description, "fake model")
})


test_that("new_model() .based_on arg works", {
  temp_mod_path <- create_temp_model(YAML_TEST_FILE)
  parent_model_id <- get_model_id(create_temp_model())
  fs::file_delete(fs::path_ext_set(temp_mod_path, "yaml"))

  mod1a <- new_model(
    temp_mod_path,
    .description = "original acop model",
    .based_on = parent_model_id
  )

  expect_equal(mod1a[[YAML_BASED_ON]], parent_model_id)
})

test_that("new_model() .based_on arg errors on fake model", {
  # create new model with args
  expect_error(
    new_model(
      file.path(ABS_MODEL_DIR, "tmp"),
      .description = "original acop model",
      .based_on = c("1", "fake")
    ),
    regexp = "cannot find .yaml files"
  )
})

test_that("new_model() supports `.path` containing a period", {
  temp_ctl <- tempfile(pattern = "file.", fileext = ".ctl")
  # ensure that `temp_ctl` exists
  readr::write_file("foo", temp_ctl)

  # this will be created by new_model()
  temp_yaml <- fs::path_ext_set(temp_ctl, ".yaml")
  on.exit(fs::file_delete(c(temp_ctl, temp_yaml)))

  mod <- new_model(fs::path_ext_remove(temp_ctl), "path with period")
  expect_true(fs::file_exists(temp_yaml))
})
