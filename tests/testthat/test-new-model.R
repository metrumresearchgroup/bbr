context("Testing function to create or read in model object")

withr::with_options(list(rbabylon.model_directory = NULL), {

  bad_keys <- c(
    ABS_MOD_PATH
  )

  test_that("read_model() returns expected object", {
    expect_equal(read_model("model-examples/1.yaml"), REF_LIST_1)
  })

  test_that("read_model() returns expected object from no ext specified", {
    temp_path <- "model-examples/temp.yaml"
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

  test_that("yaml with no model type will fail", {
    expect_error(
      read_model("test-yaml/zz_fail_no_modtype.yaml"),
      regexp = "Model list must have keys"
    )
  })

  test_that("new_model() creates new YAML file", {
    # TODO: change this pending #179
    temp_yaml <- create_temp_model()
    fs::file_delete(temp_yaml)

    mod1a <- new_model(
      fs::path_ext_remove(temp_yaml),
      .description = "new model test"
    )

    # read model from YAML
    mod1b <- read_model(.path = temp_yaml)

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
    # TODO: change this pending #179
    temp_yaml <- create_temp_model(YAML_TEST_FILE)
    fs::file_delete(temp_yaml)

    # create a new model with arguments known to match the reference model at
    # YAML_TEST_FILE
    mod1a <- new_model(
      fs::path_ext_remove(temp_yaml),
      .description = "original acop model",
      .tags = c("acop tag", "other tag"),
      .bbi_args = list(overwrite = TRUE, threads = 4)
    )

    # read in the reference model
    mod1b <- read_model(.path = YAML_TEST_FILE)

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
    temp_yaml <- create_temp_model(YAML_TEST_FILE)

    # error if file exists
    expect_error(
      new_model(
        fs::path_ext_remove(temp_yaml),
        .description = "fake model"
      ),
      regexp = "File already exists at"
    )

    new_model(
      fs::path_ext_remove(temp_yaml),
      .description = "fake model",
      .overwrite = TRUE
    )

    new_mod <- read_model(temp_yaml)
    expect_equal(new_mod$description, "fake model")
  })


  test_that("new_model() .based_on arg works", {
    # TODO: change this pending #179
    temp_yaml <- create_temp_model(YAML_TEST_FILE)
    parent_model_id <- get_model_id(create_temp_model())
    fs::file_delete(temp_yaml)

    mod1a <- new_model(
      fs::path_ext_remove(temp_yaml),
      .description = "original acop model",
      .based_on = parent_model_id
    )

    expect_equal(mod1a[[YAML_BASED_ON]], parent_model_id)
  })

  test_that("new_model() .based_on arg errors on fake model", {
    # create new model with args
    .test_path <- "model-examples/tmp.yaml"

    expect_error(
      suppressSpecificWarning({
        mod1a <- new_model(
          fs::path_ext_remove(.test_path),
          .description = "original acop model",
          .based_on = c("1", "fake")
        )
      }, "No model file found at.+\\.ctl")
      , regexp = "cannot find .yaml files"
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

}) # closing withr::with_options
