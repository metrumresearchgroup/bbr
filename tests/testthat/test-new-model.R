context("Testing function to create or read in model object")

withr::with_options(list(rbabylon.model_directory = NULL), {

  test_that("read_model() returns expected object", {
    expect_equal(read_model("model-examples/1.yaml"), REF_LIST_1)
  })

  test_that("read_model() returns expected object from yml ext", {
    # copy the .yaml to .yml to test
    .yaml_path <- "model-examples/1.yaml"
    .yml_path <- "model-examples/tmp.yml"
    fs::file_copy(.yaml_path, .yml_path)

    # check against ref, reading with .yml extension specified
    mod2 <- suppressSpecificWarning({
      read_model(.yml_path)
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_equal(mod2, REF_LIST_TMP)

    fs::file_delete(.yml_path)
  })

  test_that("read_model() returns expected object from no ext specified", {
    # copy the .yaml to .yml to test
    .yaml_path <- "model-examples/1.yaml"
    .yml_path <- "model-examples/tmp.yml"
    fs::file_copy(.yaml_path, .yml_path)

    # check against ref, reading with no extension
    mod2 <- suppressSpecificWarning({
      read_model(tools::file_path_sans_ext(.yml_path))
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_equal(mod2, REF_LIST_TMP)

    fs::file_delete(.yml_path)
  })


  test_that("yaml with no model type will fail", {
    expect_error(read_model("test-yaml/zz_fail_no_modtype.yaml"), regexp = "Model yaml must have keys")
  })

  test_that("yaml with bad model path will fail", {
    expect_error(read_model("test-yaml/zz_fail_bad_modpath.yaml"), regexp = "must have either a .ctl or .mod extension")
  })

  test_that("yaml with no model path will return ctl", {
    .test_path <- "test-yaml/zz_pass_no_modpath"
    suppressSpecificWarning({
      .spec <- read_model(yaml_ext(.test_path))
    }, .regexpr = "No model file found at.+\\.ctl")
    expect_identical(.spec[[YAML_MOD_PATH]], basename(ctl_ext(.test_path)))
  })

  test_that("new_model() creates new YAML file", {
    .test_path <- "model-examples/tmp.yaml"
    on.exit({ fs::file_delete(.test_path) })

    suppressSpecificWarning({
      mod1a <- new_model(
        .yaml_path = .test_path,
        .description = "new model test"
      )
    }, "No model file found at.+\\.ctl")

    # read model from YAML
    mod1b <- read_model(.path = .test_path)

    # check class and keys are right
    expect_identical(class(mod1a), MOD_CLASS_LIST)
    expect_identical(class(mod1b), MOD_CLASS_LIST)

    expect_true(all(MODEL_REQ_KEYS %in% names(mod1a)))
    expect_true(all(MODEL_REQ_KEYS %in% names(mod1b)))
  })

  test_that("new_model() fails with invalid yaml path", {
    .test_path <- "naw"
    expect_warning(new_model(.yaml_path = .test_path, .description = "naw dawg"), regexp = "Did not pass a YAML extension")
    fs::file_delete(yaml_ext(.test_path))
  })

  test_that("compare read_model() and new_model() objects", {
    # create new model with args
    .test_yaml <- "model-examples/1.yaml"
    .test_path <- "model-examples/tmp.yaml"

    suppressSpecificWarning({
      mod1a <- new_model(
        .yaml_path = .test_path,
        .description = "original acop model",
        .tags = c("acop tag", "other tag"),
        .bbi_args = list(overwrite = TRUE, threads = 4)
      )
    }, "No model file found at.+\\.ctl")

    # read model from YAML
    suppressSpecificWarning({
      mod1b <- read_model(.path = .test_yaml)
    }, "No model file found at.+\\.ctl")

    # check class and keys are right
    expect_identical(class(mod1a), MOD_CLASS_LIST)
    expect_identical(class(mod1b), MOD_CLASS_LIST)

    expect_true(all(MODEL_REQ_KEYS %in% names(mod1a)))
    expect_true(all(MODEL_REQ_KEYS %in% names(mod1b)))

    # also check that some of the required keys have the same value
    for (k in MODEL_REQ_KEYS) {
      if (k == YAML_MOD_PATH) {
        expect_identical(mod1a[[k]], basename(ctl_ext(.test_path)))
        expect_identical(mod1b[[k]], basename(ctl_ext(.test_yaml)))
      } else if (k == YAML_OUT_DIR) {
        expect_identical(mod1a[[k]], basename(tools::file_path_sans_ext(.test_path)))
        expect_identical(mod1b[[k]], basename(tools::file_path_sans_ext(.test_yaml)))
      } else if (k == YAML_YAML_NAME) {
        expect_identical(mod1a[[k]], basename(.test_path))
        expect_identical(mod1b[[k]], basename(.test_yaml))
      } else {
        expect_equal(mod1a[[k]], mod1b[[k]])
      }
    }

    # clean up tmp file
    fs::file_delete(.test_path)
  })

  test_that("new_model() works with yml path", {
    # create new model with args
    .test_yaml <- "model-examples/1.yaml"
    .test_path <- "model-examples/tmp.yml"

    suppressSpecificWarning({
      mod1a <- new_model(
        .yaml_path = .test_path,
        .description = "original acop model",
        .tags = c("acop tag", "other tag"),
        .bbi_args = list(overwrite = TRUE, threads = 4)
      )
    }, "No model file found at.+\\.ctl")

    # read model from YAML
    mod1b <- read_model(.path = .test_yaml)

    # check class and keys are right
    expect_identical(class(mod1a), MOD_CLASS_LIST)
    expect_identical(class(mod1b), MOD_CLASS_LIST)

    expect_true(all(MODEL_REQ_KEYS %in% names(mod1a)))
    expect_true(all(MODEL_REQ_KEYS %in% names(mod1b)))

    # also check that some of the required keys have the same value
    for (k in MODEL_REQ_KEYS) {
      if (k == YAML_MOD_PATH) {
        expect_identical(mod1a[[k]], basename(ctl_ext(.test_path)))
        expect_identical(mod1b[[k]], basename(ctl_ext(.test_yaml)))
      } else if (k == YAML_OUT_DIR) {
        expect_identical(mod1a[[k]], basename(tools::file_path_sans_ext(.test_path)))
        expect_identical(mod1b[[k]], basename(tools::file_path_sans_ext(.test_yaml)))
      } else if (k == YAML_YAML_NAME) {
        expect_identical(mod1a[[k]], basename(.test_path))
        expect_identical(mod1b[[k]], basename(.test_yaml))
      } else {
        expect_equal(mod1a[[k]], mod1b[[k]])
      }
    }

    # clean up tmp file
    fs::file_delete(.test_path)
  })

  test_that("new_model() .overwrite arg works", {
    # create new model with args
    .test_yaml <- "model-examples/1.yaml"
    .new_path <- "model-examples/new.yml"
    on.exit({
      if (fs::file_exists(.new_path)) fs::file_delete(.new_path)
    })
    fs::file_copy(.test_yaml, .new_path)

    # error if file exists
    expect_error(new_model(
        .yaml_path = .new_path,
        .description = "fake model"),
      regexp = "that file already exists")

    suppressSpecificWarning({
      new_model(
        .yaml_path = .new_path,
        .description = "fake model",
        .overwrite = TRUE
      )
    }, "No model file found at.+\\.ctl")

    suppressSpecificWarning({
      new_mod <- read_model(.new_path)
    }, "No model file found at.+\\.ctl")

    expect_equal(new_mod$description, "fake model")

  })


  test_that("new_model() .based_on arg works", {
    # create new model with args
    .test_path <- "model-examples/tmp.yaml"

    suppressSpecificWarning({
      mod1a <- new_model(
        .yaml_path = .test_path,
        .description = "original acop model",
        .based_on = "1"
      )
    }, "No model file found at.+\\.ctl")

    expect_equal(mod1a[[YAML_BASED_ON]], "1")

    # clean up tmp file
    fs::file_delete(.test_path)
  })

  test_that("new_model() .based_on arg errors on fake model", {
    # create new model with args
    .test_path <- "model-examples/tmp.yaml"

    expect_error(
      suppressSpecificWarning({
        mod1a <- new_model(
          .yaml_path = .test_path,
          .description = "original acop model",
          .based_on = c("1", "fake")
        )
      }, "No model file found at.+\\.ctl")
      , regexp = "cannot find .yaml or .yml files"
    )
  })

  test_that("as_model() returns the correct type from a model object", {
    # read model from disk
    mod1 <- read_model(YAML_TEST_FILE)

    # pass through as_model() and expect the same thing
    mod2 <- mod1 %>% as_model()
    expect_equal(mod1, mod2)
  })


  test_that("as_model() returns the correct type from a process object", {
    # build fake process object
    .ctl_file <- basename(ctl_ext(YAML_TEST_FILE))
    proc1 <- bbi_dry_run(.cmd_args = c("run", "nonmem", "sge", .ctl_file), .dir = MODEL_DIR)

    # convert to model
    mod1 <- proc1 %>% as_model()

    # check class and model path
    expect_identical(mod1[[YAML_MOD_PATH]], .ctl_file)
    expect_identical(class(mod1), MOD_CLASS_LIST)
  })

  test_that("as_model() errors with non-existent model", {
    proc1 <- bbi_dry_run(c("naw", "dawg"), "yea")
    expect_error(as_model(proc1), regexp = FIND_YAML_ERR_MSG)
  })

}) # closing withr::with_options


withr::with_options(list(rbabylon.model_directory = normalizePath(MODEL_DIR)), {
  test_that("compare read_model() and new_model() objects with numeric input", {
  # create new model with args
    .test_yaml <- 1
    .test_path <- 2
    .cleanup_path <- "model-examples/2.yaml"
    on.exit({ fs::file_delete(.cleanup_path) })

    expect_warning(mod1a <- new_model(
      .yaml_path = .test_path,
      .description = "original acop model",
      .tags = c("acop tag", "other tag"),
      .bbi_args = list(overwrite = TRUE, threads = 4)
    ), regexp = "Did not pass a YAML extension")

    # read model from YAML
    mod1b <- read_model(.path = .test_yaml)

    # check class and keys are right
    expect_identical(class(mod1a), MOD_CLASS_LIST)
    expect_identical(class(mod1b), MOD_CLASS_LIST)

    expect_true(all(MODEL_REQ_KEYS %in% names(mod1a)))
    expect_true(all(MODEL_REQ_KEYS %in% names(mod1b)))

    # also check that some of the required keys have the same value
    for (k in MODEL_REQ_KEYS) {
      if (k == YAML_MOD_PATH) {
        expect_identical(mod1a[[k]], basename(ctl_ext(.test_path)))
        expect_identical(mod1b[[k]], basename(ctl_ext(.test_yaml)))
      } else if (k == YAML_OUT_DIR) {
        expect_identical(mod1a[[k]], basename(tools::file_path_sans_ext(.test_path)))
        expect_identical(mod1b[[k]], basename(tools::file_path_sans_ext(.test_yaml)))
      } else if (k == YAML_YAML_NAME) {
        expect_identical(mod1a[[k]], basename(yaml_ext(.test_path)))
        expect_identical(mod1b[[k]], basename(yaml_ext(.test_yaml)))
      } else {
        expect_equal(mod1a[[k]], mod1b[[k]])
      }
    }
  })

}) # closing withr::with_options
