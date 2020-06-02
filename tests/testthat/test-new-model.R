context("Testing function to create or read in model object")

source("data/test-workflow-ref.R")

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
    mod1b <- read_model(.path = .test_yaml)

    # check class and keys are right
    expect_identical(class(mod1a), MODEL_CLASS_LIST)
    expect_identical(class(mod1b), MODEL_CLASS_LIST)

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
    expect_identical(class(mod1a), MODEL_CLASS_LIST)
    expect_identical(class(mod1b), MODEL_CLASS_LIST)

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

  test_that("save_model_yaml() saves to correct default path", {
    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)
    on.exit({ fs::file_delete(new_yaml) })

    # make a spec from it
    suppressSpecificWarning({
      new_mod <- read_model(new_yaml)
    }, .regexpr = "No model file found at.+\\.ctl")

    # delete the underlying yaml
    fs::file_delete(new_yaml)
    expect_false(fs::file_exists(new_yaml))

    # re-save yaml
    save_model_yaml(new_mod)

    # look for it
    expect_true(fs::file_exists(new_yaml))
  })

  test_that("save_model_yaml() saves to user supplied path", {
    # give fake path
    fake_path <- "model-examples/fake.yaml"
    expect_false(fs::file_exists(fake_path))

    # make a spec
    new_mod <- read_model(YAML_TEST_FILE)

    # re-save yaml
    save_model_yaml(new_mod, fake_path)
    on.exit({ fs::file_delete(fake_path) })

    # look for it
    expect_true(fs::file_exists(fake_path))
  })

  test_that("save_model_yaml() deletes the right keys", {
    # give fake path
    fake_path <- "model-examples/fake.yaml"
    expect_false(fs::file_exists(fake_path))

    # make a spec
    new_mod <- read_model(YAML_TEST_FILE)

    # re-save yaml
    save_model_yaml(new_mod, fake_path)
    on.exit({ fs::file_delete(fake_path) })

    # read it back in and check the keys
    loaded_yaml <- yaml::read_yaml(fake_path)
    expect_false(any(names(loaded_yaml) %in% YAML_ERASE_OUT_KEYS))
  })


  test_that("save_model_yaml() doesn't save an empty list", {
    # give fake path
    fake_path <- "model-examples/fake.yaml"
    expect_false(fs::file_exists(fake_path))

    # make a spec
    new_mod <- read_model(YAML_TEST_FILE)

    # erase bbi_args with empty list
    new_mod[[YAML_BBI_ARGS]] <- list()

    # re-save yaml
    save_model_yaml(new_mod, fake_path)
    on.exit({ fs::file_delete(fake_path) })

    # read it back in and check that bbi_args are gone
    loaded_yaml <- readr::read_lines(fake_path)
    expect_false(any(stringr::str_detect(YAML_BBI_ARGS, loaded_yaml)))
  })


  test_that("save_model_yaml() saves tags as an array", {
    # give fake path
    fake_path <- "model-examples/fake.yaml"
    expect_false(fs::file_exists(fake_path))

    # make a spec
    new_mod <- read_model(YAML_TEST_FILE)

    # reset yaml path so that it's reconciles to fake path
    new_mod[[YAML_YAML_NAME]] <- basename(fake_path)

    # erase tags and re-save
    new_mod[[YAML_TAGS]] <- NULL
    save_model_yaml(new_mod, fake_path)
    on.exit({ fs::file_delete(fake_path) })

    # read it back in and check that tags are gone, then reconcile
    loaded_yaml <- readr::read_lines(fake_path)
    expect_false(any(stringr::str_detect(YAML_TAGS, loaded_yaml)))
    new_mod <- reconcile_yaml(new_mod)

    # add a single tag
    FAKE_TAG1 <- "naw1"
    new_mod <- new_mod %>% add_tags(FAKE_TAG1)

    # read it back in and check for new tag
    loaded_yaml <- readr::read_lines(fake_path)
    expect_true(any(stringr::str_detect(glue("{YAML_TAGS}:"), loaded_yaml)))
    expect_true(any(stringr::str_detect(glue("- {FAKE_TAG1}"), loaded_yaml)))

    # add a another tag
    FAKE_TAG2 <- "naw2"
    new_mod <- new_mod %>% add_tags(FAKE_TAG2)

    # read it back in and check for new tags
    loaded_yaml <- readr::read_lines(fake_path)
    expect_true(any(stringr::str_detect(glue("{YAML_TAGS}:"), loaded_yaml)))
    expect_true(any(stringr::str_detect(glue("- {FAKE_TAG1}"), loaded_yaml)))
    expect_true(any(stringr::str_detect(glue("- {FAKE_TAG2}"), loaded_yaml)))
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
    expect_identical(class(mod1), MODEL_CLASS_LIST)
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
    expect_identical(class(mod1a), MODEL_CLASS_LIST)
    expect_identical(class(mod1b), MODEL_CLASS_LIST)

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
