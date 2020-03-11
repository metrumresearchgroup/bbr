context("Workflow file manipulation")

# define constants
MODEL_DIR <- "model-examples"
YAML_TEST_FILE <- file.path(MODEL_DIR, "1.yaml")
NEW_MOD2 <- file.path(MODEL_DIR, "2")
NEW_MOD3 <- file.path(MODEL_DIR, "3")

ORIG_DESC <- "original acop model"
NEW_DESC <- "new description"

ORIG_TAGS <- c("acop tag", "other tag")
NEW_TAGS <- c("new tag 1", "new tag 2")

NEW_TEXT1 <- c("naw", "paw")
NEW_TEXT2 <- c("all", "done")

MODEL_CLASS_LIST <- c("bbi_nonmem_model", "list")

withr::with_options(list(rbabylon.model_directory = NULL), {

  #########################
  # copy_model_from tests
  #########################

  # delete tmp files if they are leftover from previous test
  for (m in c(NEW_MOD2, NEW_MOD3)) {
    if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
    if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
  }

  test_that("copy_from_model creates accurate copy", {
    # run copy_model_from
    new_mod <- copy_model_from(YAML_TEST_FILE, NEW_MOD2, NEW_DESC, .add_tags = NEW_TAGS)

    # check that everything is copied through in the object
    expect_identical(class(new_mod), MODEL_CLASS_LIST)
    expect_identical(new_mod[[YAML_MOD_PATH]], basename(ctl_ext(NEW_MOD2)))
    expect_identical(new_mod[[YAML_DESCRIPTION]], NEW_DESC)
    expect_identical(new_mod[[YAML_BASED_ON]], "1")
    expect_identical(new_mod[[YAML_TAGS]], NEW_TAGS)
    expect_equal(new_mod[[YAML_BBI_ARGS]], list(overwrite = TRUE, threads = 4L))
    expect_identical(new_mod[[YAML_OUT_DIR]], basename(NEW_MOD2))

    # check that everything is copied through in the YAML
    new_yaml <- yaml::read_yaml(yaml_ext(NEW_MOD2))

    expect_identical(new_yaml[[YAML_MOD_PATH]], basename(ctl_ext(NEW_MOD2)))
    expect_identical(new_yaml[[YAML_DESCRIPTION]], NEW_DESC)
    expect_identical(new_yaml[[YAML_BASED_ON]], "1")
    expect_identical(new_yaml[[YAML_TAGS]], NEW_TAGS)
    expect_equal(new_yaml[[YAML_BBI_ARGS]], list(overwrite = TRUE, threads = 4L))

    # check the control stream is modified
    new_mod_str <- ctl_ext(NEW_MOD2) %>% readr::read_file()
    new_desc_pattern <- paste0("\\$PROBLEM ", get_model_id(NEW_MOD2), " ", NEW_DESC, "\n\n\\$INPUT")
    expect_true(grepl(new_desc_pattern, new_mod_str))

  })


  test_that("copy_from_model options work", {
    # run copy_model_from
    copy_model_from(YAML_TEST_FILE,
                    NEW_MOD3,
                    NEW_DESC,
                    .based_on_additional = get_model_id(NEW_MOD2),
                    .inherit_tags = TRUE,
                    .update_mod_file = FALSE)

    # check that everything is copied through
    new_yaml <- yaml::read_yaml(yaml_ext(NEW_MOD3))

    expect_identical(new_yaml[[YAML_MOD_PATH]], basename(ctl_ext(NEW_MOD3)))
    expect_identical(new_yaml[[YAML_DESCRIPTION]], NEW_DESC)
    expect_identical(new_yaml[[YAML_BASED_ON]], c("1", get_model_id(NEW_MOD2)))
    expect_identical(new_yaml[[YAML_TAGS]], ORIG_TAGS)
    expect_equal(new_yaml[[YAML_BBI_ARGS]], list(overwrite = TRUE, threads = 4L))

    # check the control stream is not modified
    prob_pattern <- "\\$PROB(.|\n)*?\\$"
    orig_mod_str <- ctl_ext(YAML_TEST_FILE) %>% readr::read_file()
    new_mod_str <- ctl_ext(NEW_MOD3) %>% readr::read_file()
    expect_identical(
      stringr::str_extract(orig_mod_str, prob_pattern),
      stringr::str_extract(new_mod_str, prob_pattern)
    )
  })

  test_that("run_log matches reference tibble", {
    df <- suppressWarnings(run_log("model-examples"))
    ref_df <- readRDS("data/run_log_basic_200224.rds")
    expect_identical(df, ref_df)
  })

  # cleanup temp files
  for (m in c(NEW_MOD2, NEW_MOD3)) {
    if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
    if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
  }

  test_that("copy_from_model bbi_nonmem_model", {
    # run copy_model_from on a model object
    mod1 <- read_model(YAML_TEST_FILE)
    new_yaml_path <- yaml_ext(NEW_MOD2)
    new_ctl_path <- ctl_ext(NEW_MOD2)
    expect_false(fs::file_exists(new_yaml_path))
    expect_false(fs::file_exists(new_ctl_path))
    copy_model_from(mod1, NEW_MOD2, NEW_DESC, .add_tags = NEW_TAGS)

    # check that everything is copied through
    new_yaml <- yaml::read_yaml(new_yaml_path)

    expect_identical(new_yaml[[YAML_MOD_PATH]], basename(new_ctl_path))
    expect_identical(new_yaml[[YAML_DESCRIPTION]], NEW_DESC)
    expect_identical(new_yaml[[YAML_BASED_ON]], "1")
    expect_identical(new_yaml[[YAML_TAGS]], NEW_TAGS)
    expect_equal(new_yaml[[YAML_BBI_ARGS]], list(overwrite = TRUE, threads = 4L))

    # check the control stream is modified
    new_mod_str <- readr::read_file(new_ctl_path)
    new_desc_pattern <- paste0("\\$PROBLEM ", get_model_id(NEW_MOD2), " ", NEW_DESC, "\n\n\\$INPUT")
    expect_true(grepl(new_desc_pattern, new_mod_str))

    # cleanup
    fs::file_delete(new_yaml_path)
    fs::file_delete(new_ctl_path)
  })

  test_that("new_model() fails with invalid yaml path", {
    expect_error(new_model(.yaml_path = "naw", .description = "naw dawg"), regexp = "Must pass a file with a valid YAML extension")
  })

  test_that("compare read_model() and new_model() objects", {
    # create new model with args
    .test_path <- "model-examples/tmp.yaml"
    mod1a <- new_model(
      .yaml_path = .test_path,
      .description = "original acop model",
      .tags = c("acop tag", "other tag"),
      .bbi_args = list(overwrite = TRUE, threads = 4)
    )

    # read model from YAML
    mod1b <- read_model(.path = "model-examples/1.yaml")

    # check class and keys are right
    expect_identical(class(mod1a), MODEL_CLASS_LIST)
    expect_identical(class(mod1b), MODEL_CLASS_LIST)

    expect_true(all(MODEL_REQ_KEYS %in% names(mod1a)))
    expect_true(all(MODEL_REQ_KEYS %in% names(mod1b)))

    # also check that some of the required keys have the same value
    for (k in MODEL_REQ_KEYS) {
      if (k == YAML_MOD_PATH) {
        expect_identical(mod1a[[k]], basename(ctl_ext(.test_path)))
        expect_identical(mod1b[[k]], "1.ctl")
      } else if (k == YAML_OUT_DIR) {
        expect_identical(mod1a[[k]], basename(tools::file_path_sans_ext(.test_path)))
        expect_identical(mod1b[[k]], "1")
      } else {
        expect_equal(mod1a[[k]], mod1b[[k]])
      }
    }

    # clean up tmp file
    fs::file_delete(.test_path)
  })


  #########################
  # interacting with YAML
  #########################

  test_that("read_model() returns expected object", {
    ref_list <- list(
      description = ORIG_DESC,
      model_type = "nonmem",
      model_path = "1.ctl",
      tags = ORIG_TAGS,
      bbi_args = list(
        overwrite = TRUE,
        threads = 4L),
      orig_working_dir = file.path(getwd(), "model-examples"),
      orig_yaml_file ="1.yaml",
      output_dir = "1"
    )
    class(ref_list) <- MODEL_CLASS_LIST
    expect_equal(read_model("model-examples/1.yaml"), ref_list)
  })


  test_that("yaml with no model type will fail", {
    expect_error(read_model("test-yaml/zz_fail_no_modtype.yaml"), regexp = "Model yaml must have keys")
  })

  test_that("yaml with bad model path will fail", {
    expect_error(read_model("test-yaml/zz_fail_bad_modpath.yaml"), regexp = "must have either a .ctl or .mod extension")
  })

  test_that("yaml with no model path will return ctl", {
    .test_path <- "test-yaml/zz_pass_no_modpath"
    .spec <- read_model(yaml_ext(.test_path))
    expect_identical(.spec[[YAML_MOD_PATH]], basename(ctl_ext(.test_path)))
  })

  test_that("save_model_yaml() saves to correct default path", {
    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    new_mod <- read_model(new_yaml)

    # delete the underlying yaml
    fs::file_delete(new_yaml)
    expect_false(fs::file_exists(new_yaml))

    # re-save yaml
    save_model_yaml(new_mod)

    # look for it
    expect_true(fs::file_exists(new_yaml))

    # cleanup
    fs::file_delete(new_yaml)
  })

  test_that("save_model_yaml() saves to user supplied path", {
    # give fake path
    fake_path <- "model-examples/fake.yaml"
    expect_false(fs::file_exists(fake_path))

    # make a spec
    new_mod <- read_model(YAML_TEST_FILE)

    # re-save yaml
    save_model_yaml(new_mod, fake_path)

    # look for it
    expect_true(fs::file_exists(fake_path))

    # cleanup
    fs::file_delete(fake_path)
  })

  test_that("save_model_yaml() deletes the right keys", {
    # give fake path
    fake_path <- "model-examples/fake.yaml"
    expect_false(fs::file_exists(fake_path))

    # make a spec
    new_mod <- read_model(YAML_TEST_FILE)

    # re-save yaml
    save_model_yaml(new_mod, fake_path)

    # read it back in and check the keys
    loaded_yaml <- yaml::read_yaml(fake_path)
    expect_false(any(names(loaded_yaml) %in% YAML_ERASE_OUT_KEYS))

    # cleanup
    fs::file_delete(fake_path)
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

    # read it back in and check that bbi_args are gone
    loaded_yaml <- readr::read_lines(fake_path)
    expect_false(any(stringr::str_detect(YAML_BBI_ARGS, loaded_yaml)))

    # cleanup
    fs::file_delete(fake_path)
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

    # read it back in and check that tags are gone
    loaded_yaml <- readr::read_lines(fake_path)
    expect_false(any(stringr::str_detect(YAML_TAGS, loaded_yaml)))

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

    # cleanup
    fs::file_delete(fake_path)
  })


  test_that("reconcile_mod_yaml() pulls in new tags", {
    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    new_mod <- read_model(new_yaml)
    expect_identical(new_mod[[YAML_TAGS]], ORIG_TAGS)

    # add the tags to the yaml manually
    rogue_spec <- yaml::read_yaml(new_yaml)
    rogue_spec[[YAML_TAGS]] <- c(rogue_spec[[YAML_TAGS]], NEW_TAGS)
    yaml::write_yaml(rogue_spec, new_yaml)

    # check the reconcile add the new tags
    new_mod <- reconcile_mod_yaml(new_mod, new_yaml)
    expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))

    # cleanup
    fs::file_delete(new_yaml)
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
    expect_error(as_model(proc1), regexp = "Inferred YAML")
  })

  ######################################
  # modify_model_field and its wrappers
  ######################################

  test_that("modify_model_field() works correctly", {
    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    new_mod <- read_model(new_yaml)
    expect_identical(new_mod[[YAML_TAGS]], ORIG_TAGS)

    # modify the tags field
    new_mod <- modify_model_field(new_mod, .field=YAML_TAGS, .value=NEW_TAGS)
    expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))

    # check that the yaml was also modified
    rogue_spec <- yaml::read_yaml(new_yaml)
    expect_identical(rogue_spec[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))

    # cleanup
    fs::file_delete(new_yaml)
  })

  test_that("modify_model_field() de-duplication works", {
    dupe_tags <- c("ha", "hey", "ha")
    uniq_tags <- c("ha", "hey")

    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    new_mod <- read_model(new_yaml)
    expect_identical(new_mod[[YAML_TAGS]], ORIG_TAGS)

    # check that .unique = FALSE turns off de-duping
    new_mod <- modify_model_field(new_mod, .field=YAML_TAGS, .value=dupe_tags, .unique = FALSE)
    expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, dupe_tags))

    # check that .unique = TRUE (default) correctly de-dupes
    new_mod <- modify_model_field(new_mod, .field=YAML_TAGS, .value=dupe_tags)
    expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, uniq_tags))

    # cleanup
    fs::file_delete(new_yaml)
  })

  test_that("add_tags() and replace_tags() work correctly", {
    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    new_mod <- read_model(new_yaml)
    expect_identical(new_mod[[YAML_TAGS]], ORIG_TAGS)

    # test adding
    new_mod <- add_tags(new_mod, NEW_TAGS)
    expect_identical(new_mod[[YAML_TAGS]], c(ORIG_TAGS, NEW_TAGS))

    # test_replacing
    new_mod <- replace_tags(new_mod, NEW_TAGS)
    expect_identical(new_mod[[YAML_TAGS]], NEW_TAGS)

    # cleanup
    fs::file_delete(new_yaml)
  })

  test_that("add_decisions() and replace_decisions() work correctly", {
    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    new_mod <- read_model(new_yaml)
    expect_null(new_mod[[YAML_DECISIONS]])

    # test adding
    new_mod <- add_decisions(new_mod, NEW_TEXT1)
    expect_identical(new_mod[[YAML_DECISIONS]], NEW_TEXT1)
    new_mod <- add_decisions(new_mod, NEW_TEXT2)
    expect_identical(new_mod[[YAML_DECISIONS]], c(NEW_TEXT1, NEW_TEXT2))

    # test_replacing
    new_mod <- replace_decisions(new_mod, NEW_TEXT2)
    expect_identical(new_mod[[YAML_DECISIONS]], NEW_TEXT2)

    # cleanup
    fs::file_delete(new_yaml)
  })

  test_that("replace_description() works correctly", {
    # make a new yaml
    new_yaml <- yaml_ext(NEW_MOD2)
    fs::file_copy(YAML_TEST_FILE, new_yaml)

    # make a spec from it
    new_mod <- read_model(new_yaml)
    expect_identical(new_mod[[YAML_DESCRIPTION]], ORIG_DESC)

    # test_replacing
    new_mod <- replace_description(new_mod, NEW_DESC)
    expect_identical(new_mod[[YAML_DESCRIPTION]], NEW_DESC)

    # cleanup
    fs::file_delete(new_yaml)
  })

}) # closing withr::with_options
