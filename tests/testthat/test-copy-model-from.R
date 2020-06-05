context("Copying model objects")

source("data/test-workflow-ref.R")


#########################
# copy_model_from tests
#########################

withr::with_options(list(rbabylon.model_directory = NULL), {

  cleanup()

  test_that("copy_from_model creates accurate copy", {
    on.exit({ cleanup() })

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
    on.exit({ cleanup() })

    # run copy_model_from
    fs::file_copy(YAML_TEST_FILE, paste0(NEW_MOD2, '.yml'))
    copy_model_from(YAML_TEST_FILE,
                    NEW_MOD3,
                    NEW_DESC,
                    .based_on_additional = get_model_id(NEW_MOD2),
                    .inherit_tags = TRUE,
                    .update_model_file = FALSE)

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

  test_that("copy_from_model bbi_nonmem_model", {
    on.exit({ cleanup() })

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
  })


  test_that("copy_from_model .overwrite=TRUE works", {
    on.exit({ cleanup() })

    # set up model object
    mod1 <- read_model(YAML_TEST_FILE)
    new_yaml_path <- yaml_ext(NEW_MOD2)
    new_ctl_path <- ctl_ext(NEW_MOD2)
    expect_false(fs::file_exists(new_yaml_path))
    expect_false(fs::file_exists(new_ctl_path))

    # copy control stream
    fs::file_copy(ctl_ext(YAML_TEST_FILE), new_ctl_path)

    # copy with .overwrite=TRUE
    copy_model_from(mod1, NEW_MOD2, NEW_DESC, .overwrite=TRUE)

    # check the control stream is modified by overwrite
    new_mod_str <- readr::read_file(new_ctl_path)

    orig_desc_pattern <- paste0("\\$PROBLEM ", DESC_IN_CTL, "\n\n\\$INPUT")
    expect_false(grepl(orig_desc_pattern, new_mod_str))

    new_desc_pattern <- paste0("\\$PROBLEM ", get_model_id(NEW_MOD2), " ", NEW_DESC, "\n\n\\$INPUT")
    expect_true(grepl(new_desc_pattern, new_mod_str))
  })

  test_that("copy_from_model .overwrite=FALSE works", {
    on.exit({ cleanup() })

    # set up model object
    mod1 <- read_model(YAML_TEST_FILE)
    new_yaml_path <- yaml_ext(NEW_MOD2)
    new_ctl_path <- ctl_ext(NEW_MOD2)
    expect_false(fs::file_exists(new_yaml_path))
    expect_false(fs::file_exists(new_ctl_path))

    # copy control stream
    fs::file_copy(ctl_ext(YAML_TEST_FILE), new_ctl_path)

    # copy with .overwrite=FALSE
    expect_error(
      copy_model_from(mod1, NEW_MOD2, NEW_DESC, .overwrite=FALSE),
      regexp = "File already exists at"
    )

    # check the control stream is NOT modified (i.e. no overwrite)
    new_mod_str <- readr::read_file(new_ctl_path)

    orig_desc_pattern <- paste0("\\$PROBLEM ", DESC_IN_CTL, "\n\n\\$INPUT")
    expect_true(grepl(orig_desc_pattern, new_mod_str))

    new_desc_pattern <- paste0("\\$PROBLEM ", get_model_id(NEW_MOD2), " ", NEW_DESC, "\n\n\\$INPUT")
    expect_false(grepl(new_desc_pattern, new_mod_str))
  })
}) # closing withr::with_options


withr::with_options(list(rbabylon.model_directory = normalizePath(MODEL_DIR)), {
  test_that("copy_from_model numeric", {
    on.exit({ cleanup() })

    # get integer input and check for related paths
    mod1 <- stringr::str_replace_all(YAML_TEST_FILE, "[^\\d]", "") %>% as.numeric()
    mod2 <- stringr::str_replace_all(NEW_MOD2, "[^\\d]", "") %>% as.numeric()
    new_yaml_path <- yaml_ext(NEW_MOD2)
    new_ctl_path <- ctl_ext(NEW_MOD2)
    expect_false(fs::file_exists(new_yaml_path))
    expect_false(fs::file_exists(new_ctl_path))

    # run copy_model_from with the integer input
    copy_model_from(mod1, mod2, NEW_DESC, .add_tags = NEW_TAGS)

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
  })
}) # closing withr::with_options
