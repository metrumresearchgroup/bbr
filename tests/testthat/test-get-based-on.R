context("Extract model paths from based_on fields")

source("data/test-workflow-ref.R")

withr::with_options(list(rbabylon.model_directory = NULL), {

  test_that("get_based_on works happy path model object" , {
    # create copy
    mod2 <- copy_model_from(YAML_TEST_FILE, NEW_MOD2,   "level 1 copy of 1")

    expect_identical(get_based_on(mod2), file.path(getwd(), tools::file_path_sans_ext(YAML_TEST_FILE)))

    cleanup()
  })

  test_that("get_based_on works happy path character" , {
    # create copy
    mod2 <- copy_model_from(YAML_TEST_FILE, NEW_MOD2,   "level 1 copy of 1")

    expect_identical(get_based_on(NEW_MOD2), file.path(getwd(), tools::file_path_sans_ext(YAML_TEST_FILE)))

    cleanup()
  })

  test_that("get_based_on works happy path run_log tibble" , {
    # create copies
    mod1 <- read_model(YAML_TEST_FILE)
    # create first two copies
    mod2 <- copy_model_from(YAML_TEST_FILE, NEW_MOD2,   "level 1 copy of 1")
    mod3 <- copy_model_from(YAML_TEST_FILE, NEW_MOD3,   "level 1 copy of 1") %>% add_based_on("2")

    # copy model 1 to level deeper
    fs::dir_create(LEVEL2_DIR)
    mod4 <- copy_model_from(mod2, LEVEL2_MOD, "level 2 copy of 2")

    # build log_df and check get_based_on
    log_df <- run_log(MODEL_DIR)

    expect_identical(
      get_based_on(log_df),
      list(
        NULL,
        file.path(getwd(), tools::file_path_sans_ext(YAML_TEST_FILE)),
        c(file.path(getwd(), tools::file_path_sans_ext(YAML_TEST_FILE)), file.path(getwd(), NEW_MOD2)),
        file.path(getwd(), NEW_MOD2)
      )
    )

    cleanup()
  })

  test_that("get_based_on constructs ancestry manually" , {
    # create first two copies
    mod1 <- read_model(YAML_TEST_FILE)
    mod2 <- copy_model_from(mod1, NEW_MOD2,   "level 1 copy of 1")
    mod3 <- copy_model_from(mod1, NEW_MOD3,   "level 1 copy of 1")

    # copy model 1 to level deeper
    fs::dir_create(LEVEL2_DIR)
    mod3 <- copy_model_from(mod2, LEVEL2_MOD, "level 2 copy of 2")

    mod3_bo <- get_based_on(mod3)

    # check that the first path is right
    expect_identical(as.character(mod3_bo), file.path(getwd(), NEW_MOD2))

    # load the model and check it matches what it was copied from
    mod2_reload <- read_model(mod3_bo)
    for (.k in names(mod2_reload)) {
      expect_equal(mod2[[.k]], mod2_reload[[.k]])
    }

    # check the based_on of that model
    mod2_bo <- get_based_on(mod2_reload)
    expect_identical(mod2_bo, file.path(getwd(), tools::file_path_sans_ext(YAML_TEST_FILE)))

    # load that model and check it
    mod1_reload <- read_model(mod2_bo)
    for (.k in names(mod1_reload)) {
      expect_equal(mod1[[.k]], mod1_reload[[.k]])
    }

    cleanup()
  })

  test_that("get_based_on .check_exists=TRUE errors if model is gone." , {
    # create two copies
    mod1 <- read_model(YAML_TEST_FILE)
    mod2 <- copy_model_from(mod1, NEW_MOD2,   "copy of 1")
    mod3 <- copy_model_from(mod2, NEW_MOD3,   "copy of 2")

    # check based_on is right
    expect_identical(get_based_on(mod3), file.path(getwd(), NEW_MOD2))

    # delete mod2 yaml check based_on still works (.check_exists=FALSE is default)
    fs::file_delete(yaml_ext(file.path(getwd(), NEW_MOD2)))
    expect_identical(get_based_on(mod3), file.path(getwd(), NEW_MOD2))

    # check that it fails with .check_exists=TRUE
    expect_error(get_based_on(mod3, .check_exists = TRUE), regexp = "but cannot find .yaml or .yml")

    cleanup()
  })

  test_that("get_based_on() behaves correctly on missing keys", {
    # missing YAML_BASED_ON
    .test_list <- list()
    .test_list[[WORKING_DIR]] <- "/fake/path"
    expect_null(get_based_on(.test_list))

    # missing WORKING_DIR
    .test_list <- list()
    expect_error(get_based_on(.test_list), regexp = "must contain key")

    .test_list[[YAML_BASED_ON]] <- "fake_model"
    expect_error(get_based_on(.test_list), regexp = "must contain key")
  })


  # test_that("model_ancestry UNFINSHED" , {
  #   # create first two copies
  #   mod2 <- copy_model_from(YAML_TEST_FILE, NEW_MOD2,   "level 1 copy of 1")
  #   mod3 <- copy_model_from(YAML_TEST_FILE, NEW_MOD3,   "level 1 copy of 1")
  #
  #   # copy model 1 to level deeper
  #   fs::dir_create(LEVEL2_DIR)
  #   mod <- copy_model_from(mod2, LEVEL2_MOD, "level 2 copy of 2")
  #
  #
  #   #get_based_on(mod)
  #
  #   cleanup()
  # })

}) # closing withr::with_options
