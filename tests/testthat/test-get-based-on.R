context("Extract model paths from based_on fields")

source("data/test-workflow-ref.R")

withr::with_options(list(rbabylon.model_directory = NULL), {

  test_that("get_based_on works happy path model object" , {
    # create copy
    mod2 <- copy_model_from(YAML_TEST_FILE, NEW_MOD2,   "level 1 copy of 1")

    expect_identical(get_based_on(mod2), MOD1_ABS_PATH)

    cleanup()
  })

  test_that("get_based_on works happy path character" , {
    # create copy
    mod2 <- copy_model_from(YAML_TEST_FILE, NEW_MOD2,   "level 1 copy of 1")

    expect_identical(get_based_on(NEW_MOD2), MOD1_ABS_PATH)

    cleanup()
  })

  test_that("get_based_on works happy path run_log tibble" , {
    create_all_models()
    mod3 <- mod3 %>% add_based_on("2")

    # build log_df and check get_based_on
    log_df <- run_log(MODEL_DIR)

    expect_identical(
      get_based_on(log_df),
      list(
        NULL,
        MOD1_ABS_PATH,
        c(MOD1_ABS_PATH, MOD2_ABS_PATH),
        MOD2_ABS_PATH
      )
    )

    cleanup()
  })

  test_that("get_based_on constructs ancestry manually" , {
    create_all_models()

    mod4_bo <- get_based_on(mod4)

    # check that the first path is right
    expect_identical(as.character(mod4_bo), MOD2_ABS_PATH)

    # load the model and check it matches what it was copied from
    mod2_reload <- read_model(mod4_bo)
    for (.k in names(mod2_reload)) {
      expect_equal(mod2[[.k]], mod2_reload[[.k]])
    }

    # check the based_on of that model
    mod2_bo <- get_based_on(mod2_reload)
    expect_identical(mod2_bo, MOD1_ABS_PATH)

    # load that model and check it
    mod1_reload <- read_model(mod2_bo)
    for (.k in names(mod1_reload)) {
      expect_equal(mod1[[.k]], mod1_reload[[.k]])
    }

    cleanup()
  })

  test_that("get_based_on .check_exists=TRUE errors if model is gone" , {
    create_all_models()
    mod3 <- mod3 %>% add_based_on("2")

    # check based_on is right
    expect_identical(get_based_on(mod3), c(MOD1_ABS_PATH, MOD2_ABS_PATH))

    # delete mod2 yaml check based_on still works (.check_exists=FALSE is default)
    fs::file_delete(yaml_ext(MOD2_ABS_PATH))
    expect_identical(get_based_on(mod3), c(MOD1_ABS_PATH, MOD2_ABS_PATH))

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


  test_that("get_model_ancestry works happy path model object" , {
    create_all_models()
    mod3 <- mod3 %>% add_based_on("2")

    # check against character vector ref
    expect_identical(get_based_on(mod4), MOD2_ABS_PATH)
    expect_identical(
      get_model_ancestry(mod4),
      c(MOD1_ABS_PATH, MOD2_ABS_PATH)
    )

    # build log_df and check
    log_df <- run_log(MODEL_DIR)
    expect_equal(1, log_df %>% filter(absolute_model_path %in% get_based_on(mod4)) %>% nrow())
    expect_equal(2, log_df %>% filter(absolute_model_path %in% get_model_ancestry(mod4)) %>% nrow())

    cleanup()
  })

  test_that("get_model_ancestry works happy path character" , {
    create_all_models()
    mod3 <- mod3 %>% add_based_on("2")

    # check against character vector ref
    expect_identical(get_based_on(LEVEL2_MOD), MOD2_ABS_PATH)
    expect_identical(
      get_model_ancestry(LEVEL2_MOD),
      c(MOD1_ABS_PATH, MOD2_ABS_PATH)
    )

    cleanup()
  })

  test_that("get_model_ancestry works happy path run_log tibble" , {
    create_all_models()
    mod3 <- mod3 %>% add_based_on("2")

    # build log_df and check get_based_on
    log_df <- run_log(MODEL_DIR)

    expect_identical(
      get_model_ancestry(log_df),
      list(
        NULL,
        MOD1_ABS_PATH,
        c(MOD1_ABS_PATH, MOD2_ABS_PATH),
        c(MOD1_ABS_PATH, MOD2_ABS_PATH)
      )
    )

    cleanup()
  })

  test_that("get_model_ancestry errors if model is gone" , {
    create_all_models()

    # check based_on is right
    expect_identical(get_model_ancestry(mod4), c(MOD1_ABS_PATH, MOD2_ABS_PATH))

    # delete mod2 yaml and check for error
    fs::file_delete(yaml_ext(MOD2_ABS_PATH))
    expect_error(get_model_ancestry(mod4), regexp = "get_model_ancestry.+could not find a YAML")

    cleanup()
  })

}) # closing withr::with_options
