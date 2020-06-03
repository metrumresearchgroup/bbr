context("Extract model paths from based_on fields")

source("data/test-workflow-ref.R")

withr::with_options(list(rbabylon.model_directory = NULL), {

  test_that("get_based_on works happy path model object" , {
    on.exit({ cleanup() })

    # create copy
    mod2 <- copy_model_from(YAML_TEST_FILE, NEW_MOD2,   "level 1 copy of 1")

    expect_identical(get_based_on(mod2), MOD1_ABS_PATH)
  })

  test_that("get_based_on works happy path character" , {
    on.exit({ cleanup() })

    # create copy
    mod2 <- copy_model_from(YAML_TEST_FILE, NEW_MOD2,   "level 1 copy of 1")

    expect_identical(get_based_on(NEW_MOD2), MOD1_ABS_PATH)
  })

  test_that("get_based_on.character() fails with vector", {
    expect_error(get_based_on(c("naw", "dawg")), regexp = "only scaler values are permitted")
  })

  test_that("get_based_on works happy path run_log tibble" , {
    on.exit({ cleanup() })
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
  })

  test_that("get_based_on constructs ancestry manually" , {
    on.exit({ cleanup() })
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
  })

  test_that("get_based_on .check_exists=TRUE errors if model is gone" , {
    on.exit({ cleanup() })
    create_all_models()

    mod3 <- mod3 %>% add_based_on("2")

    # check based_on is right
    expect_identical(get_based_on(mod3), c(MOD1_ABS_PATH, MOD2_ABS_PATH))

    # delete mod2 yaml check based_on still works (.check_exists=FALSE is default)
    fs::file_delete(yaml_ext(MOD2_ABS_PATH))
    expect_identical(get_based_on(mod3), c(MOD1_ABS_PATH, MOD2_ABS_PATH))

    # check that it fails with .check_exists=TRUE
    expect_error(get_based_on(mod3, .check_exists = TRUE), regexp = "but cannot find .yaml or .yml")
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
    on.exit({ cleanup() })
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
  })

  test_that("get_model_ancestry works happy path character" , {
    on.exit({ cleanup() })
    create_all_models()

    mod3 <- mod3 %>% add_based_on("2")

    # check against character vector ref
    expect_identical(get_based_on(LEVEL2_MOD), MOD2_ABS_PATH)
    expect_identical(
      get_model_ancestry(LEVEL2_MOD),
      c(MOD1_ABS_PATH, MOD2_ABS_PATH)
    )
  })

  test_that("get_model_ancestry.character() fails with vector", {
    expect_error(get_model_ancestry(c("naw", "dawg")), regexp = "only scaler values are permitted")
  })

  test_that("get_model_ancestry works happy path run_log tibble" , {
    on.exit({ cleanup() })
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
  })

  test_that("get_model_ancestry errors if model is gone" , {
    on.exit({ cleanup() })
    create_all_models()

    # check based_on is right
    expect_identical(get_model_ancestry(mod4), c(MOD1_ABS_PATH, MOD2_ABS_PATH))

    # delete mod2 yaml and check for error
    fs::file_delete(yaml_ext(MOD2_ABS_PATH))
    expect_error(get_model_ancestry(mod4), regexp = "get_model_ancestry.+could not find a YAML")
  })

  test_that("get_model_ancestry works on run_log tibble with more complicated ancestry" , {
    on.exit({ cleanup() })
    create_all_models()

    mod5 <- copy_model_from(mod3, file.path(MODEL_DIR, 5),   "level 1 copy of 3")
    mod6 <- copy_model_from(mod5, file.path(MODEL_DIR, 6),   "level 1 copy of 5")
    mod7 <- copy_model_from(mod6, file.path(MODEL_DIR, 7),   "level 1 copy of 6")
    mod8 <- copy_model_from(mod4, file.path(LEVEL2_DIR, 2),   "level 2 copy of 4")
    mod9 <- copy_model_from(mod3, file.path(LEVEL2_DIR, 3),   "level 2 copy of 3")
    mod10 <- copy_model_from(mod3, file.path(LEVEL2_DIR, 4),   "level 2 copy of 3")
    mod10 <- mod10 %>% add_based_on(get_model_path(mod4))

    # build log_df and check get_based_on
    log_df <- run_log(MODEL_DIR)

    expect_identical(
      get_model_ancestry(log_df),
      list(
        NULL, #1
        MOD1_ABS_PATH, #2
        MOD1_ABS_PATH, #3
        c(MOD1_ABS_PATH, MOD3_ABS_PATH), #5
        c(MOD1_ABS_PATH, MOD3_ABS_PATH, file.path(getwd(), MODEL_DIR, "5")), #6
        c(MOD1_ABS_PATH, MOD3_ABS_PATH, file.path(getwd(), MODEL_DIR, "5"), file.path(getwd(), MODEL_DIR, "6")), #7
        c(MOD1_ABS_PATH, MOD2_ABS_PATH), #level2/1
        c(MOD1_ABS_PATH, MOD2_ABS_PATH, MOD4_ABS_PATH), #level2/2
        c(MOD1_ABS_PATH, MOD3_ABS_PATH), #level2/3
        c(MOD1_ABS_PATH, MOD2_ABS_PATH, MOD3_ABS_PATH, MOD4_ABS_PATH)  #level2/4
      )
    )
  })

}) # closing withr::with_options
