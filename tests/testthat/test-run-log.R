context("Constructing run log from model yaml")


test_that("run_log() errors with malformed YAML [BBR-RNLG-001]", {
  clean_test_enviroment(create_rlg_models)
  temp_dir <- file.path(tempdir(), "run_log_malformed_yaml_test")
  fs::dir_create(temp_dir)
  temp_yaml <- fs::file_copy(file.path(REF_DIR, "test-yaml", "zz_fail_no_modtype.yaml"), temp_dir)
  on.exit(fs::dir_delete(temp_dir))

  expect_warning(log_df <- run_log(temp_dir), "do not contain required keys")
  expect_true(nrow(log_df) == 0L)

  fs::file_copy(YAML_TEST_FILE, temp_dir)
  expect_error(
    run_log(temp_dir),
    regexp = "Unexpected error trying to read model"
  )
})


test_that("run_log returns NULL and warns when no YAML found [BBR-RNLG-002]", {
  clean_test_enviroment(create_rlg_models)
  log_df <- expect_warning(run_log(file.path(REF_DIR, "read-output-refs")), regexp = "Found no valid model YAML files in")
  expect_true(inherits(log_df, "tbl"))
  expect_equal(nrow(log_df), 0)
  expect_equal(ncol(log_df), 0)
})


test_that("run_log matches reference [BBR-RNLG-003]", {
  clean_test_enviroment(create_rlg_models)
  log_df <- run_log(MODEL_DIR)
  expect_equal(nrow(log_df), RUN_LOG_ROWS)
  expect_equal(ncol(log_df), RUN_LOG_COLS)
  expect_identical(log_df[[RUN_ID_COL]], c("1", "2", "3"))
  expect_identical(log_df[[RUN_ID_COL]], basename(log_df[[ABS_MOD_PATH]]))
  expect_identical(log_df$tags, list(ORIG_TAGS, NEW_TAGS, ORIG_TAGS))
  expect_identical(log_df$yaml_md5, RUN_LOG_YAML_MD5)
  expect_identical(log_df$based_on, list(NULL, "1", c("1", "2")))
  expect_identical(log_df$star, rep(FALSE, nrow(log_df)))

  # check log_df class
  expect_true(inherits(log_df, RUN_LOG_CLASS))
  expect_true(inherits(log_df, LOG_DF_CLASS))

  # check class of each column
  log_classes <- log_df %>% dplyr::summarise_all(class) %>% as.list()

  run_log_classes_ref <- tibble::tibble(
    !!ABS_MOD_PATH      := "character",
    !!YAML_YAML_MD5     := "character",
    !!YAML_MOD_TYPE     := "character",
    !!YAML_DESCRIPTION  := "character",
    !!YAML_BBI_ARGS     := "list",
    !!YAML_BASED_ON     := "list",
    !!YAML_TAGS         := "list",
    !!YAML_STAR         := "logical"
  ) %>% as.list()

  for (.n in names(run_log_classes_ref)) {
    expect_identical(log_classes[[.n]], run_log_classes_ref[[.n]])
  }
})

test_that("run_log works with star attribute [BBR-RNLG-005]", {
  clean_test_enviroment(create_rlg_models)
  MOD1 <- add_star(MOD1)
  withr::defer({
    MOD1 <- remove_star(MOD1)
  })
  log_df <- run_log(MODEL_DIR)
  expect_identical(log_df$star, c(TRUE, FALSE, FALSE))

  star_df <- filter(log_df, star)
  expect_identical(nrow(star_df), 1L)
})

##########################################
# testing hierarchical nested directories
##########################################

# copy model 1 to level deeper
# TODO: consider unifying this (and the same thing on line 63 of test-config-log.R) with the other create_ functions in setup-workflow-ref.R


test_that("run_log() works correctly with nested dirs [BBR-RNLG-004]", {
  clean_test_enviroment(create_rlg_models)
  fs::dir_create(LEVEL2_DIR)
  copy_model_from(MOD1, file.path(LEVEL2_SUBDIR, MOD_ID), "level 2 copy of 1.yaml", .inherit_tags = TRUE)
  fs::dir_copy(MOD1_PATH, LEVEL2_MOD)

  # test default
  log_df <- run_log(MODEL_DIR)
  expect_equal(nrow(log_df), RUN_LOG_ROWS)

  # test with .recurse = TRUE
  log_df <- run_log(MODEL_DIR, .recurse = TRUE)
  expect_equal(nrow(log_df), RUN_LOG_ROWS+1)
  expect_equal(ncol(log_df), RUN_LOG_COLS)
  expect_false(any(duplicated(log_df[[ABS_MOD_PATH]])))
  expect_identical(basename(log_df[[ABS_MOD_PATH]]), c("1", "2", "3", "1"))
  expect_identical(log_df$tags, list(ORIG_TAGS, NEW_TAGS, ORIG_TAGS, ORIG_TAGS))
  expect_identical(log_df$yaml_md5, c(RUN_LOG_YAML_MD5, MOD_LEVEL2_MD5))
  expect_identical(log_df$based_on, list(NULL, "1", c("1", "2"), "../1"))
})

# ##########################################
# # Testing Additional Parameters Passed
# ##########################################


test_that("run_log() can be filtered via model run: numeric name [BBR-RNLG-005]", {
  clean_test_enviroment(create_rlg_models)
  log_df <- run_log(MODEL_DIR, .include = 1:2)
  expect_equal(nrow(log_df), 2)
  expect_equal(unique(log_df$run), c("1", "2"))

  log_df <- run_log(MODEL_DIR, .include = 1)
  expect_equal(nrow(log_df), 1)
  expect_equal(unique(log_df$run), c("1"))

  # Reverse order
  log_df <- run_log(MODEL_DIR, .include = 2:1)
  expect_equal(nrow(log_df), 2)
  expect_equal(unique(log_df$run), c("1", "2"))
})

test_that("run_log() can be filtered via model run: character name [BBR-RNLG-005]", {
  setup_this_test <- function() {
    create_rlg_models()
    copy_model_from(MOD1, "Child")
    copy_model_from(MOD1, "Parent")
  }

  clean_test_enviroment(setup_this_test)
  log_df <- run_log(MODEL_DIR, .include = c(1:2, "Child"))
  expect_equal(nrow(log_df), 3)
  expect_equal(unique(log_df$run), c("1", "2", "Child"))

  # Reverse order of above test
  log_df <- run_log(MODEL_DIR, .include = c(2:1, "Child"))
  expect_equal(nrow(log_df), 3)
  expect_equal(unique(log_df$run), c("1", "2", "Child"))

  log_df <- run_log(MODEL_DIR, .include = c("Child", 1, 2, 3))
  expect_equal(nrow(log_df), 4)
  expect_equal(unique(log_df$run), c("1", "2", "3", "Child"))

  log_df <- run_log(MODEL_DIR, .include = c(1:2, "Parent"))
  expect_equal(nrow(log_df), 3)
  expect_equal(unique(log_df$run), c("1", "2", "Parent"))
})

test_that("run_log() can be filtered via tags: tags only [BBR-RNLG-006]", {
  clean_test_enviroment(create_rlg_models)
  log_df <- run_log(MODEL_DIR, .include = "new tag 1")
  expect_equal(nrow(log_df), 1)
  expect_equal(unique(log_df$run), "2")

  log_df <- run_log(MODEL_DIR, .include = "acop tag")
  expect_equal(nrow(log_df), 2)
  expect_equal(unique(log_df$run), c("1", "3"))

  log_df <- run_log(MODEL_DIR, .include = c("acop tag", "new tag 1"))
  expect_equal(nrow(log_df), 3)
  expect_equal(unique(log_df$run), c("1", "2", "3"))
})

test_that("run_log() can be filtered via tags: tags and model run [BBR-RNLG-006]", {
  clean_test_enviroment(create_rlg_models)
  log_df <- run_log(MODEL_DIR, .include = c(1, "new tag 1"))
  expect_equal(nrow(log_df), 2)
  expect_equal(unique(log_df$run), c("1", "2"))

  log_df <- run_log(MODEL_DIR, .include = c("acop tag", 2))
  expect_equal(nrow(log_df), 3)
  expect_equal(unique(log_df$run), c("1", "2", "3"))
})
