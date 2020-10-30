context("Constructing run log from model yaml")

setup({
  cleanup()
  create_rlg_models()
})

teardown({ cleanup() })

test_that("run_log() errors with malformed YAML", {
  temp_dir <- file.path(tempdir(), "run_log_malformed_yaml_test")
  fs::dir_create(temp_dir)
  temp_yaml <- fs::file_copy("test-yaml/zz_fail_no_modtype.yaml", temp_dir)
  on.exit(fs::dir_delete(temp_dir))

  expect_warning(log_df <- run_log(temp_dir), "do not contain required keys")
  expect_true(nrow(log_df) == 0L)

  fs::file_copy(YAML_TEST_FILE, temp_dir)
  expect_error(
    run_log(temp_dir),
    regexp = "Unexpected error trying to read model"
  )
})

test_that("run_log returns NULL and warns when no YAML found", {
  log_df <- expect_warning(run_log("data"), regexp = "Found no valid model YAML files in data")
  expect_true(inherits(log_df, "tbl"))
  expect_equal(nrow(log_df), 0)
  expect_equal(ncol(log_df), 0)
})

test_that("run_log matches reference", {
  log_df <- run_log(MODEL_DIR)
  expect_equal(nrow(log_df), RUN_LOG_ROWS)
  expect_equal(ncol(log_df), RUN_LOG_COLS)
  expect_identical(log_df[[RUN_ID_COL]], c("1", "2", "3"))
  expect_identical(log_df[[RUN_ID_COL]], basename(log_df[[ABS_MOD_PATH]]))
  expect_identical(log_df$tags, list(ORIG_TAGS, NEW_TAGS, ORIG_TAGS))
  expect_identical(log_df$yaml_md5, RUN_LOG_YAML_MD5)
  expect_identical(log_df$based_on, list(NULL, "1", c("1", "2")))

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
    !!YAML_DECISIONS    := "list",
  ) %>% as.list()

  for (.n in names(run_log_classes_ref)) {
    expect_identical(log_classes[[.n]], run_log_classes_ref[[.n]])
  }
})

##########################################
# testing hierarchical nested directories
##########################################

# copy model 1 to level deeper
# TODO: consider unifying this (and the same thing on line 63 of test-config-log.R) with the other create_ functions in setup-workflow-ref.R
fs::dir_create(LEVEL2_DIR)
copy_model_from(MOD1, file.path(LEVEL2_SUBDIR, MOD_ID), "level 2 copy of 1.yaml", .inherit_tags = TRUE)
fs::dir_copy(MOD1_PATH, LEVEL2_MOD)

test_that("run_log() works correctly with nested dirs", {
  log_df <- run_log(MODEL_DIR)
  expect_equal(nrow(log_df), RUN_LOG_ROWS+1)
  expect_equal(ncol(log_df), RUN_LOG_COLS)
  expect_false(any(duplicated(log_df[[ABS_MOD_PATH]])))
  expect_identical(basename(log_df[[ABS_MOD_PATH]]), c("1", "2", "3", "1"))
  expect_identical(log_df$tags, list(ORIG_TAGS, NEW_TAGS, ORIG_TAGS, ORIG_TAGS))
  expect_identical(log_df$yaml_md5, c(RUN_LOG_YAML_MD5, MOD_LEVEL2_MD5))
  expect_identical(log_df$based_on, list(NULL, "1", c("1", "2"), "../1"))
})

##########################################
# testing errors after meddling
##########################################

test_that("run_log fails after messing with YAML", {
  # make the description field an array
  rogue_yaml <- yaml::read_yaml(yaml_ext(NEW_MOD3))
  orig_desc <- rogue_yaml[[YAML_DESCRIPTION]]
  rogue_yaml[[YAML_DESCRIPTION]] <- c(orig_desc, "bad stuff")
  yaml::write_yaml(rogue_yaml, yaml_ext(NEW_MOD3))
  on.exit({
    rogue_yaml[[YAML_DESCRIPTION]] <- orig_desc
    yaml::write_yaml(rogue_yaml, yaml_ext(NEW_MOD3))
  })

  expect_error(log_df <- run_log(MODEL_DIR), regexp = "Must have length 1")
})
