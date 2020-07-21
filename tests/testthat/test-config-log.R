context("Constructing config log from bbi_config.json")

source("data/test-workflow-ref.R")

check_config_ref <- function(log_df, run_nums, col_count) {

  expect_identical(basename(log_df[[ABS_MOD_PATH]]), run_nums)

  run_count <- length(run_nums)

  expect_equal(nrow(log_df), run_count)
  expect_equal(ncol(log_df), col_count)
  expect_false(any(duplicated(log_df[[ABS_MOD_PATH]])))

  # these are the same because bbi_config.json was just copied through
  expect_identical(log_df$data_md5, rep("4ddb44da897c26681d892aa7be99f74b", run_count))
  expect_identical(log_df$data_path, rep("../../data/acop.csv", run_count))
  expect_identical(log_df$model_md5, rep("731923458236cc008c3adafa2f0877a7", run_count))
}

setup({
  cleanup()

  # copy models before creating logs
  copy_model_from(YAML_TEST_FILE, NEW_MOD2, NEW_DESC, .add_tags = NEW_TAGS)
  copy_model_from(YAML_TEST_FILE,
                  NEW_MOD3,
                  NEW_DESC,
                  .based_on_additional = get_model_id(NEW_MOD2),
                  .inherit_tags = TRUE,
                  .update_model_file = FALSE)


  CONFIG_1 <- file.path(tools::file_path_sans_ext(YAML_TEST_FILE), "bbi_config.json")
  fs::dir_create(NEW_MOD2)
  fs::dir_create(NEW_MOD3)
  fs::file_copy(CONFIG_1, file.path(NEW_MOD2, "bbi_config.json"))
  fs::file_copy(CONFIG_1, file.path(NEW_MOD3, "bbi_config.json"))

  # copy model 1 to level deeper
  fs::dir_create(LEVEL2_DIR)
  copy_model_from(YAML_TEST_FILE, LEVEL2_MOD, "level 2 copy of 1.yaml", .inherit_tags = TRUE)
  fs::dir_copy(tools::file_path_sans_ext(YAML_TEST_FILE), file.path(LEVEL2_DIR, tools::file_path_sans_ext(basename(YAML_TEST_FILE))))

})

teardown({ cleanup() })

withr::with_options(list(rbabylon.model_directory = NULL), {

  test_that("config_log() works correctly with nested dirs", {
    log_df <- config_log(MODEL_DIR)
    check_config_ref(log_df, c("1", "2", "3", "1"), CONFIG_COLS)
  })

  test_that("config_log(.recurse = FALSE) works", {
    log_df <- config_log(MODEL_DIR, .recurse = FALSE)
    check_config_ref(log_df, c("1", "2", "3"), CONFIG_COLS)
  })

  test_that("add_config() works correctly", {
    log_df <- run_log(MODEL_DIR) %>% add_config()
    check_config_ref(log_df, c("1", "2", "3", "1"), RUN_LOG_COLS+CONFIG_COLS-1)
  })

  # THESE TESTS NEEDS TO BE LAST BECAUSE IT DELETES NECESSARY FILES
  fs::file_delete(file.path(NEW_MOD2, "bbi_config.json"))
  fs::file_delete(file.path(NEW_MOD3, "bbi_config.json"))

  test_that("add_config() works correctly with missing json", {
    log_df <- expect_warning(run_log(MODEL_DIR) %>% add_config(), regexp = "Found only 2 bbi_config.json files for 4 models")
    expect_equal(nrow(log_df), RUN_LOG_ROWS+1)
    expect_equal(ncol(log_df), RUN_LOG_COLS+CONFIG_COLS-1)
    expect_false(any(duplicated(log_df[[ABS_MOD_PATH]])))

    # run_log fields
    expect_identical(basename(log_df[[ABS_MOD_PATH]]), c("1", "2", "3", "1"))
    expect_identical(log_df$tags, list(ORIG_TAGS, NEW_TAGS, ORIG_TAGS, ORIG_TAGS))
    expect_identical(log_df$yaml_md5, c("ee5a30a015c4e09bc29334188ff28b58", "5576ed6fa6e1e4e9b0c25dbf62ae42e5", "ebadcc4a3c0f4d16f61251605136942b", "6132d34ba27caf3460d23c9b4a3937d9"))

    # config log fields
    expect_identical(log_df$data_md5, c("4ddb44da897c26681d892aa7be99f74b", NA_character_, NA_character_, "4ddb44da897c26681d892aa7be99f74b"))
    expect_identical(log_df$data_path, c("../../data/acop.csv", NA_character_, NA_character_, "../../data/acop.csv"))
    expect_identical(log_df$model_md5, c("731923458236cc008c3adafa2f0877a7", NA_character_, NA_character_, "731923458236cc008c3adafa2f0877a7"))
  })

}) # closing withr::with_options
