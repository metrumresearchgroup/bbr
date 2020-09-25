context("Constructing config log from bbi_config.json")

expected_bbi_version <- "v2.3.0"
expected_nonmem_version <- "nm74gf"

# to minimize changes to the existing tests, we define the model and data status
# for each of the models any particular test might need
run_status <- dplyr::tribble(
  ~rel_path, ~model_has_changed, ~data_has_changed,
        "1",              FALSE,             FALSE,
        "2",               TRUE,             FALSE,
        "3",              FALSE,             FALSE,
 "level2/1",               TRUE,             TRUE
)

#' Helper to check config output
#'
#' @param log_df an object of class `bbi_config_log_df`
#' @param run_nums character vector of model run numbers
#' @param col_count number of columns to expect in `log_df`
#' @param run_status a tibble holding model and data status by run
check_config_ref <- function(log_df, run_nums, col_count, run_status) {
  # check log_df class
  expect_true(inherits(log_df, CONF_LOG_CLASS))
  expect_true(inherits(log_df, LOG_DF_CLASS))

  expect_identical(basename(log_df[[ABS_MOD_PATH]]), run_nums)

  run_count <- length(run_nums)

  expect_equal(nrow(log_df), run_count)
  expect_equal(ncol(log_df), col_count)
  expect_false(any(duplicated(log_df[[ABS_MOD_PATH]])))

  # these are the same because bbi_config.json was just copied through
  expect_identical(log_df$data_md5, rep(CONFIG_DATA_MD5, run_count))
  expect_identical(log_df$data_path, rep(CONFIG_DATA_PATH, run_count))
  expect_identical(log_df$model_md5, rep(CONFIG_MODEL_MD5, run_count))

  base_path <- fs::path_common(log_df[["absolute_model_path"]])

  actual_status <-
    run_status %>%
    dplyr::mutate(
      absolute_model_path = as.character(fs::path(base_path, rel_path))
    ) %>%
    dplyr::semi_join(log_df, by = "absolute_model_path")

  expect_identical(
    log_df[["model_has_changed"]],
    actual_status[["model_has_changed"]]
  )
  expect_identical(
    log_df[["data_has_changed"]],
    actual_status[["data_has_changed"]]
  )
}

# TODO: replace setup() and teardown() with 3e test fixtures
setup({
  cleanup()

  # copy models before creating logs
  copy_model_from(MOD1, NEW_MOD2, NEW_DESC, .add_tags = NEW_TAGS)
  copy_model_from(MOD1,
                  NEW_MOD3,
                  NEW_DESC,
                  .based_on_additional = get_model_id(NEW_MOD2),
                  .inherit_tags = TRUE,
                  .update_model_file = FALSE)

  fs::dir_copy(MOD1_PATH, NEW_MOD2)
  fs::dir_copy(MOD1_PATH, NEW_MOD3)

  # copy model 1 to level deeper
  fs::dir_create(LEVEL2_DIR)
  copy_model_from(MOD1, LEVEL2_MOD, "level 2 copy of 1.yaml", .inherit_tags = TRUE)
  fs::dir_copy(MOD1_PATH, LEVEL2_MOD)

})

teardown({ cleanup() })

withr::with_options(list(rbabylon.model_directory = NULL), {

  test_that("config_log() errors with no .base_dir set", {
    log_df <- expect_error(config_log(), regexp = "`.base_dir` cannot be `NULL`")
  })

  test_that("config_log() errors with malformed YAML", {
    log_df <- expect_error(config_log(getwd()), regexp = "Unexpected error.+model_path defined in yaml")
  })

  test_that("config_log() returns NULL and warns when no YAML found", {
    log_df <- expect_warning(config_log("data"), regexp = "Found no valid model YAML files in data")
    expect_true(inherits(log_df, "tbl"))
    expect_equal(nrow(log_df), 0)
    expect_equal(ncol(log_df), 0)
  })

  test_that("config_log() works correctly with nested dirs", {
    log_df <- config_log(MODEL_DIR)
    check_config_ref(
      log_df,
      c("1", "2", "3", "1"),
      CONFIG_COLS,
      run_status
    )
  })

  test_that("config_log(.recurse = FALSE) works", {
    log_df <- config_log(MODEL_DIR, .recurse = FALSE)
    check_config_ref(
      log_df,
      c("1", "2", "3"),
      CONFIG_COLS,
      run_status
    )
  })

  test_that("config_log() reflects model mismatch", {
    # TODO: update this pattern once the model_directory option is deprecated
    perturb_file(CTL_TEST_FILE)
    log_df <- config_log(MODEL_DIR)
    expect_equal(log_df[["model_has_changed"]][1], TRUE)
  })

  test_that("config_log() reflects data mismatch", {
    # TODO: update this pattern once the model_directory option is deprecated
    perturb_file("data/acop.csv")
    log_df <- config_log(MODEL_DIR)
    expect_equal(log_df[["data_has_changed"]][1], TRUE)
  })

  test_that("config_log() includes babylon version", {
    log_df <- config_log(MODEL_DIR)
    expect_equal(log_df[["bbi_version"]][1], expected_bbi_version)
  })

  test_that("config_log() includes NONMEM version", {
    log_df <- config_log(MODEL_DIR)
    expect_equal(log_df[["nm_version"]][1], expected_nonmem_version)
  })

  test_that("add_config() works correctly", {
    log_df <- run_log(MODEL_DIR) %>% add_config()
    check_config_ref(
      log_df,
      c("1", "2", "3", "1"),
      RUN_LOG_COLS + CONFIG_COLS - 1,
      run_status
    )
  })

  test_that("add_config() has correct columns", {
    conf_df <- config_log(MODEL_DIR)
    log_df <- run_log(MODEL_DIR)
    add_df <- log_df %>% add_config()

    # should have all columns from both (minus the join key)
    expect_identical(names(add_df), c(names(log_df), names(conf_df)[2:length(names(conf_df))]))

    # check one col to make sure it matches
    col_to_check <- names(conf_df)[2]
    expect_identical(conf_df[[col_to_check]], add_df[[col_to_check]])
  })

  # THESE TESTS NEED TO BE LAST BECAUSE IT DELETES NECESSARY FILES
  fs::file_delete(file.path(NEW_MOD2, "bbi_config.json"))
  fs::file_delete(file.path(NEW_MOD3, "bbi_config.json"))
  missing_idx <- c(2L, 3L)

  test_that("add_config() works correctly with missing json", {
    log_df <- expect_warning(run_log(MODEL_DIR) %>% add_config(), regexp = "Found only 2 bbi_config.json files for 4 models")
    expect_equal(nrow(log_df), RUN_LOG_ROWS+1)
    expect_equal(ncol(log_df), RUN_LOG_COLS+CONFIG_COLS-1)
    expect_false(any(duplicated(log_df[[ABS_MOD_PATH]])))

    # run_log fields
    expect_identical(basename(log_df[[ABS_MOD_PATH]]), c("1", "2", "3", "1"))
    expect_identical(log_df$tags, list(ORIG_TAGS, NEW_TAGS, ORIG_TAGS, ORIG_TAGS))
    expect_identical(
      log_df[["yaml_md5"]],
      c(
        "ee5a30a015c4e09bc29334188ff28b58",
        "5576ed6fa6e1e4e9b0c25dbf62ae42e5",
        "ebadcc4a3c0f4d16f61251605136942b",
        "6132d34ba27caf3460d23c9b4a3937d9"
      )
    )

    # config log fields
    expect_identical(
      log_df[["data_md5"]],
      rep_missing(CONFIG_DATA_MD5, missing_idx, 4L)
    )
    expect_identical(
      log_df[["data_path"]],
      rep_missing(CONFIG_DATA_PATH, missing_idx, 4L)
    )
    expect_identical(
      log_df[["model_md5"]],
      rep_missing(CONFIG_MODEL_MD5, missing_idx, 4L)
    )
    expect_identical(
      log_df[["bbi_version"]],
      rep_missing(expected_bbi_version, missing_idx, 4L)
    )
    expect_identical(
      log_df[["nm_version"]],
      rep_missing(expected_nonmem_version, missing_idx, 4L)
    )
  })

}) # closing withr::with_options
