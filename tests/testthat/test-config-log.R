context("Constructing config log from bbi_config.json")

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

# setup
cleanup()
create_rlg_models()
# copy model 1 to level deeper
fs::dir_create(LEVEL2_DIR)
copy_model_from(MOD1, file.path(LEVEL2_SUBDIR, MOD_ID), "level 2 copy of 1.yaml", .inherit_tags = TRUE)
copy_all_output_dirs()

# teardown
withr::defer(cleanup())

test_that("config_log() returns NULL and warns when no YAML found [BBR-CGLG-001]", {
  log_df <- expect_warning(config_log("."), regexp = "Found no valid model YAML files in")
  expect_true(inherits(log_df, "tbl"))
  expect_equal(nrow(log_df), 0)
  expect_equal(ncol(log_df), 0)
})

test_that("config_log() works correctly with nested dirs [BBR-CGLG-002]", {
  log_df <- config_log(MODEL_DIR)
  check_config_ref(
    log_df,
    c("1", "2", "3", "1"),
    CONFIG_COLS,
    run_status
  )
})

test_that("config_log(.recurse = FALSE) works [BBR-CGLG-003]", {
  log_df <- config_log(MODEL_DIR, .recurse = FALSE)
  check_config_ref(
    log_df,
    c("1", "2", "3"),
    CONFIG_COLS,
    run_status
  )
})

test_that("config_log() reflects model mismatch [BBR-CGLG-004]", {
  perturb_file(CTL_TEST_FILE)
  log_df <- config_log(MODEL_DIR)
  expect_equal(log_df[["model_has_changed"]][1], TRUE)
})

test_that("config_log() reflects data mismatch [BBR-CGLG-005]", {
  perturb_file(system.file("extdata", "acop.csv", package = "bbr"))
  log_df <- config_log(MODEL_DIR)
  expect_equal(log_df[["data_has_changed"]][1], TRUE)
})

test_that("config_log() includes bbi version [BBR-CGLG-006]", {
  log_df <- config_log(MODEL_DIR)
  expect_equal(log_df[["bbi_version"]][1], MOD_BBI_VERSION)
})

test_that("config_log() includes NONMEM version [BBR-CGLG-007]", {
  log_df <- config_log(MODEL_DIR)
  expect_equal(log_df[["nm_version"]][1], MOD_NM_VERSION)
})

test_that("add_config() works correctly [BBR-CGLG-008]", {
  log_df <- run_log(MODEL_DIR) %>% add_config()
  check_config_ref(
    log_df,
    c("1", "2", "3", "1"),
    RUN_LOG_COLS + CONFIG_COLS-2,
    run_status
  )
})

test_that("add_config() has correct columns [BBR-CGLG-009]", {
  conf_df <- config_log(MODEL_DIR)
  log_df <- run_log(MODEL_DIR)
  add_df <- log_df %>% add_config()

  # should have all columns from both (minus the join key)
  expect_identical(names(add_df), c(names(log_df), names(conf_df)[3:length(names(conf_df))]))

  # check one col to make sure it matches
  col_to_check <- names(conf_df)[3]
  expect_identical(conf_df[[col_to_check]], add_df[[col_to_check]])
})

# THESE TESTS NEED TO BE LAST BECAUSE IT DELETES NECESSARY FILES
fs::file_delete(file.path(NEW_MOD3, "bbi_config.json"))
fs::file_delete(file.path(LEVEL2_MOD, "bbi_config.json"))
missing_idx <- c(3L, 4L)

test_that("add_config() works correctly with missing json [BBR-CGLG-010]", {
  log_df <- expect_warning(run_log(MODEL_DIR) %>% add_config(), regexp = "Found only 2 bbi_config.json files for 4 models")
  expect_equal(nrow(log_df), RUN_LOG_ROWS+1)
  expect_equal(ncol(log_df), RUN_LOG_COLS+CONFIG_COLS-2)
  expect_false(any(duplicated(log_df[[ABS_MOD_PATH]])))

  # run_log fields
  expect_identical(basename(log_df[[ABS_MOD_PATH]]), c("1", "2", "3", "1"))
  expect_identical(log_df$tags, list(ORIG_TAGS, NEW_TAGS, ORIG_TAGS, ORIG_TAGS))
  expect_identical(
    log_df[["yaml_md5"]],
    c(RUN_LOG_YAML_MD5, MOD_LEVEL2_MD5)
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
    rep_missing(MOD_BBI_VERSION, missing_idx, 4L)
  )
  expect_identical(
    log_df[["nm_version"]],
    rep_missing(MOD_NM_VERSION, missing_idx, 4L)
  )
})

fs::dir_delete(NEW_MOD3)
fs::dir_delete(LEVEL2_MOD)

test_that("config_log() works with missing output dirs [BBR-CGLG-011]", {
  log_df <- expect_warning(
    config_log(MODEL_DIR),
    regexp = "Found only 2 bbi_config.json files for 4 models"
  )
  expect_true(inherits(log_df, CONF_LOG_CLASS))
  expect_equal(nrow(log_df), RUN_LOG_ROWS+1-2)
  expect_equal(ncol(log_df), CONFIG_COLS)
  expect_false(any(duplicated(log_df[[ABS_MOD_PATH]])))
})

test_that("config_log() works with no json found [BBR-CGLG-012]", {

  expect_warning({
    log_df <- config_log(LEVEL2_DIR)
  }, regexp = "Found no bbi_config")

  expect_equal(nrow(log_df), 0)
  expect_equal(names(log_df), c(ABS_MOD_PATH, RUN_ID_COL))
})

test_that("add_config() works no json found [BBR-CGLG-013]", {

  expect_warning({
    log_df <- run_log(LEVEL2_DIR) %>% add_config()
  }, regexp = "Found no bbi_config")

  expect_equal(nrow(log_df), 1)
  expect_true(all(c(ABS_MOD_PATH, RUN_ID_COL, YAML_TAGS) %in% names(log_df)))
})

# ##########################################
# # Testing Additional Parameters Passed
# ##########################################


test_that("config_log() works with filtering parameter numeric [BBR-CGLG-014]",
          {
            setup_this_test <- function() {
              create_rlg_models()
              purrr::walk(
                c(2, 3),
                ~system("cp -r {MODEL_DIR}/1/  {MODEL_DIR}/{.x}/" %>% glue())
              )
            }



            clean_test_enviroment(setup_this_test)


            log_df <- list(df = config_log(MODEL_DIR), length = config_log(MODEL_DIR) %>% nrow())


            expect_equal(config_log(MODEL_DIR, .exclude  = 1:(log_df$length - 1) ) %>% nrow(), 1)
            expect_equal(config_log(MODEL_DIR, .exclude = 1:(log_df$length - 2)) %>% nrow(), 2)
            expect_equal(config_log(MODEL_DIR, .exclude = (log_df$length - 2):1) %>% nrow(), 2)

          })

test_that("config_log() works with filtering parameter string [BBR-CGLG-014]",
          {
            setup_this_test <- function() {
              create_rlg_models()
              copy_model_from(MOD1, "Child")
              copy_model_from(MOD1, "Parent")
              purrr::walk(
                c("2", "3", "Child", "Parent"),
                ~system("cp -r {MODEL_DIR}/1/  {MODEL_DIR}/{.x}/" %>% glue())
              )

            }


            clean_test_enviroment(setup_this_test)


            log_df <- list(df = run_log(MODEL_DIR), length = config_log(MODEL_DIR) %>% nrow())
            expect_equal(config_log(MODEL_DIR, .exclude  = c(1:2, "Child")) %>% nrow() ,2)
            expect_equal(config_log(MODEL_DIR, .exclude = c(2:1, "Child")) %>% nrow() ,2)
            expect_equal(config_log(MODEL_DIR, .exclude = c("Child", 1, 2, 3)) %>% nrow() ,1)
            expect_equal(config_log(MODEL_DIR, .exclude =  c(1:2, "Parent")) %>% nrow() ,2)

            dir_delete("{MODEL_DIR}/Parent" %>% glue())
            dir_delete("{MODEL_DIR}/Child" %>% glue())
          })

