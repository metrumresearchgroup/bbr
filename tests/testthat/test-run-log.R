context("Constructing run log from model yaml")

source("data/test-workflow-ref.R")

withr::with_options(list(rbabylon.model_directory = NULL), {
  cleanup()
  on.exit({ cleanup() })

  # copy models before creating run log
  copy_model_from(YAML_TEST_FILE, NEW_MOD2, NEW_DESC, .add_tags = NEW_TAGS)
  copy_model_from(YAML_TEST_FILE,
                  NEW_MOD3,
                  NEW_DESC,
                  .based_on_additional = get_model_id(NEW_MOD2),
                  .inherit_tags = TRUE,
                  .update_model_file = FALSE)

  test_that("run_log matches reference", {
    log_df <- run_log(MODEL_DIR)
    expect_equal(nrow(log_df), 3)
    expect_equal(ncol(log_df), 8)
    expect_identical(basename(log_df[[ABS_MOD_PATH]]), c("1", "2", "3"))
    expect_identical(log_df$tags, list(ORIG_TAGS, NEW_TAGS, ORIG_TAGS))
    expect_identical(log_df$yaml_md5, c("ee5a30a015c4e09bc29334188ff28b58", "5576ed6fa6e1e4e9b0c25dbf62ae42e5", "ebadcc4a3c0f4d16f61251605136942b"))
    expect_identical(log_df$based_on, list(NULL, "1", c("1", "2")))

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


  test_that("run_log() works with both yaml and yml", {
    fake_yml <- stringr::str_replace(YAML_TEST_FILE, "1.yaml", "4.yml")
    fs::file_copy(YAML_TEST_FILE, fake_yml)

    log_df <- suppressSpecificWarning({
      run_log(MODEL_DIR)
    }, .regexpr = "No model file found")

    expect_equal(nrow(log_df), 4)
    expect_equal(ncol(log_df), 8)
    expect_identical(basename(log_df[[ABS_MOD_PATH]]), c("1", "2", "3", "4"))
    expect_identical(log_df$yaml_md5, c("ee5a30a015c4e09bc29334188ff28b58", "5576ed6fa6e1e4e9b0c25dbf62ae42e5", "ebadcc4a3c0f4d16f61251605136942b", "ee5a30a015c4e09bc29334188ff28b58"))

    fs::file_delete(fake_yml)

  })

  ##########################################
  # testing hierarchical nested directories
  ##########################################

  # copy model 1 to level deeper
  fs::dir_create(LEVEL2_DIR)
  copy_model_from(YAML_TEST_FILE, LEVEL2_MOD, "level 2 copy of 1.yaml", .inherit_tags = TRUE)
  fs::dir_copy(tools::file_path_sans_ext(YAML_TEST_FILE), file.path(LEVEL2_DIR, tools::file_path_sans_ext(basename(YAML_TEST_FILE))))

  test_that("run_log() works correctly with nested dirs", {
    log_df <- run_log(MODEL_DIR)
    expect_equal(nrow(log_df), 4)
    expect_equal(ncol(log_df), 8)
    expect_false(any(duplicated(log_df[[ABS_MOD_PATH]])))
    expect_identical(basename(log_df[[ABS_MOD_PATH]]), c("1", "2", "3", "1"))
    expect_identical(log_df$tags, list(ORIG_TAGS, NEW_TAGS, ORIG_TAGS, ORIG_TAGS))
    expect_identical(log_df$yaml_md5, c("ee5a30a015c4e09bc29334188ff28b58", "5576ed6fa6e1e4e9b0c25dbf62ae42e5", "ebadcc4a3c0f4d16f61251605136942b", "6132d34ba27caf3460d23c9b4a3937d9"))
    expect_identical(log_df$based_on, list(NULL, "1", c("1", "2"), "../1"))
  })

  test_that("config_log() works correctly with nested dirs", {
    log_df <- config_log(MODEL_DIR)
    expect_equal(nrow(log_df), 2)
    expect_equal(ncol(log_df), 4)
    expect_false(any(duplicated(log_df[[ABS_MOD_PATH]])))
    expect_identical(basename(log_df[[ABS_MOD_PATH]]), c("1", "1"))
    expect_identical(log_df$data_md5, c("4ddb44da897c26681d892aa7be99f74b", "4ddb44da897c26681d892aa7be99f74b"))
    expect_identical(log_df$data_path, c("../../data/acop.csv", "../../data/acop.csv"))
    expect_identical(log_df$model_md5, c("731923458236cc008c3adafa2f0877a7", "731923458236cc008c3adafa2f0877a7")) # these are the same because bbi_config.json was just copied through
  })

  test_that("add_config() works correctly with nested dirs", {
    log_df <- expect_warning(run_log(MODEL_DIR) %>% add_config(), regexp = "found 4 runs but found 2 configs")
    expect_equal(nrow(log_df), 4)
    expect_equal(ncol(log_df), 11)
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


  ##########################################
  # testing errors after meddling
  ##########################################

  test_that("run_log fails after messing with YAML", {
    # make the description field an array
    rogue_yaml <- yaml::read_yaml(yaml_ext(NEW_MOD3))
    rogue_yaml[[YAML_DESCRIPTION]] <- c(rogue_yaml[[YAML_DESCRIPTION]], "bad stuff")
    yaml::write_yaml(rogue_yaml, yaml_ext(NEW_MOD3))

    expect_error(log_df <- run_log(MODEL_DIR), regexp = "expected to have length of")
  })

}) # closing withr::with_options
