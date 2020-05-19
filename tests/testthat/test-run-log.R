context("Constructing run log from model yaml")

source("data/test-workflow-ref.R")

withr::with_options(list(rbabylon.model_directory = NULL), {

  # copy models before creating run log
  cleanup()
  copy_model_from(YAML_TEST_FILE, NEW_MOD2, NEW_DESC, .add_tags = NEW_TAGS)
  copy_model_from(YAML_TEST_FILE,
                  NEW_MOD3,
                  NEW_DESC,
                  .based_on_additional = get_model_id(NEW_MOD2),
                  .inherit_tags = TRUE,
                  .update_model_file = FALSE)

  test_that("run_log matches reference", {
    #log_df <- suppressWarnings(run_log("model-examples"))
    log_df <- run_log("model-examples")
    expect_equal(nrow(log_df), 3)
    expect_equal(ncol(log_df), 8)
    expect_identical(basename(log_df[[ABS_MOD_PATH]]), c("1", "2", "3"))
    expect_identical(log_df$tags, list(ORIG_TAGS, NEW_TAGS, ORIG_TAGS))
    expect_identical(log_df$yaml_md5, c("ee5a30a015c4e09bc29334188ff28b58", "5576ed6fa6e1e4e9b0c25dbf62ae42e5", "ebadcc4a3c0f4d16f61251605136942b"))

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

  test_that("run_log fails after messing with YAML", {
    # make the description field an array
    rogue_yaml <- yaml::read_yaml(yaml_ext(NEW_MOD3))
    rogue_yaml[[YAML_DESCRIPTION]] <- c(rogue_yaml[[YAML_DESCRIPTION]], "bad stuff")
    yaml::write_yaml(rogue_yaml, yaml_ext(NEW_MOD3))

    expect_error(log_df <- run_log("model-examples"), regexp = "expected to have length of")
  })

  cleanup()

}) # closing withr::with_options
