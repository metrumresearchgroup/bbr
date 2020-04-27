context("Workflow file manipulation")

# define constants
MODEL_DIR <- "model-examples"
YAML_TEST_FILE <- file.path(MODEL_DIR, "1.yaml")
NEW_MOD2 <- file.path(MODEL_DIR, "2")
NEW_MOD3 <- file.path(MODEL_DIR, "3")

ORIG_DESC <- "original acop model"
NEW_DESC <- "new description"
DESC_IN_CTL <- "PK model 1 cmt base"

ORIG_TAGS <- c("acop tag", "other tag")
NEW_TAGS <- c("new tag 1", "new tag 2")

NEW_TEXT1 <- c("naw", "paw")
NEW_TEXT2 <- c("all", "done")

MODEL_CLASS_LIST <- c("bbi_nonmem_model", "list")

withr::with_options(list(rbabylon.model_directory = NULL), {





  test_that("run_log matches reference", {
    log_df <- suppressWarnings(run_log("model-examples"))
    expect_equal(nrow(log_df), 3)
    expect_equal(ncol(log_df), 12)
    expect_identical(log_df$run_id, c("1", "2", "3"))
    expect_identical(log_df$tags, list(ORIG_TAGS, NEW_TAGS, ORIG_TAGS))
    expect_identical(log_df$yaml_md5, c("ee5a30a015c4e09bc29334188ff28b58", "5576ed6fa6e1e4e9b0c25dbf62ae42e5", "ebadcc4a3c0f4d16f61251605136942b"))

    # check class of each column
    log_classes <- log_df %>% dplyr::summarise_all(class) %>% as.list()

    run_log_classes_ref <- tibble::tibble(
      run_id = "character",
      !!WORKING_DIR       := "character",
      !!YAML_YAML_NAME    := "character",
      !!YAML_YAML_MD5     := "character",
      !!YAML_MOD_TYPE     := "character",
      !!YAML_DESCRIPTION  := "character",
      !!YAML_MOD_PATH     := "character",
      !!YAML_BBI_ARGS     := "list",
      !!YAML_BASED_ON     := "list",
      !!YAML_TAGS         := "list",
      !!YAML_DECISIONS    := "list",
      !!YAML_OUT_DIR      := "character"
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

  # cleanup temp files
  for (m in c(NEW_MOD2, NEW_MOD3)) {
    if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
    if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
  }

}) # closing withr::with_options
