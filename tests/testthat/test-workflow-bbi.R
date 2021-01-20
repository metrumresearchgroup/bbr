context("testing a composable workflow and running bbi")

####################################################
# testing a composable workflow
# This test file actually runs the bbi calls
# and so it must have working version of both bbi and NONMEM.
# Because of this, it is disabled unless on Metworx.
#
# Additionally, tests in this file rely on each other
# and therefore must be run in order.
####################################################

# can't run on Drone because there's no NONMEM
if (Sys.getenv("METWORX_VERSION") == "" || Sys.getenv("SKIP_BBI_TEST") == "true") {
  skip("test-workflow-composable-bbi only runs on Metworx")
}

# define constants
MODEL_DIR_BBI <- file.path(dirname(ABS_MODEL_DIR), "test-workflow-bbi-models")

# cleanup function
cleanup_bbi <- function(.recreate_dir = FALSE) {
  if (fs::dir_exists(MODEL_DIR_BBI)) fs::dir_delete(MODEL_DIR_BBI)
  if (isTRUE(.recreate_dir)) fs::dir_create(MODEL_DIR_BBI)
}
cleanup_bbi(.recreate_dir = TRUE)

# set options and run tests
withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {

  # cleanup when done
  on.exit({
    Sys.sleep(3) # wait for some NONMEM mess to delete itself
    cleanup_bbi()
  })

  # clear old bbi.yaml
  if (fs::file_exists(file.path(MODEL_DIR_BBI, "bbi.yaml"))) fs::file_delete(file.path(MODEL_DIR_BBI, "bbi.yaml"))

  # create new bbi.yaml
  bbi_init(MODEL_DIR_BBI, "/opt/NONMEM", "nm74gf")

  # copy model file into new model dir
  fs::file_copy(CTL_TEST_FILE, MODEL_DIR_BBI)

  #######################
  # create model from R
  #######################

  test_that("step by step create_model to submit_model to model_summary works", {
    # create model
    mod1 <- new_model(
      file.path(MODEL_DIR_BBI, "1"),
      .description = "original test-workflow-bbi model",
      .tags = ORIG_TAGS,
      .bbi_args = list(overwrite = TRUE, threads = 4)
    )
    expect_identical(class(mod1), MOD_CLASS_LIST)

    # submit model
    proc1 <- submit_model(mod1, .mode = "local", .wait = TRUE)
    expect_identical(class(proc1), PROC_CLASS_LIST)

    # get summary from model object
    sum1 <- mod1 %>% model_summary()

    # can't check against SUMMARY_REF_FILE because run time, etc. will be different
    # so we just check the structure
    expect_identical(class(sum1), SUM_CLASS_LIST)
    expect_identical(names(sum1), SUM_NAMES_REF)

    # check parameters table
    expect_equal(param_estimates(sum1), dget(PARAM_REF_FILE))
  })

  test_that("copying model works and new models run correctly", {
    mod1 <- read_model(file.path(MODEL_DIR_BBI, "1"))
    mod2 <- copy_model_from(mod1, 2)
    mod3 <- copy_model_from(mod1, 3, .inherit_tags = TRUE) %>% add_bbi_args(list(clean_lvl=2, overwrite = FALSE))

    # run new models
    list(mod2, mod3) %>% submit_models(.mode = "local", .wait = TRUE)

    # get summary from model object
    sum2 <- mod2 %>% model_summary()
    expect_identical(class(sum2), SUM_CLASS_LIST)
    expect_identical(names(sum2), SUM_NAMES_REF)

    # check parameters table
    expect_equal(param_estimates(sum2), dget(PARAM_REF_FILE))

    # add some tags to new model
    mod2 <- mod2 %>% add_tags(NEW_TAGS)

    # check that overwrite error parses correctly
    expect_error(
      submit_model(mod3, .mode = "local", .wait = TRUE),
      regexp = "The target output directory already exists"
    )

  })

  test_that("config_log() works correctly", {
    # check config log for all models so far
    log_df <- config_log(MODEL_DIR_BBI)
    expect_equal(nrow(log_df), 3)
    expect_equal(ncol(log_df), CONFIG_COLS)
    expect_false(any(is.na(log_df$model_md5)))
    expect_false(any(is.na(log_df$data_md5)))
    expect_false(any(is.na(log_df$data_path)))
  })

  test_that(".wait = FALSE returns correctly", {
    # launch a model but don't wait for it to finish
    mod1 <- read_model(file.path(MODEL_DIR_BBI, "1"))
    proc <- copy_model_from(mod1, 4, .inherit_tags = TRUE) %>% submit_model(.mode = "local", .wait = FALSE)
    expect_true(stringr::str_detect(proc[[PROC_STDOUT]], ".wait = FALSE"))
  })

  test_that("run_log() captures runs correctly", {
    # check run log for all models
    log_df <- run_log(MODEL_DIR_BBI)
    expect_equal(nrow(log_df), 4)
    expect_equal(ncol(log_df), RUN_LOG_COLS)
    expect_identical(basename(log_df[[ABS_MOD_PATH]]), as.character(seq(1:4)))
    expect_identical(log_df$description, c("original test-workflow-bbi model", rep(NA_character_, 3)))
    expect_identical(log_df$tags, list(ORIG_TAGS, NEW_TAGS, ORIG_TAGS, ORIG_TAGS))
  })

  test_that("add_config() works with in progress model run", {
    # add config log to run log
    log_df <- expect_warning(
      run_log(MODEL_DIR_BBI) %>% add_config(),
      regexp = "in progress"
    )
    expect_equal(nrow(log_df), 4)
    expect_equal(ncol(log_df), RUN_LOG_COLS + CONFIG_COLS-2)

    # check that the running model has NA for config fields
    expect_equal(sum(is.na(log_df$model_md5)), 1L)
  })


  test_that("submit_model() works with non-NULL .config_path", {
    if (requireNamespace("withr", quietly = TRUE) &&
        utils::packageVersion("withr") < "2.2.0") {
      skip("must have withr >= 2.2.0 to run this test")
    }

    test_dir <- getwd()
    withr::with_tempdir({
      # copy model, YAML, and data files to the same location
      files_to_copy <- file.path(
        ABS_MODEL_DIR,
        c("1.ctl", "1.yaml", "../../../extdata/acop.csv")
      )

      purrr::walk(files_to_copy, fs::file_copy, ".")

      # modify DATA to reflect location in temp dir
      ctl <- readr::read_file("1.ctl")
      ctl_mod <- stringr::str_replace(
        ctl, "\\$DATA\\s+[^\\s]+", "$DATA ../acop.csv"
      )
      readr::write_file(ctl_mod, "1.ctl")

      mod <- read_model("1")
      res <- submit_model(
        mod,
        .mode = "local",
        .config_path = file.path(MODEL_DIR_BBI, "bbi.yaml"),
        .wait = TRUE
      )

      expect_true(any(grepl("--config", res[["cmd_args"]], fixed = TRUE)))
      expect_true(any(grepl("models completed", res[["stdout"]], fixed = TRUE)))
    })
  })

}) # closing withr::with_options

