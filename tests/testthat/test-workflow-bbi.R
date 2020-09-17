context("testing a composable workflow and running bbi")

####################################################
# testing a composable workflow
# This test file actually runs the bbi calls
# and so it must have working version of both bbi and NONMEM.
# Because of this, it is disabled unless on Metworx.
####################################################

# can't run on Drone because there's no NONMEM
if (Sys.getenv("METWORX_VERSION") == "" || Sys.getenv("SKIP_BBI_TEST") == "true") {
  skip("test-workflow-composable-bbi only runs on Metworx")
}

# define constants
STARTER_FILE <- file.path("model-examples/1.ctl")
MODEL_DIR_BBI <- "model-examples-bbi"
BBI_PATH <- read_bbi_path()

# cleanup function
cleanup_bbi <- function(.recreate_dir = FALSE) {
  if (fs::dir_exists(MODEL_DIR_BBI)) fs::dir_delete(MODEL_DIR_BBI)
  if (isTRUE(.recreate_dir)) fs::dir_create(MODEL_DIR_BBI)
}
cleanup_bbi(.recreate_dir = TRUE)

# set options and run tests
withr::with_options(list(rbabylon.bbi_exe_path = BBI_PATH,
                         rbabylon.model_directory = normalizePath(MODEL_DIR_BBI)), {

  # cleanup when done
  on.exit({
    Sys.sleep(3) # wait for some NONMEM mess to delete itself
    cleanup_bbi()
  })

  # clear old babylon.yaml
  if (fs::file_exists(file.path(MODEL_DIR_BBI, "babylon.yaml"))) fs::file_delete(file.path(MODEL_DIR_BBI, "babylon.yaml"))

  # create new babylon.yaml
  bbi_init(MODEL_DIR_BBI, "/opt/NONMEM", "nm74gf")

  # copy model file into new model dir
  fs::file_copy(STARTER_FILE, MODEL_DIR_BBI)

  #######################
  # create model from R
  #######################

  test_that("step by step create_model to submit_model to model_summary works", {
    # create model spec
    mod1 <- suppressWarnings(new_model(
      .yaml_path = 1,
      .description = ORIG_DESC,
      .tags = ORIG_TAGS,
      .bbi_args = list(overwrite = TRUE, threads = 4)
    ))
    expect_identical(class(mod1), MOD_CLASS_LIST)

    # submit model
    proc1 <- submit_model(mod1, .mode = "local", .wait = TRUE)
    expect_identical(class(proc1), PROC_CLASS_LIST)

    # get summary from model object
    sum1a <- mod1 %>% model_summary()
    expect_identical(class(sum1a), SUM_CLASS_LIST)
    expect_identical(names(sum1a), SUM_NAMES_REF)

    # get summary from process object
    sum1b <- proc1 %>% as_model %>% model_summary()
    expect_identical(class(sum1b), SUM_CLASS_LIST)
    expect_identical(names(sum1b), SUM_NAMES_REF)

    # extract parameters table
    ref_df <- readRDS(PARAM_REF_FILE)

    par_df1a <- param_estimates(sum1a)
    suppressSpecificWarning({
      expect_equal(par_df1a, ref_df) # from model object
    }, .regexpr = "Column .+ has different attributes on LHS and RHS of join")

    par_df1b <- param_estimates(sum1b)
    suppressSpecificWarning({
      expect_equal(par_df1b, ref_df) # from process object
    }, .regexpr = "Column .+ has different attributes on LHS and RHS of join")
  })

  test_that("copying model works and new models run correctly", {
    # copy model
    mod2 <- copy_model_from(1, 2, NEW_DESC)
    mod3 <- copy_model_from(1, 3, NEW_DESC, .inherit_tags = TRUE) %>% add_bbi_args(list(clean_lvl=2, overwrite = FALSE))

    # run new model
    list(mod2, mod3) %>% submit_models(.mode = "local", .wait = TRUE)

    # get summary from model object
    sum2 <- mod2 %>% model_summary()
    expect_identical(class(sum2), SUM_CLASS_LIST)
    expect_identical(names(sum2), SUM_NAMES_REF)

    # extract parameters table
    ref_df <- readRDS(PARAM_REF_FILE)
    par_df2 <- param_estimates(sum2)
    suppressSpecificWarning({
      expect_equal(par_df2, ref_df) # from process object
    }, .regexpr = "Column .+ has different attributes on LHS and RHS of join")

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
    log_df <- config_log()
    expect_equal(nrow(log_df), 3)
    expect_equal(ncol(log_df), CONFIG_COLS)
    expect_false(any(is.na(log_df$model_md5)))
    expect_false(any(is.na(log_df$data_md5)))
    expect_false(any(is.na(log_df$data_path)))
  })

  test_that(".wait = FALSE returns correctly", {
    # launch a model but don't wait for it to finish
    proc <- copy_model_from(1, 4, NEW_DESC, .inherit_tags = TRUE) %>% submit_model(.mode = "local", .wait = FALSE)
    expect_true(stringr::str_detect(proc[[PROC_STDOUT]], ".wait = FALSE"))
  })

  test_that("run_log() captures runs correctly", {
    # check run log for all models
    log_df <- run_log()
    expect_equal(nrow(log_df), 4)
    expect_equal(ncol(log_df), 8)
    expect_identical(basename(log_df[[ABS_MOD_PATH]]), c("1", "2", "3", "4"))
    expect_identical(log_df$tags, list(ORIG_TAGS, NEW_TAGS, ORIG_TAGS, ORIG_TAGS))
  })

  test_that("add_config() md5 matches original md5", {
    # add config log to run log
    log_df <- expect_warning(run_log() %>% add_config(), regexp = "in progress")
    expect_equal(nrow(log_df), 4)
    expect_equal(ncol(log_df), RUN_LOG_COLS + CONFIG_COLS - 1)

    # check config md5's against ctl md5's
    log_df <- log_df %>% filter(!is.na(model_md5))
    expect_equal(nrow(log_df), 3)

    norm_data_paths <- fs::path_norm(file.path(log_df$absolute_model_path, log_df$data_path))
    norm_model_paths <- get_model_path(log_df)

    log_df <- log_df %>% mutate(
                            current_data_md5  = tools::md5sum(norm_data_paths),
                            data_md5_match    = .data$data_md5 == .data$current_data_md5,
                            current_model_md5  = tools::md5sum(norm_model_paths),
                            model_md5_match   = .data$model_md5 == .data$current_model_md5
                          )

    expect_true(all(log_df$data_md5_match))
    expect_true(all(log_df$model_md5_match))
  })

}) # closing withr::with_options

