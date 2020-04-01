####################################################
# testing a composable workflow
# This test file actually runs the bbi calls
# and so it must have working version of both bbi and NONMEM.
# Because of this, it is disabled unless on Metworx.
####################################################

if (Sys.getenv("METWORX_VERSION") == "") {
  skip("test-workflow-composable-bbi only runs on Metworx")
}

# define constants
STARTER_FILE <- file.path("model-examples/1.ctl")
PARAM_REF_FILE <- "data/acop_param_table_ref_200228.rds"

MODEL_DIR <- "model-examples-bbi"
BBI_PATH <- '/data/apps/bbi'

ORIG_DESC <- "original acop model"
NEW_DESC <- "new description"

ORIG_TAGS <- c("acop tag", "other tag")
NEW_TAGS <- c("acop tag", "new tag")

# reference
MODEL_CLASS_REF <- c("bbi_nonmem_model", "list")
PROCESS_CLASS_REF <- c("babylon_process", "list")
SUM_CLASS_REF <- c("bbi_nonmem_summary", "list")
SUM_NAMES_REF <- c("run_details", "run_heuristics", "parameters_data", "parameter_names",
                   "ofv", "shrinkage_details", "covariance_theta", "correlation_theta")

# cleanup function
cleanup <- function(.recreate_dir = FALSE) {
  if (fs::dir_exists(MODEL_DIR)) fs::dir_delete(MODEL_DIR)
  if (isTRUE(.recreate_dir)) fs::dir_create(MODEL_DIR)
}
cleanup(.recreate_dir = TRUE)

# set options and run tests
withr::with_options(list(rbabylon.bbi_exe_path = BBI_PATH,
                         rbabylon.model_directory = MODEL_DIR), {

  # clear old babylon.yaml
  if (fs::file_exists(file.path(MODEL_DIR, "babylon.yaml"))) fs::file_delete(file.path(MODEL_DIR, "babylon.yaml"))

  # create new babylon.yaml
  bbi_init(MODEL_DIR, "/opt/NONMEM", "nm74gf")

  # copy model file into new model dir
  fs::file_copy(STARTER_FILE, MODEL_DIR)

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
    expect_identical(class(mod1), MODEL_CLASS_REF)

    # submit model
    proc1 <- submit_model(mod1, .mode = "local", .wait = TRUE)
    expect_identical(class(proc1), PROCESS_CLASS_REF)

    # get summary from model object
    sum1a <- mod1 %>% model_summary()
    expect_identical(class(sum1a), SUM_CLASS_REF)
    expect_identical(names(sum1a), SUM_NAMES_REF)

    # get summary from process object
    sum1b <- proc1 %>% as_model %>% model_summary()
    expect_identical(class(sum1b), SUM_CLASS_REF)
    expect_identical(names(sum1b), SUM_NAMES_REF)

    # extract parameters table
    ref_df <- readRDS(PARAM_REF_FILE)
    par_df1a <- param_estimates(sum1a); expect_identical(par_df1a, ref_df) # from model object
    par_df1b <- param_estimates(sum1b); expect_identical(par_df1b, ref_df) # from process object

  })

  test_that("copying model works and new model runs correctly", {
    # copy model
    mod2 <- copy_model_from(1, 2, NEW_DESC)

    # run new model
    mod2 %>% submit_model(.mode = "local", .wait = TRUE)

    # get summary from model object
    sum2 <- mod2 %>% model_summary()
    expect_identical(class(sum2), SUM_CLASS_REF)
    expect_identical(names(sum2), SUM_NAMES_REF)

    # extract parameters table
    ref_df <- readRDS(PARAM_REF_FILE)
    par_df2 <- param_estimates(sum2); expect_identical(par_df2, ref_df) # from model object

    # add some tags to new model
    mod2 <- mod2 %>% add_tags(NEW_TAGS)

  })

  test_that("run_log() captures runs correctly", {
    # check run log for both models
    log_df <- run_log()
    expect_equal(nrow(log_df), 2)
    expect_equal(ncol(log_df), 12)
    expect_identical(log_df$run_id, c("1", "2"))
    expect_identical(log_df$tags, list(ORIG_TAGS, NEW_TAGS))

    # add config log
    log_df <- log_df %>% add_config()
    expect_equal(nrow(log_df), 2)
    expect_equal(ncol(log_df), 16)
    expect_false(any(is.null(log_df$data_md5)))
  })

}) # closing withr::with_options

# cleanup
Sys.sleep(3) # wait for some NONMEM mess to delete itself
cleanup()
