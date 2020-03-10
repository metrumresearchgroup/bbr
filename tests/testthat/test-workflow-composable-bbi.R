####################################################
# testing a composable workflow
# This test file actually runs the bbi calls
# and so it must have working version of both bbi and NONMEM.
# Because of this, it is disabled unless on Metworx.
####################################################

if (Sys.getenv("METWORX_VERSION") == "") {
  skip("test-workflow-composable-bbi only runs on Metworx")
}

withr::with_options(list(rbabylon.bbi_exe_path = '/data/apps/bbi'), {

# define constants
STARTER_FILE <- file.path("model-examples/1.ctl")
MODEL_DIR <- "model-examples-bbi"

NEW_MOD1 <- file.path(MODEL_DIR, "1")
NEW_MOD2 <- file.path(MODEL_DIR, "2")
NEW_MOD3 <- file.path(MODEL_DIR, "3")

ORIG_DESC <- "original acop model"
NEW_DESC <- "new description"

ORIG_TAGS <- c("acop tag", "other tag")
NEW_TAGS <- c("new tag 1", "new tag 2")

NEW_TEXT1 <- c("naw", "paw")
NEW_TEXT2 <- c("all", "done")

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

# setup bbi path
options('rbabylon.bbi_exe_path' = '/data/apps/bbi')

# clear old babylon.yaml
if (fs::file_exists(file.path(MODEL_DIR, "babylon.yaml"))) fs::file_delete(file.path(MODEL_DIR, "babylon.yaml"))

# create new babylon.yaml
bbi_init(MODEL_DIR, "/opt/NONMEM", "nm74gf")

# copy model file into new model dir
fs::file_copy(STARTER_FILE, ctl_ext(NEW_MOD1))

#######################
# create model from R
#######################

test_that("step by step create_model to submit_model to model_summary works", {
  # create model spec
  mod1 <- new_model(
    .yaml_path = yaml_ext(NEW_MOD1),
    .description = ORIG_DESC,
    .tags = ORIG_TAGS,
    .bbi_args = list(overwrite = TRUE, threads = 4)
  )
  expect_identical(class(mod1), MODEL_CLASS_REF)

  # submit model
  proc1 <- submit_model(mod1, .mode = "local", .wait = TRUE)
  expect_identical(class(proc1), PROCESS_CLASS_REF)

  # get summary from model object
  Sys.sleep(3)
  sum1 <- mod1 %>% model_summary()
  expect_identical(class(sum1), SUM_CLASS_REF)
  expect_identical(names(sum1), SUM_NAMES_REF)

  # get summary from process object
  sum2 <- proc1 %>% as_model %>% model_summary()
  expect_identical(class(sum2), SUM_CLASS_REF)
  expect_identical(names(sum2), SUM_NAMES_REF)

  # extract parameters table
  ref_df <- readRDS("data/acop_param_table_ref_200228.rds")
  par_df1 <- param_estimates(sum1); expect_identical(par_df1, ref_df) # from model object
  par_df2 <- param_estimates(sum2); expect_identical(par_df2, ref_df) # from process object

})

####### add in copying from one spec/res to another like in the demo file?


# cleanup
Sys.sleep(3) # wait for some NONMEM mess to delete itself
cleanup()

}) # closing withr::with_options
