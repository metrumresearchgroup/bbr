####################################################
# testing a composable workflow
# This test file actually runs the bbi calls
# and so it must have working version of both bbi and NONMEM.
# Because of this, it is disabled unless on Metworx.
####################################################

if (Sys.getenv("METWORX_VERSION") == "") skip("test-workflow-composable-bbi only runs on Metworx")

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
SPEC_CLASS_REF <- c("bbi_nonmem_spec", "list")
RES_CLASS_REF <- c("bbi_nonmem_result", "babylon_result", "bbi_nonmem_spec", "list")
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
  spec1 <- create_model(
    .yaml_path = yaml_ext(NEW_MOD1),
    .description = ORIG_DESC,
    .tags = ORIG_TAGS,
    .bbi_args = list(overwrite = TRUE, threads = 4)
  )
  expect_identical(class(spec1), SPEC_CLASS_REF)

  # submit model
  res1 <- submit_model(spec1)
  expect_identical(class(res1), RES_CLASS_REF)

  # expect error on submit with no wait
  expect_error(model_summary(res1, .wait = NULL))

  # Try to get a summary and wait if not finished.
  sum1 <- model_summary(res1)
  expect_identical(class(sum1), SUM_CLASS_REF)
  expect_identical(names(sum1), SUM_NAMES_REF)

  # extract parameters table
  par_df1 <- param_estimates(sum1)
  ref_df <- readRDS("data/acop_param_table_ref_200228.rds")
  expect_identical(par_df1, ref_df)

})

####### add in copying from one spec/res to another like in the demo file?


# cleanup
Sys.sleep(3) # wait for some NONMEM mess to delete itself
cleanup()
