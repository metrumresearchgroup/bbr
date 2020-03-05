context("Test bbi summary functions")

# constants
MODEL_FILE <- "1.ctl"
MODEL_YAML <- yaml_ext(MODEL_FILE)
MODEL_DIR <- "model-examples"
MOD1_PATH <- file.path(MODEL_DIR, "1")
MOD1 <- MOD1_PATH %>% read_model()
MOD2_PATH <- file.path(MODEL_DIR, "2")

# references
SUM_CLASS_LIST <- c("bbi_nonmem_summary", "list")
NOT_FINISHED_ERR_MSG <- "nonmem_summary.*modeling run has not finished"
NO_LST_ERR_MSG <- "Unable to locate `.lst` file.*NONMEM output folder"


#########################################
# extracting things from summary object
#########################################

test_that("model_summary() produces expected list object", {
  # get summary
  sum1 <- MOD1 %>% model_summary()

  # check class
  expect_identical(class(sum1), SUM_CLASS_LIST)

  # compare to reference
  ref_sum <- readRDS("data/acop_summary_obj_ref_200305.rds")
  expect_equal(ref_sum, sum1)
})

test_that("model_summary() fails predictably if it can't find some parts (i.e. model isn't finished)", {
  # create new model
  mod2 <- MOD1 %>% copy_model_from(MOD2_PATH, .description = "number 2")

  # copy output directory (to simulate model run)
  fs::dir_copy(MOD1_PATH, MOD2_PATH)

  # delete a necessary file
  fs::file_delete(file.path(MOD2_PATH, "1.ext"))

  # try to run and expect error with NOT_FINISHED_ERR_MSG
  expect_error(model_summary(mod2), regexp = NOT_FINISHED_ERR_MSG)

  # cleanup
  fs::dir_delete(MOD2_PATH)
  fs::file_delete(ctl_ext(MOD2_PATH))
  fs::file_delete(yaml_ext(MOD2_PATH))
})

test_that("model_summary() fails predictably if no .lst file present", {
  # create new model
  mod2 <- MOD1 %>% copy_model_from(MOD2_PATH, .description = "number 2")

  # copy output directory (to simulate model run)
  fs::dir_copy(MOD1_PATH, MOD2_PATH)

  # delete a necessary file
  fs::file_delete(file.path(MOD2_PATH, "1.lst"))

  # try to run and expect error with NOT_FINISHED_ERR_MSG
  expect_error(model_summary(mod2), regexp = NO_LST_ERR_MSG)

  # cleanup
  fs::dir_delete(MOD2_PATH)
  fs::file_delete(ctl_ext(MOD2_PATH))
  fs::file_delete(yaml_ext(MOD2_PATH))
})


#########################################
# extracting things from summary object
#########################################

test_that("param_estimates() gets expected table", {
  # get summary
  sum1 <- MOD1 %>% model_summary()

  # extract parameter df
  par_df1 <- param_estimates(sum1)

  # compare to reference
  ref_df <- readRDS("data/acop_param_table_ref_200228.rds")
  expect_equivalent(par_df1, ref_df)
})
