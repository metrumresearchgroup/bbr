context("testing a composable workflow but only dryrun and NOT running bbi")

####################################################
# testing a composable workflow
# checking that paths and return calls are correct
####################################################

# reference constants
REF_SUMMARY_CALL <- as.character(glue("cd {getwd()}/model-examples/1 ; {getOption('rbabylon.bbi_exe_path')} nonmem summary 1 --json"))

########################################
###   change the working directory   ###
########################################

# if we are running in covr, then the directory holding the tests will be
# `rbabylon-tests`, rather than `tests`, which is a consequence of
# tools::testInstalledPackage(); note that the conditional is the definition
# of covr::in_covr()
if (identical(Sys.getenv("R_COVR"), "true")) {
  test_dir <- "rbabylon-tests"
} else {
  test_dir <- "tests"
}

.TEST_CASES_WD <- list(
  list(test_wd = ".",        bbi_path = "babylon.yaml",           test_path = file.path(MODEL_DIR, MOD_ID)),
  list(test_wd = "..",       bbi_path = "testthat/babylon.yaml",  test_path = file.path("testthat", MODEL_DIR, MOD_ID)),
  list(test_wd = MODEL_DIR,  bbi_path = "../babylon.yaml",        test_path = MOD_ID),
  list(test_wd = ".",        bbi_path = "babylon.yaml",           test_path = file.path(MODEL_DIR, MOD_ID),              change_midstream = "testthat/babylon.yaml"),
  list(test_wd = "..",       bbi_path = "testthat/babylon.yaml",  test_path = file.path("testthat", MODEL_DIR, MOD_ID),  change_midstream = file.path(test_dir, "testthat/babylon.yaml")),
  list(test_wd = MODEL_DIR,  bbi_path = "../babylon.yaml",        test_path = MOD_ID,                                    change_midstream = "babylon.yaml")
)

# create fake babylon.yaml
readr::write_file("created_by: test-workflow-dryrun part 2", "babylon.yaml")
on.exit({ fs::file_delete("babylon.yaml") })

for (.test_case in .TEST_CASES_WD) {
  test_that(paste("basic workflow is correct from different working directories", .test_case$test_wd, if(!is.null(.test_case$change_midstream)) "change_midstream"), {
    withr::with_dir(.test_case$test_wd, {

      # load model from yaml
      this_mod <- read_model(.test_case[["test_path"]])

      # check class and keys are right
      expect_identical(class(this_mod), MOD_CLASS_LIST)
      expect_true(all(MODEL_REQ_KEYS %in% names(this_mod)))

      # check the the model path parses correctly
      this_mod_path <- get_model_path(this_mod)
      expect_true(fs::file_exists(this_mod_path))

      # dry run of model submission
      if (!is.null(.test_case$change_midstream)) {
        this_proc <- withr::with_dir("..", { submit_model(this_mod, .dry_run = TRUE, .config_path = .test_case$change_midstream) })
      } else {
        this_proc <- submit_model(this_mod, .dry_run = TRUE, .config_path = .test_case$bbi_path)
      }


      # check class and keys are right
      expect_identical(class(this_proc), PROC_CLASS_LIST)
      expect_true(all(PROCESS_REQ_KEYS %in% names(this_proc)))

      # check the call looks right
      call_str <- unlist(stringr::str_split(this_proc[[PROC_CALL]], " --"))[1]
      proc_str <- as.character(glue::glue("cd {this_proc[[PROC_WD]]} ; {getOption('rbabylon.bbi_exe_path')} nonmem run sge {this_proc[[PROC_CMD_ARGS]][4]}"))
      expect_identical(call_str, proc_str)

      # look for outputs
      expect_true(fs::file_exists(build_path_from_mod_obj(this_mod, "lst")))
      expect_true(fs::file_exists(build_path_from_mod_obj(this_mod, "ext")))
      expect_true(fs::file_exists(build_path_from_mod_obj(this_mod, "grd")))

      # dry run of summary call
      if (!is.null(.test_case$change_midstream)) {
        this_sum <- withr::with_dir("..", { model_summary(this_mod, .dry_run = TRUE) })
      } else {
        this_sum <- model_summary(this_mod, .dry_run = TRUE)
      }

      # check the call looks right
      expect_identical(this_sum$call, REF_SUMMARY_CALL)

    })
  })
}


for (.test_case in .TEST_CASES_WD) {
  test_that(paste("Summary call is the same after changing directories", .test_case$test_wd, if(!is.null(.test_case$change_midstream)) "change_midstream"), {
    withr::with_dir(.test_case$test_wd, {

      # load mod from yaml and dry run through to summary object
      this_mod <- read_model(.test_case[["test_path"]])
      this_proc <- submit_model(this_mod, .dry_run = TRUE, .config_path = .test_case$bbi_path)
      this_sum <- model_summary(this_mod, .dry_run = TRUE)
      expect_identical(this_sum[[PROC_CALL]], REF_SUMMARY_CALL)

      # check that summary call matches when generated with file path instead of object
      if (!is.null(.test_case$change_midstream)) {
        withr::with_dir("..", {
          sum_change_dir <- model_summary(this_mod, .model_type = "nonmem", .dry_run = TRUE)
          expect_identical(sum_change_dir$call, REF_SUMMARY_CALL)
        })
      }
    })
  })
}

