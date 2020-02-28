####################################################
# testing a composable workflow
# checking that paths and return calls are correct
####################################################

# reference constants
REF_SUMMARY_CALL <- as.character(glue("cd {getwd()}/model-examples/1 ; {getOption('rbabylon.bbi_exe_path')} nonmem summary 1 --json"))

.TEST_CASES_WD <- list(
  list(test_wd = ".",               test_yaml_path = "model-examples/1.yaml"),
  list(test_wd = "..",              test_yaml_path = "testthat/model-examples/1.yaml"),
  list(test_wd = "../..",           test_yaml_path = "tests/testthat/model-examples/1.yaml"),
  list(test_wd = "model-examples",  test_yaml_path = "1.yaml"),
  list(test_wd = ".",               test_yaml_path = "model-examples/1.yaml", change_midstream = TRUE),
  list(test_wd = "..",              test_yaml_path = "testthat/model-examples/1.yaml", change_midstream = TRUE),
  list(test_wd = "../..",           test_yaml_path = "tests/testthat/model-examples/1.yaml", change_midstream = TRUE),
  list(test_wd = "model-examples",  test_yaml_path = "1.yaml", change_midstream = TRUE)
)


for (.test_case in .TEST_CASES_WD) {
  test_that(paste("basic workflow is correct from different working directories", .test_case$test_wd, if(isTRUE(.test_case$change_midstream)) "change_midstream"), {
    withr::with_dir(.test_case$test_wd, {
      test_yaml_path <- .test_case$test_yaml_path

      # load spec from yaml
      this_spec <- create_model_from_yaml(.yaml_path = test_yaml_path)

      # check class and keys are right
      expect_identical(class(this_spec), c("bbi_nonmem_spec", "list"))
      expect_true(all(SPEC_REQ_KEYS %in% names(this_spec)))

      # check the the model path parses correctly
      this_mod_path <- file.path(this_spec[[WORKING_DIR]], this_spec[[YAML_MOD_PATH]])
      expect_true(fs::file_exists(this_mod_path))

      # dry run of model submission
      if (isTRUE(.test_case$change_midstream)) {
        this_res <- withr::with_dir("..", { submit_model(this_spec, .dry_run = TRUE) })
      } else {
        this_res <- submit_model(this_spec, .dry_run = TRUE)
      }


      # check class and keys are right
      expect_identical(class(this_res), c("bbi_nonmem_result", "bbi_nonmem_spec", "list"))
      expect_true(all(RESULT_REQ_KEYS %in% names(this_res)))

      # check the the model path parses correctly
      this_mod_path <- file.path(this_res[[WORKING_DIR]], this_res[[YAML_MOD_PATH]])
      expect_true(fs::file_exists(this_mod_path))

      # check the call looks right
      call_str <- unlist(stringr::str_split(this_res$call, " --"))[1]
      res_str <- as.character(glue::glue("cd {this_res[[WORKING_DIR]]} ; {getOption('rbabylon.bbi_exe_path')} nonmem run sge {this_res[[YAML_MOD_PATH]]}"))
      expect_identical(call_str, res_str)

      # look for outputs
      expect_identical(this_res[[YAML_OUT_DIR]], get_mod_id(test_yaml_path))
      expect_true(fs::file_exists(build_path_from_res(this_res, "lst")))
      expect_true(fs::file_exists(build_path_from_res(this_res, "ext")))
      expect_true(fs::file_exists(build_path_from_res(this_res, "grd")))

      # dry run of summary call
      if (isTRUE(.test_case$change_midstream)) {
        this_sum <- withr::with_dir("..", { model_summary(this_res, .dry_run = TRUE) })
      } else {
        this_sum <- model_summary(this_res, .dry_run = TRUE)
      }

      # check the call looks right
      expect_identical(this_sum$call, REF_SUMMARY_CALL)

    })
  })
}


for (.test_case in .TEST_CASES_WD) {
  test_that(paste("Summary call is the same after changing directories", .test_case$test_wd, if(isTRUE(.test_case$change_midstream)) "change_midstream"), {
    withr::with_dir(.test_case$test_wd, {
      test_yaml_path <- .test_case$test_yaml_path

      # load spec from yaml and dry run through to summary object
      this_spec <- create_model_from_yaml(.yaml_path = test_yaml_path)
      this_res <- submit_model(this_spec, .dry_run = TRUE)
      this_sum <- model_summary(this_res, .dry_run = TRUE)
      expect_identical(this_sum$call, REF_SUMMARY_CALL)

      # check that summary call matches when generated with file path instead of object
      if (isTRUE(.test_case$change_midstream)) {
        .former_dir <- basename(getwd())
        withr::with_dir("..", {
          this_out_dir <- tools::file_path_sans_ext(file.path(.former_dir, test_yaml_path))
          this_out_dir_sum <- model_summary(this_out_dir, .model_type = "nonmem", .dry_run = TRUE)

          this_ctl_file <- ctl_ext(file.path(.former_dir, test_yaml_path))
          this_ctl_file_sum <- model_summary(this_ctl_file, .model_type = "nonmem", .dry_run = TRUE)
        })
      } else {
        this_out_dir <- tools::file_path_sans_ext(test_yaml_path)
        this_out_dir_sum <- model_summary(this_out_dir, .model_type = "nonmem", .dry_run = TRUE)

        this_ctl_file <- ctl_ext(test_yaml_path)
        this_ctl_file_sum <- model_summary(this_ctl_file, .model_type = "nonmem", .dry_run = TRUE)
      }
      expect_identical(this_out_dir_sum$call, REF_SUMMARY_CALL)
      expect_identical(this_ctl_file_sum$call, REF_SUMMARY_CALL)

    })
  })
}

