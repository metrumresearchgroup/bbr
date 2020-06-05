context("testing a composable workflow but only dryrun and NOT running bbi")

####################################################
# testing a composable workflow
# checking that paths and return calls are correct
####################################################

# reference constants
MODEL_DIR <- "model-examples"
TEST_YAML <- "1.yaml"

REF_SUMMARY_CALL <- as.character(glue("cd {getwd()}/model-examples/1 ; {getOption('rbabylon.bbi_exe_path')} nonmem summary 1 --json"))
MOD_CLASS <- c("bbi_nonmem_model", "list")
PROC_CLASS <- c("babylon_process", "list")


###############################################################
# with options("rbabylon.model_directory" = MODEL_DIR)
###############################################################

.TEST_CASES_WD <- list(
  list(test_wd = "."),
  list(test_wd = ".."),
  list(test_wd = MODEL_DIR),
  list(test_wd = ".",        change_midstream = TRUE),
  list(test_wd = "..",       change_midstream = TRUE),
  list(test_wd = MODEL_DIR,  change_midstream = TRUE)
)

withr::with_options(list(rbabylon.model_directory = normalizePath(MODEL_DIR)), {

  # create fake babylon.yaml
  bbi_yaml <- normalizePath(file.path(MODEL_DIR, "babylon.yaml"), mustWork = FALSE)
  readr::write_file("created_by: test-workflow-dryrun part 1", bbi_yaml)

  for (.test_case in .TEST_CASES_WD) {
    test_that(paste0("basic workflow is correct using rbabylon.model_directory = '", MODEL_DIR, "'", if(isTRUE(.test_case$change_midstream)) " change_midstream"), {
      withr::with_dir(.test_case$test_wd, {

        # load model from yaml
        this_mod <- read_model(.path = TEST_YAML)

        # check class and keys are right
        expect_identical(class(this_mod), MOD_CLASS)
        expect_true(all(MODEL_REQ_KEYS %in% names(this_mod)))

        # check the the model path parses correctly
        this_mod_path <- file.path(this_mod[[WORKING_DIR]], this_mod[[YAML_MOD_PATH]])
        expect_true(fs::file_exists(this_mod_path))

        # dry run of model submission
        if (isTRUE(.test_case$change_midstream)) {
          this_proc <- withr::with_dir("..", { submit_model(this_mod, .dry_run = TRUE) })
        } else {
          this_proc <- submit_model(this_mod, .dry_run = TRUE)
        }

        # check class and keys are right
        expect_identical(class(this_proc), PROC_CLASS)
        expect_true(all(PROCESS_REQ_KEYS %in% names(this_proc)))

        # check the call looks right
        call_str <- unlist(stringr::str_split(this_proc[[PROC_CALL]], " --"))[1]
        proc_str <- as.character(glue::glue("cd {this_proc[[PROC_WD]]} ; {getOption('rbabylon.bbi_exe_path')} nonmem run sge {this_proc[[PROC_CMD_ARGS]][4]}"))
        expect_identical(call_str, proc_str)

        # look for outputs
        expect_identical(this_mod[[YAML_OUT_DIR]], get_model_id(TEST_YAML))
        expect_true(fs::file_exists(build_path_from_mod_obj(this_mod, "lst")))
        expect_true(fs::file_exists(build_path_from_mod_obj(this_mod, "ext")))
        expect_true(fs::file_exists(build_path_from_mod_obj(this_mod, "grd")))

        # dry run of summary call
        if (isTRUE(.test_case$change_midstream)) {
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
    test_that(paste(glue::glue("summary call is the same using rbabylon.model_directory = '{MODEL_DIR}'"), .test_case$test_wd, if(isTRUE(.test_case$change_midstream)) "change_midstream"), {
      withr::with_dir(.test_case$test_wd, {

        # load mod from yaml and dry run through to summary object
        this_mod <- read_model(.path = TEST_YAML)
        #this_proc <- submit_model(this_mod, .dry_run = TRUE, .config_path = .test_case$bbi_path)
        this_proc <- submit_model(this_mod, .dry_run = TRUE)
        this_sum <- this_proc %>% as_model() %>% model_summary(.dry_run = TRUE)
        expect_identical(this_sum[[PROC_CALL]], REF_SUMMARY_CALL)

        # check that summary call matches when generated with file path instead of object
        this_out_dir <- tools::file_path_sans_ext(TEST_YAML)
        this_ctl_file <- ctl_ext(TEST_YAML)

        if (isTRUE(.test_case$change_midstream)) {
          withr::with_dir("..", {
            this_out_dir_sum <- model_summary(this_out_dir, .model_type = "nonmem", .dry_run = TRUE)
            this_ctl_file_sum <- model_summary(this_ctl_file, .model_type = "nonmem", .dry_run = TRUE)
          })
        } else {
          this_out_dir_sum <- model_summary(this_out_dir, .model_type = "nonmem", .dry_run = TRUE)
          this_ctl_file_sum <- model_summary(this_ctl_file, .model_type = "nonmem", .dry_run = TRUE)
        }
        expect_identical(this_out_dir_sum$call, REF_SUMMARY_CALL)
        expect_identical(this_ctl_file_sum$call, REF_SUMMARY_CALL)

      })
    })
  }

  fs::file_delete(bbi_yaml)

}) # closing withr::with_options



###################################################################################
# with options("rbabylon.model_directory" = NULL), changing the working directory
###################################################################################

.TEST_CASES_WD <- list(
  list(test_wd = ".",        bbi_path = "babylon.yaml",           test_yaml_path = file.path(MODEL_DIR, TEST_YAML)),
  list(test_wd = "..",       bbi_path = "testthat/babylon.yaml",  test_yaml_path = file.path("testthat", MODEL_DIR, TEST_YAML)),
  list(test_wd = MODEL_DIR,  bbi_path = "../babylon.yaml",        test_yaml_path = TEST_YAML),
  list(test_wd = ".",        bbi_path = "babylon.yaml",           test_yaml_path = file.path(MODEL_DIR, TEST_YAML),              change_midstream = "testthat/babylon.yaml"),
  list(test_wd = "..",       bbi_path = "testthat/babylon.yaml",  test_yaml_path = file.path("testthat", MODEL_DIR, TEST_YAML),  change_midstream = "tests/testthat/babylon.yaml"),
  list(test_wd = MODEL_DIR,  bbi_path = "../babylon.yaml",        test_yaml_path = TEST_YAML,                                    change_midstream = "babylon.yaml")
)

withr::with_options(list(rbabylon.model_directory = NULL), {

  # create fake babylon.yaml
  readr::write_file("created_by: test-workflow-dryrun part 2", "babylon.yaml")
  on.exit({ fs::file_delete("babylon.yaml") })

  for (.test_case in .TEST_CASES_WD) {
    test_that(paste("basic workflow is correct from different working directories", .test_case$test_wd, if(!is.null(.test_case$change_midstream)) "change_midstream"), {
      withr::with_dir(.test_case$test_wd, {
        test_yaml_path <- .test_case$test_yaml_path

        # load model from yaml
        this_mod <- read_model(.path = test_yaml_path)

        # check class and keys are right
        expect_identical(class(this_mod), MOD_CLASS)
        expect_true(all(MODEL_REQ_KEYS %in% names(this_mod)))

        # check the the model path parses correctly
        this_mod_path <- file.path(this_mod[[WORKING_DIR]], this_mod[[YAML_MOD_PATH]])
        expect_true(fs::file_exists(this_mod_path))

        # dry run of model submission
        if (!is.null(.test_case$change_midstream)) {
          this_proc <- withr::with_dir("..", { submit_model(this_mod, .dry_run = TRUE, .config_path = .test_case$change_midstream) })
        } else {
          this_proc <- submit_model(this_mod, .dry_run = TRUE, .config_path = .test_case$bbi_path)
        }


        # check class and keys are right
        expect_identical(class(this_proc), PROC_CLASS)
        expect_true(all(PROCESS_REQ_KEYS %in% names(this_proc)))

        # check the call looks right
        call_str <- unlist(stringr::str_split(this_proc[[PROC_CALL]], " --"))[1]
        proc_str <- as.character(glue::glue("cd {this_proc[[PROC_WD]]} ; {getOption('rbabylon.bbi_exe_path')} nonmem run sge {this_proc[[PROC_CMD_ARGS]][4]}"))
        expect_identical(call_str, proc_str)

        # look for outputs
        expect_identical(this_mod[[YAML_OUT_DIR]], get_model_id(test_yaml_path))
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
        test_yaml_path <- .test_case$test_yaml_path

        # load mod from yaml and dry run through to summary object
        this_mod <- read_model(.path = test_yaml_path)
        this_proc <- submit_model(this_mod, .dry_run = TRUE, .config_path = .test_case$bbi_path)
        this_sum <- this_proc %>% as_model() %>% model_summary(.dry_run = TRUE)
        expect_identical(this_sum[[PROC_CALL]], REF_SUMMARY_CALL)

        # check that summary call matches when generated with file path instead of object
        if (!is.null(.test_case$change_midstream)) {
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

}) # closing withr::with_options

