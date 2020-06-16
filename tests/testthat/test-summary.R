context("Test bbi summary functions")

if (Sys.getenv("METWORX_VERSION") == "" && Sys.getenv("DRONE") != "true") {
  skip("test-summary only runs on Metworx or Drone")
}

withr::with_options(list(rbabylon.bbi_exe_path = '/data/apps/bbi',
                         rbabylon.model_directory = NULL), {

  # constants
  MODEL_FILE <- "1.ctl"
  MODEL_YAML <- yaml_ext(MODEL_FILE)
  MODEL_DIR <- "model-examples"
  MOD1_PATH <- file.path(MODEL_DIR, "1")
  MOD1 <- MOD1_PATH %>% read_model()
  MOD2_PATH <- file.path(MODEL_DIR, "2")

  # references
  SUMMARY_REF_FILE <- "data/acop_summary_obj_ref_200616.rds"
  SUM_CLASS_LIST <- c("bbi_nonmem_summary", "list")
  NOT_FINISHED_ERR_MSG <- "nonmem_summary.*modeling run has not finished"
  NO_LST_ERR_MSG <- "Unable to locate `.lst` file.*NONMEM output folder"


  #########################################
  # extracting things from summary object
  #########################################

  test_that("model_summary.bbi_nonmem_model produces expected output", {

    # get summary
    sum1 <- MOD1 %>% model_summary()

    # check class
    expect_identical(class(sum1), SUM_CLASS_LIST)

    # compare to reference
    ref_sum <- readRDS(SUMMARY_REF_FILE)
    expect_equal(ref_sum, sum1)
  })

  withr::with_options(list(rbabylon.model_directory = normalizePath(MODEL_DIR)), {
    test_that("model_summary.character produces expected output", {
      # get summary
      sum1 <- "1" %>% model_summary()

      # check class
      expect_identical(class(sum1), SUM_CLASS_LIST)

      # compare to reference
      ref_sum <- readRDS(SUMMARY_REF_FILE)
      expect_equal(ref_sum, sum1)
    })

    test_that("model_summary.numeric produces expected output", {
      # get summary
      sum1 <- 1 %>% model_summary()

      # check class
      expect_identical(class(sum1), SUM_CLASS_LIST)

      # compare to reference
      ref_sum <- readRDS(SUMMARY_REF_FILE)
      expect_equal(ref_sum, sum1)
    })
  })


  test_that("model_summary() works with custom .ext file", {
    on.exit({
      fs::dir_delete(MOD2_PATH)
      fs::file_delete(ctl_ext(MOD2_PATH))
      fs::file_delete(yaml_ext(MOD2_PATH))
    })

    # create new model
    mod2 <- MOD1 %>% copy_model_from(MOD2_PATH, .description = "number 2")

    # copy output directory (to simulate model run)
    fs::dir_copy(MOD1_PATH, MOD2_PATH)

    # move .ext file
    fs::file_move(file.path(MOD2_PATH, "1.ext"), file.path(MOD2_PATH, "EXT"))

    # INCOMPLETE COMES BACK WITH NO FLAG AND NO EXT. IS THIS RIGHT?????
    sum2a <- model_summary(mod2)
    expect_equal(length(sum2a), 4)
    expect_equal(length(sum2a$run_details), 0)
    expect_equal(length(sum2a$run_heuristics), 7)
    expect_equal(length(sum2a$parameter_names), 0)
    expect_equal(length(sum2a$ofv), 0)

    # works correctly with ext_file flag added
    sum2b <- model_summary(mod2, .bbi_args = list(ext_file = "EXT"))

    # some things will be a little different, most will be the same
    ref_sum <- readRDS(SUMMARY_REF_FILE)

    for (.d in names(sum2b$run_details)) {
      if (.d == "output_files_used") {
        expect_false(all(sum2b$run_details$output_files_used == ref_sum$run_details$output_files_used))
        expect_true(length(sum2b$run_details$output_files_used) == length(ref_sum$run_details$output_files_used))
      } else {
        expect_equal(sum2b$run_details[[.d]], ref_sum$run_details[[.d]])
      }
    }

    for (.n in names(sum2b)) {
      if (.n != "run_details") {
        expect_equal(sum2b[[.n]], ref_sum[[.n]])
      }
    }

  })


  test_that("model_summary() works with no .ext file", {
    on.exit({
      fs::dir_delete(MOD2_PATH)
      fs::file_delete(ctl_ext(MOD2_PATH))
      fs::file_delete(yaml_ext(MOD2_PATH))
    })

    # create new model
    mod2 <- MOD1 %>% copy_model_from(MOD2_PATH, .description = "number 2")

    # copy output directory (to simulate model run)
    fs::dir_copy(MOD1_PATH, MOD2_PATH)

    fs::file_delete(file.path(MOD2_PATH, "1.ext"))

    # INCOMPLETE COMES BACK WITH NO FLAG AND NO EXT. IS THIS RIGHT?????
    sum2a <- model_summary(mod2)
    expect_equal(length(sum2a), 4)
    expect_equal(length(sum2a$run_details), 0)
    expect_equal(length(sum2a$run_heuristics), 7)
    expect_equal(length(sum2a$parameter_names), 0)
    expect_equal(length(sum2a$ofv), 0)

    # works correctly with no_ext_file flag added
    sum2b <- model_summary(mod2, .bbi_args = list(no_ext_file = TRUE))

    # some things will be a little different, most will be the same
    ref_sum <- readRDS(SUMMARY_REF_FILE)

    expect_equal(
      sum2b[["parameters_data"]][[1]][["estimates"]],
      ref_sum[["parameters_data"]][[1]][["estimates"]],
      tolerance = 0.01
    )

    for (.d in names(sum2b$run_details)) {
      if (.d == "output_files_used") {
        expect_false(length(sum2b$run_details$output_files_used) == length(ref_sum$run_details$output_files_used))
      } else {
        expect_equal(sum2b$run_details[[.d]], ref_sum$run_details[[.d]])
      }
    }

    for (.n in names(sum2b)) {
      if (.n != "run_details" && .n != "parameters_data") {
        expect_equal(sum2b[[.n]], ref_sum[[.n]])
      }
    }
  })


  test_that("model_summary() fails predictably if it can't find some parts (i.e. model isn't finished)", {
    on.exit({
      fs::dir_delete(MOD2_PATH)
      fs::file_delete(ctl_ext(MOD2_PATH))
      fs::file_delete(yaml_ext(MOD2_PATH))
      rm(mod2)
    })

    # create new model
    mod2 <- MOD1 %>% copy_model_from(MOD2_PATH, .description = "number 2")

    # copy head of .lst file (to simulate partially done model run)
    fs::dir_create(MOD2_PATH)
    lst_head <- readr::read_lines(file.path(MOD1_PATH, "1.lst"), n_max = 20)
    readr::write_lines(lst_head, file.path(MOD2_PATH, "2.lst"))

    # try to run and expect error with NOT_FINISHED_ERR_MSG
    expect_error(model_summary(mod2), regexp = NOT_FINISHED_ERR_MSG)
  })

  test_that("model_summary() fails predictably if no .lst file present", {
    on.exit({
      fs::dir_delete(MOD2_PATH)
      fs::file_delete(ctl_ext(MOD2_PATH))
      fs::file_delete(yaml_ext(MOD2_PATH))
    })

    # create new model
    mod2 <- MOD1 %>% copy_model_from(MOD2_PATH, .description = "number 2")

    # copy output directory (to simulate model run)
    fs::dir_copy(MOD1_PATH, MOD2_PATH)

    # delete a necessary file
    fs::file_delete(file.path(MOD2_PATH, "1.lst"))

    # try to run and expect error with NO_LST_ERR_MSG
    expect_error(model_summary(mod2), regexp = NO_LST_ERR_MSG)
  })

}) # closing withr::with_options
