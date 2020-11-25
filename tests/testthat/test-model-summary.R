context("Test bbi summary functions")

if (Sys.getenv("METWORX_VERSION") == "" && Sys.getenv("DRONE") != "true") {
  skip("test-summary only runs on Metworx or Drone")
}

withr::with_options(list(rbabylon.bbi_exe_path = read_bbi_path()), {

  #########################################
  # extracting things from summary object
  #########################################

  test_that("model_summary.bbi_nonmem_model produces expected output", {

    # get summary
    sum1 <- MOD1 %>% model_summary()

    # check class
    expect_identical(class(sum1), SUM_CLASS_LIST)

    # compare to reference
    ref_sum <- dget(SUMMARY_REF_FILE)
    expect_equal(ref_sum, sum1)
  })

  #####################
  # passing file flags
  #####################

  test_that("model_summary() works with custom .ext file", {
    on.exit({
      fs::dir_delete(NEW_MOD2)
      fs::file_delete(ctl_ext(NEW_MOD2))
      fs::file_delete(yaml_ext(NEW_MOD2))
    })

    # create new model
    mod2 <- MOD1 %>% copy_model_from(basename(NEW_MOD2))

    # copy output directory (to simulate model run)
    fs::dir_copy(MOD1_PATH, NEW_MOD2)

    # move .ext file
    fs::file_move(file.path(NEW_MOD2, "1.ext"), file.path(NEW_MOD2, "EXT"))

    # errors without the flag
    expect_error(model_summary(mod2), "No file present at.*2/1\\.ext")

    # works correctly with ext_file flag added
    sum2 <- model_summary(mod2, .bbi_args = list(ext_file = "EXT"))

    # some things will be a little different, most will be the same
    ref_sum <- dget(SUMMARY_REF_FILE)

    for (.d in names(ref_sum$run_details)) {
      if (.d == "output_files_used") {
        expect_false(all(sum2$run_details$output_files_used == ref_sum$run_details$output_files_used))
        expect_true(length(sum2$run_details$output_files_used) == length(ref_sum$run_details$output_files_used))
      } else {
        expect_equal(sum2$run_details[[.d]], ref_sum$run_details[[.d]])
      }
    }

    for (.n in names(ref_sum)) {
      if (.n != "run_details") {
        expect_equal(sum2[[.n]], ref_sum[[.n]])
      }
    }

  })


  TEST_CASES <- list(
    list(ext = "ext", missing = NULL),
    list(ext = "grd", missing = NULL),
    list(ext = "shk", missing = "shrinkage_details")
  )
  for (.tc in TEST_CASES) {
    test_that(glue::glue("model_summary() works with no .{.tc$ext} file"), {
      on.exit({
        fs::dir_delete(NEW_MOD2)
        fs::file_delete(ctl_ext(NEW_MOD2))
        fs::file_delete(yaml_ext(NEW_MOD2))
      })

      # create new model
      mod2 <- MOD1 %>% copy_model_from(basename(NEW_MOD2))

      # copy output directory (to simulate model run)
      fs::dir_copy(MOD1_PATH, NEW_MOD2)

      fs::file_delete(file.path(NEW_MOD2, paste0("1.", .tc$ext)))

      # errors without the flag
      expect_error(model_summary(mod2), glue::glue("No file present at.*2/1\\.{.tc$ext}"))

      # works correctly with flag added
      args_list <- list()
      args_list[[as.character(glue::glue("no_{.tc$ext}_file"))]] <- TRUE
      sum2 <- model_summary(mod2, .bbi_args = args_list)

      # some things will be a little different, most will be the same
      ref_sum <- dget(SUMMARY_REF_FILE)

      expect_equal(length(ref_sum), length(sum2) + length(.tc$missing))

      for (.d in names(ref_sum$run_details)) {
        if (.d != "output_files_used") {
          expect_equal(sum2$run_details[[.d]], ref_sum$run_details[[.d]])
        }
      }

      if (is.null(.tc$missing)) .tc$missing <- "NAAAAAAAAH" # if nothing is missing, set to fake key so `.n != .tc$missing` works
      for (.n in names(ref_sum)) {

        if (.tc$ext == "ext" && .n == "parameters_data") {
          # special test for .ext file because significant digits are different
          expect_equal(
            sum2[["parameters_data"]][[1]][["estimates"]],
            ref_sum[["parameters_data"]][[1]][["estimates"]],
            tolerance = 0.01
          )
        } else if (.n != "run_details" && .n != .tc$missing) {
          expect_equal(sum2[[.n]], ref_sum[[.n]])
        }
      }
    })
  }


  #######################
  # errors when expected
  #######################

  test_that("model_summary() fails predictably if it can't find some parts (i.e. model isn't finished)", {
    on.exit({
      fs::dir_delete(NEW_MOD2)
      fs::file_delete(ctl_ext(NEW_MOD2))
      fs::file_delete(yaml_ext(NEW_MOD2))
      rm(mod2)
    })

    # create new model
    mod2 <- MOD1 %>% copy_model_from(basename(NEW_MOD2))

    # copy head of .lst file (to simulate partially done model run)
    fs::dir_create(NEW_MOD2)
    lst_head <- readr::read_lines(file.path(MOD1_PATH, "1.lst"), n_max = 20)
    readr::write_lines(lst_head, file.path(NEW_MOD2, "2.lst"))

    # try to run and expect error with NOT_FINISHED_ERR_MSG
    expect_error(model_summary(mod2), regexp = NOT_FINISHED_ERR_MSG)
  })

  test_that("model_summary() fails predictably if no .lst file present", {
    on.exit({
      fs::dir_delete(NEW_MOD2)
      fs::file_delete(ctl_ext(NEW_MOD2))
      fs::file_delete(yaml_ext(NEW_MOD2))
    })

    # create new model
    mod2 <- MOD1 %>% copy_model_from(basename(NEW_MOD2))

    # copy output directory (to simulate model run)
    fs::dir_copy(MOD1_PATH, NEW_MOD2)

    # delete a necessary file
    fs::file_delete(file.path(NEW_MOD2, "1.lst"))

    # try to run and expect error with NO_LST_ERR_MSG
    expect_error(model_summary(mod2), regexp = NO_LST_ERR_MSG)
  })

}) # closing withr::with_options
