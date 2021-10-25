context("Test bbi batch parameter estimate functions")

skip_if_not_drone_or_metworx("test-batch-param-estimates")

withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {

  #########################################
  # creating parameter summary tibble
  #########################################

  test_that("param_estimates_batch produces expected output", {

    on.exit(cleanup())

    # copy .ext file two times (to simulate two model runs)
    copy_to_batch_params(MOD1_PATH, "4")
    copy_to_batch_params(MOD1_PATH, "5")

    # get table of parameter estimates
    param_tbl <- BATCH_PARAM_TEST_DIR %>% param_estimates_batch()

    # table should have two rows (for two model runs detected)
    expect_equal(nrow(param_tbl), 2)

    # table should contain absolute_model_path, error_msg, and termination_code columns
    expect_true("absolute_model_path" %in% names(param_tbl))
    expect_true("error_msg" %in% names(param_tbl))
    expect_true("termination_code" %in% names(param_tbl))

    # error_msg column should be NA for both model runs
    expect_identical(is.na(param_tbl$error_msg), c(TRUE, TRUE))

    # because these files are identical all param names detected should have values
    without_error <- param_tbl %>% select(-c("absolute_model_path", "error_msg", "termination_code"))
    expect_false(any(is.na(without_error)))

  })

  #####################
  # different number of params
  #####################

  test_that("param_estimates_batch() works with varying number of param estimates", {

    on.exit(cleanup())

    # copy two different .ext file runs (to simulate two model runs with varying parameters)
    copy_to_batch_params(MOD1_PATH, "4")
    copy_to_batch_params(file.path(MODEL_DIR_X, "iovmm"), "6")

    # get table of parameter estimates
    param_tbl <- BATCH_PARAM_TEST_DIR %>% param_estimates_batch()

    # because these files do not have identical param names, some NAs should be present for values
    without_error <- param_tbl  %>% select(-c("absolute_model_path", "error_msg", "termination_code"))
    expect_true(any(is.na(without_error)))

    # error_msg column should be NA for both model runs
    expect_identical(is.na(param_tbl$error_msg), c(TRUE, TRUE))

  })

  #####################
  # empty extension file
  #####################

  test_that("param_estimates_batch() works if an .ext file detected is empty", {

    on.exit(cleanup())

    new_mod_id <- "7"
    copy_to_batch_params(MOD1_PATH, new_mod_id)

    # write an empty .ext file to path
    readr::write_file("", file.path(BATCH_PARAM_TEST_DIR, new_mod_id, paste0(new_mod_id, ".ext")))

    # create parameter table
    param_tbl <- BATCH_PARAM_TEST_DIR %>% param_estimates_batch()

    # only `absolute_model_path`, `error_msg`, and `termination_code` columns should be present
    expect_identical(names(param_tbl), c("absolute_model_path", "run", "error_msg", "termination_code"))

    # `error_msg` should not be `NA`
    expect_false(is.na(param_tbl$error_msg))

    # `error_msg` should describe empty contents in .ext file
    expect_true(grepl('no ext file contents', param_tbl$error_msg))

  })


  #####################
  # missing termination line
  #####################

  test_that("param_estimates_batch() works if the termination line is missing in an .ext file", {

    on.exit(cleanup())

    new_mod_id <- "8"
    copy_to_batch_params(MOD1_PATH, new_mod_id)

    # erase -1000000000 line
    new_ext_file <- file.path(BATCH_PARAM_TEST_DIR, new_mod_id, paste0(new_mod_id, ".ext"))
    new_ext_lines <- readr::read_lines(new_ext_file)
    new_ext_lines %>%
      stringr::str_subset(" *\\-1000000000", negate = TRUE) %>%
      readr::write_lines(new_ext_file)

    # create parameter table
    param_tbl <- BATCH_PARAM_TEST_DIR %>% param_estimates_batch()

    # only `absolute_model_path`, `error_msg`, and `termination_code` columns should be present
    expect_identical(names(param_tbl), c("absolute_model_path", "run", "error_msg", "termination_code"))

    # `error_msg` should not be `NA`
    expect_false(is.na(param_tbl$error_msg))

    # `error_msg` should describe that estimation output is missing
    expect_true(grepl('no estimation output detected', param_tbl$error_msg))
  })

}) # closing withr::with_options
