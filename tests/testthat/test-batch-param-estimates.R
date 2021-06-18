context("Test bbi batch parameter estimate functions")

skip_if_not_drone_or_metworx("test-batch-param-estimates")

withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {

  #########################################
  # creating parameter summary tibble
  #########################################

  test_that("batch_param_estimates produces expected output", {

    on.exit({
      fs::dir_delete(NEW_MOD4)
      fs::dir_delete(NEW_MOD5)
      fs::dir_delete(BATCH_PARAM_TEST_DIR)
    })

    dir.create(BATCH_PARAM_TEST_DIR)
    dir.create(NEW_MOD4)
    dir.create(NEW_MOD5)

    # copy .ext file two times (to simulate two model runs)
    fs::file_copy(file.path(MOD1_PATH, "1.ext"), NEW_MOD4)
    fs::file_move(file.path(NEW_MOD4, "1.ext"), file.path(NEW_MOD4, "4.ext"))

    fs::file_copy(file.path(MOD1_PATH, "1.ext"), NEW_MOD5)
    fs::file_move(file.path(NEW_MOD5, "1.ext"), file.path(NEW_MOD5, "5.ext"))

    # get table of parameter estimates
    param_tbl <- BATCH_PARAM_TEST_DIR %>% batch_param_estimates()

    # table should have two rows (for two model runs detected)
    expect_equal(nrow(param_tbl), 2)

    # table should contain absolute_model_path & error_msg columns
    expect_true("absolute_model_path" %in% names(param_tbl))
    expect_true("error_msg" %in% names(param_tbl))

    # error_msg column should be NA for both model runs
    expect_identical(is.na(param_tbl$error_msg), c(TRUE, TRUE))

    # because these files are identical all param names detected should have values
    without_error <- param_tbl %>% select(-"error_msg")
    expect_false(any(is.na(without_error)))

  })

  #####################
  # different number of params
  #####################

  test_that("batch_param_estimates() works with varying number of param estimates", {

    on.exit({
      fs::dir_delete(NEW_MOD4)
      fs::dir_delete(NEW_MOD6)
      fs::dir_delete(BATCH_PARAM_TEST_DIR)
    })

    dir.create(BATCH_PARAM_TEST_DIR)
    dir.create(NEW_MOD4)
    dir.create(NEW_MOD6)

    # copy two different .ext file runs (to simulate two model runs with varying parameters)
    fs::file_copy(file.path(MOD1_PATH, "1.ext"), NEW_MOD4)
    fs::file_move(file.path(NEW_MOD4, "1.ext"), file.path(NEW_MOD4, "4.ext"))

    fs::file_copy(file.path(MODEL_DIR_X, "iovmm", "iovmm.ext"), NEW_MOD6)
    fs::file_move(file.path(NEW_MOD6, "iovmm.ext"), file.path(NEW_MOD6, "6.ext"))


    # get table of parameter estimates
    param_tbl <- BATCH_PARAM_TEST_DIR %>% batch_param_estimates()


    # because these files do not have identical param names, some NAs should be present for values
    without_error <- param_tbl %>% select(-"error_msg")
    expect_true(any(is.na(without_error)))

    # error_msg column should be NA for both model runs
    expect_identical(is.na(param_tbl$error_msg), c(TRUE, TRUE))

  })

  #####################
  # empty extension file
  #####################

  test_that("batch_param_estimates() works if an .ext file detected is empty", {

    on.exit({
      fs::dir_delete(NEW_MOD7)
      fs::dir_delete(BATCH_PARAM_TEST_DIR)
    })

    dir.create(BATCH_PARAM_TEST_DIR)
    dir.create(NEW_MOD7)

    # write an empty .ext file to path
    readr::write_file("", file.path(NEW_MOD7, "7.ext"))

    # create parameter table
    param_tbl <- BATCH_PARAM_TEST_DIR %>% batch_param_estimates()

    # only `absolute_model_path` & `error_msg` columns should be present
    expect_identical(names(param_tbl), c("absolute_model_path", "error_msg"))

    # `error_msg` should not be `NA`
    expect_false(is.na(param_tbl$error_msg))

    # `error_msg` should describe empty contents in .ext file
    expect_true(grepl('no ext file contents', param_tbl$error_msg))

  })


  #####################
  # missing termination line
  #####################

  test_that("batch_param_estimates() works if the termination line is missing in an .ext file", {

    on.exit({
      fs::dir_delete(NEW_MOD8)
      fs::dir_delete(BATCH_PARAM_TEST_DIR)
    })

    dir.create(BATCH_PARAM_TEST_DIR)
    dir.create(NEW_MOD8)

    # write an .ext file that is missing line -1000000000
    readr::write_file("TABLE NO.     1: First Order Conditional Estimation with Interaction: Goal Function=MINIMUM VALUE OF OBJECTIVE FUNCTION: Problem=1 Subproblem=0 Superproblem1=0 Iteration1=0 Superproblem2=0 Iteration2=0
 ITERATION    THETA1       THETA2       THETA3       THETA4       THETA5       SIGMA(1,1)   OMEGA(1,1)   OMEGA(2,1)   OMEGA(2,2)   OBJ
            0  2.00000E+00  3.00000E+00  1.00000E+01  2.00000E-02  1.00000E+00  1.00000E+00  5.00000E-02  0.00000E+00  2.00000E-01    15294.002144233427
            5  4.24454E+00  2.01268E+01  1.84447E+01 -6.04580E-03  5.26539E+00  1.00000E+00  8.69929E-02  0.00000E+00  8.29972E-01    3740.5412530895987
           10  5.00791E+00  7.71104E+01  5.03064E+02 -1.32147E-01  6.25451E+00  1.00000E+00  7.08461E-03  0.00000E+00  1.59955E-01    3133.0637805488759
           15  2.40082E+00  5.37872E+01  3.82758E+02 -7.69871E-02  4.03637E+00  1.00000E+00  1.16655E-01  0.00000E+00  2.43800E-01    2648.7566483156047
           20  2.30528E+00  5.58065E+01  4.72255E+02 -8.05144E-02  4.12591E+00  1.00000E+00  9.09862E-02  0.00000E+00  2.06974E-01    2638.5029836424560
           25  2.31302E+00  5.44961E+01  4.65370E+02 -8.07920E-02  4.13612E+00  1.00000E+00  9.61335E-02  0.00000E+00  1.57375E-01    2636.8778706340345
           28  2.31034E+00  5.49596E+01  4.64659E+02 -8.05722E-02  4.13030E+00  1.00000E+00  9.64407E-02  0.00000E+00  1.53571E-01    2636.8457703290751
  -1000000001  8.61470E-02  3.32914E+00  2.96177E+01  5.55149E-02  1.35989E+00  1.00000E+10  2.00146E-02  1.00000E+10  2.67330E-02    0.0000000000000000
  -1000000004  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  1.00000E+00  3.10549E-01  0.00000E+00  3.91882E-01    0.0000000000000000
  -1000000005  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  1.00000E+10  3.22245E-02  1.00000E+10  3.41085E-02    0.0000000000000000
  -1000000006  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  1.00000E+00  0.00000E+00  1.00000E+00  0.00000E+00    0.0000000000000000
  -1000000007  0.00000E+00  3.70000E+01  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00    0.0000000000000000
  -1000000008 -3.39427E-03  1.00007E-04 -7.27007E-04 -1.46088E-01  2.43350E-03  0.00000E+00  4.84108E-02  0.00000E+00 -5.58021E-03    0.0000000000000000", file.path(NEW_MOD8, "8.ext"))

    # create parameter table
    param_tbl <- BATCH_PARAM_TEST_DIR %>% batch_param_estimates()

    # only `absolute_model_path` & `error_msg` columns should be present
    expect_identical(names(param_tbl), c("absolute_model_path", "error_msg"))

    # `error_msg` should not be `NA`
    expect_false(is.na(param_tbl$error_msg))

    # `error_msg` should describe that estimation output is missing
    expect_true(grepl('no estimation output detected', param_tbl$error_msg))
  })

}) # closing withr::with_options
