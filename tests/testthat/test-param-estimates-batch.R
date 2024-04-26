context("Test bbi batch parameter estimate functions")

skip_if_not_drone_or_metworx("test-batch-param-estimates")
skip_if_old_bbi("3.1.0")

withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {

  #########################################
  # creating parameter summary tibble
  #########################################

  test_that("param_estimates_batch produces expected output [BBR-PEST-005]", {

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

  test_that("param_estimates_batch() works with varying number of param estimates [BBR-PEST-006]", {

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

  test_that("param_estimates_batch() works if an .ext file detected is empty [BBR-PEST-007]", {

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

  test_that("param_estimates_batch() works if the termination line is missing in an .ext file [BBR-PEST-008]", {

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

  ###################
  # compare tests
  ###################

  test_that("param_estimates_compare() works: same model [BBR-PEST-009]", {

    on.exit(cleanup())

    # copy 6 .ext files (to simulate model runs)
    walk(4:9, ~copy_to_batch_params(MOD1_PATH, as.character(.x)))

    # get table of parameter estimates
    param_tbl <- BATCH_PARAM_TEST_DIR %>% param_estimates_batch()

    # multiply by arbitrary value to get confidence intervals for testing
    jitter <- seq(0.95, 1.05, 0.02)
    param_tbl <- param_tbl %>%
      mutate(across(5:ncol(param_tbl), ~(.x*jitter)))

    # get quantiles without orig model
    res <- param_estimates_compare(param_tbl)
    expect_equal(ncol(res), 4)
    expect_equal(nrow(res), nrow(param_estimates(SUM1)))

    # compare to orig model
    res <- param_estimates_compare(param_tbl, SUM1)
    expect_equal(ncol(res), 5)
    expect_equal(nrow(res), nrow(param_estimates(SUM1)))

    # quantiles should be less or more than median, respectively
    expect_true(all(
      (res$p2.5 < res$p50) | res$p50 == 0
    ))

    expect_true(all(
      (res$p97.5 > res$p50) | res$p50 == 0
    ))

    # note: this is only true because of the jitter we specify
    expect_equal(
      res$original_estimate,
      res$p50
    )
  })


  test_that("param_estimates_compare() works: lots of params [BBR-PEST-009]", {

    on.exit(cleanup())

    # copy 6 .ext files (to simulate model runs)
    walk(4:9, ~copy_to_batch_params(file.path(MODEL_DIR_X, "acop-iov"), as.character(.x)))
    .s <- file.path(MODEL_DIR_X, "acop-iov") %>%
      read_model() %>%
      model_summary()

    # compare
    res <- param_estimates_compare(
      param_estimates_batch(BATCH_PARAM_TEST_DIR),
      .s
    )
    expect_equal(ncol(res), 5)
    expect_equal(nrow(res), nrow(param_estimates(.s)))

  })

  test_that("param_estimates_compare() works: .compare_cols argument [BBR-PEST-010]", {
    param_df <- param_estimates_batch(MODEL_DIR)

    orig_names <- names(param_df)
    names(param_df) <- stringr::str_replace(orig_names, "^THETA", "naw")

    res <- param_estimates_compare(
      param_df,
      .compare_cols = dplyr::matches("^naw")
    )

    expect_equal(
      nrow(res),
      sum(stringr::str_detect(orig_names, "^THETA"))
    )

    # test more complicated expression
    res <- param_estimates_compare(
      param_df,
      .compare_cols = !dplyr::matches("naw|_|run")
    )

    expect_equal(
      nrow(res),
      sum(stringr::str_detect(orig_names, "^(SIGMA|OMEGA)"))
    )
  })

  test_that("param_estimates_compare() works: probs argument [BBR-PEST-011]", {
    res <- param_estimates_compare(
      param_estimates_batch(MODEL_DIR),
      probs = c(.3, .4, .6)
    )
    expect_true(all(c("p30", "p40", "p60") %in% names(res)))
  })

  test_that("param_estimates_compare() works: na.rm argument [BBR-PEST-014]", {

    expect_error({
      res <- param_estimates_compare(
        param_estimates_batch(MODEL_DIR_X)
      )
    }, regexp = "missing values")

    res <- param_estimates_compare(
      param_estimates_batch(MODEL_DIR_X),
      na.rm = TRUE
    )
    expect_equal(ncol(res), 4)
    expect_true(nrow(res) > 10) # somewhat arbitrary, but just need to be sure it returns rows
  })

  test_that("param_estimates_compare() errors with different models [BBR-PEST-012]", {

    expect_error({
      res <- param_estimates_compare(
        param_estimates_batch(MODEL_DIR_X),
        SUM1
      )
    }, regexp = "do not have the same parameters")

  })

}) # closing withr::with_options

