context("Test creating summary logs")

skip_if_not_ci_or_metworx("test-summary-log")

withr::local_options(list(bbr.bbi_exe_path = read_bbi_path()))

# helper to run expectations
test_sum_df <- function(sum_df, .paths, .col_count) {
  # check sum_df class
  expect_true(inherits(sum_df, SUM_LOG_CLASS))
  expect_true(inherits(sum_df, LOG_DF_CLASS))

  num_mods <- length(.paths)
  expect_equal(nrow(sum_df), num_mods)
  expect_equal(ncol(sum_df), .col_count)

  # check some columns
  expect_equal(sum_df$absolute_model_path, purrr::map_chr(.paths, ~normalizePath(.x)))
  expect_true(all(is.na(sum_df$error_msg)))
  expect_false(any(sum_df$needed_fail_flags))
  expect_equal(sum_df$ofv, rep(MOD1_OFV_REF, num_mods))
  expect_equal(sum_df$param_count, rep(MOD1_PARAM_COUNT_FIXED, num_mods))
  expect_false(any(sum_df$minimization_terminated))
}

test_that("summary_log() returns NULL and warns when no YAML found [BBR-SMLG-001]", {
  clean_test_enviroment(create_all_models)
  copy_all_output_dirs()
  log_df <- expect_warning(summary_log(file.path(REF_DIR, "read-output-refs")), regexp = "Found no valid model YAML files in")
  expect_true(inherits(log_df, "tbl"))
  expect_equal(nrow(log_df), 0)
  expect_equal(ncol(log_df), 0)
})

#########################################
# extracting things from summary object
#########################################

test_that("summary_log() works correctly with nested dirs [BBR-SMLG-002]", {
  clean_test_enviroment(create_all_models)
  copy_all_output_dirs()
  sum_df <- summary_log(MODEL_DIR, .recurse = TRUE)
  test_sum_df(sum_df, c(MOD1_PATH, NEW_MOD2, NEW_MOD3, LEVEL2_MOD), SUM_LOG_COLS)
})

test_that("summary_log() defaults to .recurse = FALSE [BBR-SMLG-003]", {
  clean_test_enviroment(create_all_models)
  copy_all_output_dirs()
  sum_df <- summary_log(MODEL_DIR)
  test_sum_df(sum_df, c(MOD1_PATH, NEW_MOD2, NEW_MOD3), SUM_LOG_COLS)
})


test_that("add_summary() works correctly [BBR-SMLG-004]", {
  clean_test_enviroment(create_all_models)
  copy_all_output_dirs()
  sum_df <- run_log(MODEL_DIR, .recurse = TRUE) %>% add_summary()
  # Subtract 2 columns that overlap (between run_log and add_summary): ABS_MOD_PATH and "run"
  test_sum_df(sum_df, c(MOD1_PATH, NEW_MOD2, NEW_MOD3, LEVEL2_MOD), RUN_LOG_COLS+SUM_LOG_COLS-2)
  expect_identical(sum_df$model_type, rep("nonmem", RUN_LOG_ROWS+1))
  expect_identical(sum_df$yaml_md5, ALL_MODS_YAML_MD5)
})

test_that("add_summary() has correct columns [BBR-SMLG-005]", {
  clean_test_enviroment(create_all_models)
  copy_all_output_dirs()
  sum_df <- summary_log(MODEL_DIR)
  log_df <- run_log(MODEL_DIR)
  add_df <- log_df %>% add_summary()

  # should have all columns from both (minus the join key)
  expect_identical(names(add_df), c(names(log_df), names(sum_df)[3:length(names(sum_df))]))

  # check one col to make sure it matches
  col_to_check <- "bbi_summary"
  expect_identical(sum_df[[col_to_check]], add_df[[col_to_check]])
})

test_that("summary_log() parses heuristics correctly [BBR-SMLG-006]", {
  clean_test_enviroment(create_all_models)
  copy_all_output_dirs()
  sum_df2 <- summary_log(MODEL_DIR_X, .fail_flags = list(ext_file = "1001.1.TXT"))

  run1001 <- filter(sum_df2, run == "1001")
  expect_false(run1001$minimization_terminated)
  expect_true(run1001$large_condition_number)
  expect_true(run1001$prderr)
  expect_true(run1001[[ANY_HEURISTICS]])
  expect_true(filter(sum_df2, run == "iovmm")$has_final_zero_gradient)
  skip_if_old_bbi("3.0.3")
  expect_true(filter(sum_df2, run == "acop-fake-bayes")$eigenvalue_issues)
})

test_that("summary_log() parses more complex flags and stats [BBR-SMLG-007]", {
  clean_test_enviroment(create_all_models)
  copy_all_output_dirs()
  sum_df2 <- summary_log(MODEL_DIR_X, .fail_flags = list(ext_file = "1001.1.TXT"))

  # check fail flag parsed
  expect_equal(sum_df2[[SL_FAIL_FLAGS]],
               c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE))

  # check multiple estimation methods
  num_est_methods <- map_int(sum_df2[["estimation_method"]], length)
  expect_equal(num_est_methods, c(1, 3, 1, 0, 2, 1))
  expect_equal(sum_df2[[OFV_COL]],
               c(3842.571, 2675.293, 44158.939, NA_real_, -10838.582, 14722.149),
               tolerance = 0.01)
  expect_equal(sum_df2[[SUMMARY_COND_NUM]],
               c(4352.941, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
               tolerance = 0.01)
})

test_that("summary_log() doesn't fail on missing model output directory [BBR-SMLG-012]", {
  ctl_file <- fs::path_abs(CTL_TEST_FILE)
  withr::with_tempdir({
    fs::file_copy(ctl_file, "mod.ctl")
    new_model("mod")
    expect_warning(res <- summary_log("."), "FAILED")
    expect_identical(nrow(res), 1L)
    expect_identical(res$run, "mod")
    checkmate::expect_string(res$error_msg, min.chars = 1)
  })
})

test_that("summary_log works some failed summaries [BBR-SMLG-008]", {
  clean_test_enviroment(create_all_models)
  copy_all_output_dirs()
  fs::file_delete(file.path(LEVEL2_MOD, "1.grd"))

  sum_df <- summary_log(MODEL_DIR, .recurse = TRUE)
  expect_equal(is.na(sum_df$error_msg), c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(ncol(sum_df), SUM_LOG_COLS)
  expect_equal(sum_df[[SL_FAIL_FLAGS]], c(FALSE, FALSE, FALSE, FALSE))

  sum_df <- summary_log(MODEL_DIR, .recurse = TRUE, .bbi_args = list(no_grd_file = TRUE))
  test_sum_df(sum_df, c(MOD1_PATH, NEW_MOD2, NEW_MOD3, LEVEL2_MOD), SUM_LOG_COLS)

  # Add a failure that isn't covered by .bbi_args...
  fs::file_delete(file.path(NEW_MOD2, "2.ext"))
  sum_df <- summary_log(MODEL_DIR, .recurse = TRUE, .bbi_args = list(no_grd_file = TRUE))
  expect_equal(is.na(sum_df$error_msg), c(TRUE, FALSE, TRUE, TRUE))
  expect_equal(ncol(sum_df), SUM_LOG_COLS)
  expect_equal(sum_df[[SL_FAIL_FLAGS]], c(FALSE, FALSE, FALSE, FALSE))
  # ... or .fail_flags
  sum_df <- summary_log(MODEL_DIR, .recurse = TRUE, .fail_flags = list(no_grd_file = TRUE))
  expect_equal(is.na(sum_df$error_msg), c(TRUE, FALSE, TRUE, TRUE))
  expect_equal(ncol(sum_df), SUM_LOG_COLS)
  expect_equal(sum_df[[SL_FAIL_FLAGS]], c(FALSE, FALSE, FALSE, TRUE))
})

test_that("summary_log works all failed summaries [BBR-SMLG-009]", {
  clean_test_enviroment(create_all_models)
  copy_all_output_dirs()
  fs::file_delete(file.path(LEVEL2_MOD, "1.grd"))

  expect_warning({
    sum_df <- summary_log(LEVEL2_DIR)
  }, regexp = "ALL 1 MODEL SUMMARIES FAILED")

  expect_equal(nrow(sum_df), 1)
  expect_equal(names(sum_df), c(ABS_MOD_PATH, RUN_ID_COL, "error_msg"))
  expect_true(all(grepl("--no-grd-file", sum_df$error_msg)))

})

test_that("add_summary works all failed summaries [BBR-SMLG-010]", {
  clean_test_enviroment(create_all_models)
  copy_all_output_dirs()
  fs::file_delete(file.path(LEVEL2_MOD, "1.grd"))

  expect_warning({
    sum_df <- run_log(LEVEL2_DIR) %>% add_summary()
  }, regexp = "ALL 1 MODEL SUMMARIES FAILED")

  expect_equal(nrow(sum_df), 1)
  expect_true(all(c(ABS_MOD_PATH, RUN_ID_COL, YAML_TAGS, "error_msg") %in% names(sum_df)))
  expect_true(all(grepl("--no-grd-file", sum_df$error_msg)))

})

# ##########################################
# # Testing Additional Parameters Passed
# ##########################################

test_that("summary_log() works with filtering parameter numeric [BBR-SMLG-011]",
          {
            clean_test_enviroment(create_rlg_models)
            copy_all_output_dirs()
            log_df <- list(df = summary_log(MODEL_DIR), length = summary_log(MODEL_DIR) %>% nrow())

            expect_equal(summary_log(MODEL_DIR, .include = 1:(log_df$length - 1)) %>% nrow(), 2)
            expect_equal(summary_log(MODEL_DIR, .include = 1:(log_df$length - 2)) %>% nrow(), 1)
            expect_equal(summary_log(MODEL_DIR, .include = (log_df$length - 1):1) %>% nrow(), 2)
          })

test_that("summary_log() works with filtering parameter string [BBR-SMLG-011]",
          {
            setup_this_test <- function() {
              create_rlg_models()
              copy_model_from(MOD1, "Child")
              copy_model_from(MOD1, "Parent")
              purrr::walk(
                c(2, 3, "Child", "Parent"),
                ~fs::dir_copy(file.path(MODEL_DIR,"1"), file.path(MODEL_DIR,"{.x}" %>% glue()))
              )
            }


            clean_test_enviroment(setup_this_test)


            log_df <- list(df = summary_log(MODEL_DIR), length = summary_log(MODEL_DIR) %>% nrow())

            expect_equal(summary_log(MODEL_DIR, .include = c(1:2, "Child")) %>% nrow(), 3)
            expect_equal(summary_log(MODEL_DIR, .include = c(2:1, "Child")) %>% nrow(), 3)
            expect_equal(summary_log(MODEL_DIR, .include = c("Child", 1, 2, 3)) %>% nrow(), 4)
            expect_equal(summary_log(MODEL_DIR, .include = c(1:2, "Parent")) %>% nrow(), 3)

          })

test_that("summary_log/add_summary: AIC/BIC", {
  tdir <- local_model_dir_nonmem_dummies(
    list(
      list(
        subpath = "1",
        number_of_obs = 800,
        ofv_no_constant = 1.0,
        ofv_with_constant = 10.0
      ),
      list(
        subpath = "2",
        based_on = "1",
        final_est_method = "First Order: BAYESIAN-OPTIMALITY"
      ),
      list(
        subpath = "3",
        based_on = "1",
        number_of_obs = 900,
        final_est_method = "First Order Conditional Estimation with Interaction (No Prior) (Evaluation)",
        ofv_no_constant = 3.0,
        ofv_with_constant = 30.0
      ),
      list(
        subpath = "4",
        based_on = "2",
        final_est_method = "Importance Sampling of Variance-Covariance (SIR)"
      ),
      list(
        subpath = "5",
        based_on = "4",
        final_est_method = "Stochastic Approximation Expectation-Maximization"
      ),
      list(
        subpath = "6",
        based_on = "4",
        number_of_obs = 800,
        final_est_method = "Laplacian Conditional Estimation (Centered)",
        ofv_no_constant = 6.0,
        ofv_with_constant = 60.0
      ),
      list(
        subpath = "7",
        based_on = "4",
        number_of_obs = 800,
        final_est_method = "Importance Sampling assisted by MAP Estimation",
        ofv_no_constant = 7.0,
        ofv_with_constant = 70.0
      ),
      list(
        subpath = "8",
        based_on = "4",
        number_of_obs = 800,
        final_est_method = "Iterative Two Stage (No Prior)",
        ofv_no_constant = 8.0,
        ofv_with_constant = 80.0
      ),
      list(
        subpath = "9",
        based_on = "4",
        number_of_obs = 800,
        final_est_method = "Objective Function Evaluation by Importance/MAP Sampling",
        ofv_no_constant = 9.0,
        ofv_with_constant = 90.0
      )
    )
  )

  nparams <- 7 # from inst/model/nonmem/basic/1
  nobs <- c(800, NA, 900, NA, NA, 800, 800, 800, 800)
  # The objective function value with constant is taken as -2 loglikelihood.
  loglik <- c(10.0, NA, 30.0, NA, NA, 60.0, 70.0, 80.0, 90.0) / -2

  res_sl <- summary_log(tdir)
  # Sort so that these tests don't assume summary_log's output order.
  res_sl <- dplyr::arrange(res_sl, .data[[RUN_ID_COL]])

  expect_equal(
    res_sl[[AIC_COL]],
    2 * nparams - 2 * loglik
  )
  expect_equal(
    res_sl[[BIC_COL]],
    nparams * log(nobs) - 2 * loglik
  )

  res_as <- add_summary(run_log(tdir))
  expect_equal(
    res_sl[[AIC_COL]],
    res_as[[AIC_COL]]
  )
  expect_equal(
    res_sl[[BIC_COL]],
    res_as[[BIC_COL]]
  )
})

test_that("allow_ofv_calc selection", {
  # This set of #METH strings covers many cases but is non-exhaustive.
  allowed <- c(
    "Chain Method Processing" = FALSE,
    "First Order (Evaluation)" = TRUE,
    "First Order (Evaluation): D-OPTIMALITY" = FALSE,
    "First Order (No Prior): D-OPTIMALITY" = FALSE,
    "First Order (No Prior): ED-OPTIMALITY" = FALSE,
    "First Order Conditional Estimation (Centered)" = TRUE,
    "First Order Conditional Estimation (Hybrid)" = TRUE,
    "First Order Conditional Estimation with Interaction (No Prior) (Evaluation)" = TRUE,
    "First Order Conditional Estimation with Interaction (No Prior)" = TRUE,
    "First Order Conditional Estimation with Interaction" = TRUE,
    "First Order Conditional Estimation with Interaction: D-OPTIMALITY" = FALSE,
    "First Order Conditional Estimation" = TRUE,
    "First Order with Interaction: D-OPTIMALITY" = FALSE,
    "First Order" = TRUE,
    "First Order: A-OPTIMALITY" = FALSE,
    "First Order: BAYESIAN-OPTIMALITY" = FALSE,
    "First Order: D-OPTIMALITY DIM SCALED" = FALSE,
    "First Order: D-OPTIMALITY" = FALSE,
    "First Order: DS-OPTIMALITY" = FALSE,
    "Importance Sampling (No Prior)" = TRUE,
    "Importance Sampling assisted by MAP Estimation" = TRUE,
    "Importance Sampling of Variance-Covariance (SIR)" = FALSE,
    "Importance Sampling" = TRUE,
    "Iterative Two Stage (No Prior)" = TRUE,
    "Iterative Two Stage" = TRUE,
    "Laplacian Conditional Estimation (Centered)" = TRUE,
    "Laplacian Conditional Estimation (Evaluation)" = TRUE,
    "Laplacian Conditional Estimation with Interaction (Evaluation)" = TRUE,
    "Laplacian Conditional Estimation with Interaction (No Prior)" = TRUE,
    "Laplacian Conditional Estimation with Interaction" = TRUE,
    "Laplacian Conditional Estimation" = TRUE,
    "MCMC Bayesian Analysis" = FALSE,
    "NUTS Bayesian Analysis" = FALSE,
    "Objective Function Evaluation by Importance Sampling (No Prior)" = TRUE,
    "Objective Function Evaluation by Importance Sampling" = TRUE,
    "Objective Function Evaluation by Importance/MAP Sampling" = TRUE,
    "Objective Function Evaluation DIRECT Sampling (No Prior)" = FALSE,
    "Objective Function Evaluation DIRECT Sampling" = FALSE,
    "Stochastic Approximation Expectation-Maximization (Expectation Only) (No Prior)" = FALSE,
    "Stochastic Approximation Expectation-Maximization (No Prior)" = FALSE,
    "Stochastic Approximation Expectation-Maximization" = FALSE
  )

  expect_identical(
    allow_ofv_calc(names(allowed)),
    unname(allowed)
  )
})
