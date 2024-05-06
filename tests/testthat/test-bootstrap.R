context("testing bootstrap functionality and running bbi")

####################################################
# testing bootstrap submission and summary via a composable workflow
# This test file actually runs the bbi calls
# and so it must have working version of both bbi and NONMEM.
# Because of this, it is disabled unless on Metworx.
#
# Additionally, tests in this file rely on each other
# and therefore must be run in order.
####################################################

# can't run on Drone because there's no NONMEM
if (Sys.getenv("METWORX_VERSION") == "" || Sys.getenv("SKIP_BBI_TEST") == "true") {
  skip("test-bootstrap only runs on Metworx because it needs NONMEM installed")
}
skip_long_tests("skipping long-running bbi workflow tests")

# define constants
MODEL_DIR_BBI <- file.path(dirname(ABS_MODEL_DIR), "test-bootstrap")

# cleanup function
cleanup_bbi <- function(.recreate_dir = FALSE) {
  if (fs::dir_exists(MODEL_DIR_BBI)) fs::dir_delete(MODEL_DIR_BBI)
  if (isTRUE(.recreate_dir)) fs::dir_create(MODEL_DIR_BBI)
}
cleanup_bbi(.recreate_dir = TRUE)

# set options and run tests
withr::with_options(
  list(
    bbr.bbi_exe_path = read_bbi_path(),
    bbr.verbose = TRUE
  ), {

    # cleanup when done
    on.exit({
      Sys.sleep(3) # wait for some NONMEM mess to delete itself
      cleanup_bbi()
    })

    # clear old bbi.yaml
    if (fs::file_exists(file.path(MODEL_DIR_BBI, "bbi.yaml"))){
      fs::file_delete(file.path(MODEL_DIR_BBI, "bbi.yaml"))
    }

    # create new bbi.yaml
    bbi_init(
      MODEL_DIR_BBI,
      .nonmem_dir = Sys.getenv("BBR_TESTS_NONMEM_DIR", "/opt/NONMEM"),
      .nonmem_version = Sys.getenv("BBR_TESTS_NONMEM_VERSION", "nm74gf"),
      .bbi_args = list(mpi_exec_path = get_mpiexec_path())
    )

    # copy model file into new model dir
    fs::file_copy(CTL_TEST_FILE, MODEL_DIR_BBI)

    # Create base model to bootstrap with (not yet submitted)
    mod1 <- new_model(
      file.path(MODEL_DIR_BBI, "1"),
      .description = "original test-bootstrap model",
      .tags = ORIG_TAGS,
      .bbi_args = list(threads = 4, parallel = TRUE)
    )

    test_that("new_bootstrap_run works as expected", {
      # Test inclusion/exclusion of table and covariance records
      .boot_run <- new_bootstrap_run(mod1, remove_cov = FALSE, remove_tables = FALSE)
      expect_true(mod_has_record(.boot_run, "table"))
      expect_true(mod_has_record(.boot_run, "covariance"))

      .boot_run <- new_bootstrap_run(mod1, .overwrite = TRUE)
      expect_false(mod_has_record(.boot_run, "table"))
      expect_false(mod_has_record(.boot_run, "covariance"))

      # Check model attributes
      expect_equal(.boot_run$tags, c(mod1$tags, "BOOTSTRAP_SUBMISSION"))
      expect_equal(
        modify_prob_statement(.boot_run),
        as.character(glue("Bootstrap run of model {get_model_id(mod1)}"))
      )

      # Check helper functions before setup
      expect_message(get_boot_models(.boot_run), "has not been set up")
      expect_message(get_model_status(.boot_run), "has not been set up")
      expect_message(check_nonmem_finished(.boot_run), "has not been set up")
      expect_false(bootstrap_is_finished(.boot_run))
      expect_false(bootstrap_is_cleaned_up(.boot_run))
      expect_true(is.null(get_boot_spec(.boot_run)))
    })

    # Read in model for remainder of tests
    .boot_run <- read_model(file.path(MODEL_DIR_BBI, "1-boot"))
    boot_dir <- .boot_run[[ABS_MOD_PATH]]

    test_that("setup_bootstrap_run messages if nm_join cant be used", {
      boot_spec_path <- get_boot_spec_path(.boot_run, .check_exists = FALSE)
      expect_false(fs::file_exists(boot_spec_path))
      # Set up bootstrap run object using *non-submitted* model
      expect_message(
        setup_bootstrap_run(.boot_run, n = 3),
        "Defaulting to input data"
      )
      expect_true(fs::file_exists(boot_spec_path))
    })

    # Submit based_on model to use for remainder of tests
    proc1 <- submit_model(mod1, .mode = "local", .wait = TRUE)

    test_that("setup_bootstrap_run works as expected", {
      setup_bootstrap_run(.boot_run, n = 3, .overwrite = TRUE, seed = NULL)

      # Check boot spec
      boot_spec <- get_boot_spec(.boot_run)
      expect_true(is.null(boot_spec$cleaned_up))
      expect_true(is.null(boot_spec$seed))
      expect_true(is.null(boot_spec$strat_cols))
      expect_true(tibble::is_tibble(boot_spec$bootstrap_runs))

      # Check helper functions after setup & before submission
      expect_no_message(boot_models <- get_boot_models(.boot_run))
      expect_message(get_model_status(.boot_run), "0 model(s) have finished", fixed = TRUE)
      # returns single TRUE & message
      expect_message(check_nonmem_finished(.boot_run), "has not been submitted")
      # returns TRUE & message for each model (not meant to be called this way by user)
      expect_message(res <- check_nonmem_finished(boot_models), "has not been submitted")
      expect_true(all(res))
      # Logical and error checks
      expect_false(bootstrap_is_finished(.boot_run))
      expect_false(bootstrap_is_cleaned_up(.boot_run))
      expect_error(
        bootstrap_can_be_summarized(.boot_run),
        "One or more bootstrap runs have not finished executing"
      )
      expect_error(
        cleanup_bootstrap_run(.boot_run),
        "One or more bootstrap runs have not finished executing"
      )

      # Check gitignore
      boot_dir <- .boot_run[[ABS_MOD_PATH]]
      ignored_mods <- purrr::map_chr(get_boot_models(.boot_run), get_model_id)
      ignore_lines <- readLines(file.path(boot_dir, ".gitignore"))
      expect_true(all(c("*.ctl", "*.mod", "*.yaml", "/data") %in% ignore_lines))
      expect_true(all(paste0("/", ignored_mods) %in% ignore_lines))
    })

    test_that("setup_bootstrap_run works with stratification columns", {
      # Test stratification columns
      orig_data <- fread(get_data_path(.boot_run), na.strings = ".", verbose = FALSE) %>%
        as_tibble()
      names(orig_data) <- toupper(names(orig_data))
      setup_bootstrap_run(
        .boot_run, n = 4, strat_cols = c("SEX", "ETN"), .overwrite = TRUE,
        seed = 1234
      )
      # Pull in one of the sample datasets
      data <- nm_data(get_boot_models(.boot_run)[[1]])

      # stat_col columns should be close to the original split, with
      # (n_unique_col_vals - 1) identical matches (the last one can differ due
      # to n_subjects differing when sampling)
      expect_equal(
        length(table(orig_data$ETN)[table(orig_data$ETN) == table(data$ETN)]),
        length(table(data$ETN)) - 1
      )
      expect_equal(
        length(table(orig_data$SEX)[table(orig_data$SEX) == table(data$SEX)]),
        length(table(data$SEX)) - 1
      )

      # Check exact split as a test for the seed setting.
      expect_equal(as.vector(table(data$ETN)), c(340, 140, 300))

      # Check boot spec
      boot_spec <- get_boot_spec(.boot_run)
      expect_equal(boot_spec$seed, 1234)
      expect_equal(boot_spec$strat_cols, c("SEX", "ETN"))
    })

    test_that("submitting bootstrap model objects works", {

      # These calls wont use batch submission since .batch_size < length(boot_models)
      # - decrease batch size to test batch submission
      withr::with_envvar(new = c("BBR_DEV_LOAD_PATH" = getwd()), {
        expect_message(
          proc <- submit_model(.boot_run, .mode = "local", .wait = TRUE, .batch_size = 2),
          "submitting 4 models in batches of 2", fixed = TRUE
        )
        wait_for_nonmem(.boot_run)
        expect_true(
          any(grepl(
            "The following model(s) have finished: `1, 2, 3, 4`",
            readLines(proc$get_output_file()), fixed = TRUE
          ))
        )
      })

      # Check helper functions after submission & before summarization
      expect_no_message(boot_models <- get_boot_models(.boot_run))
      expect_message(
        get_model_status(.boot_run, max_print = 1),
        "3 model(s) have finished", fixed = TRUE
      )
      expect_true(check_nonmem_finished(.boot_run))
      expect_true(all(check_nonmem_finished(boot_models)))
      expect_true(bootstrap_is_finished(.boot_run))
      expect_false(bootstrap_is_cleaned_up(.boot_run)) # cannot be cleaned up
      expect_true(bootstrap_can_be_summarized(.boot_run)) # can now be summarized
      expect_error(
        cleanup_bootstrap_run(.boot_run), "Model has not been summarized yet"
      )
    })

    test_that("summarize_bootstrap_run works as expected", {
      boot_sum_path <- file.path(boot_dir, "boot_summary.RDS")
      expect_false(fs::file_exists(boot_sum_path))
      expect_no_message(summarize_bootstrap_run(.boot_run))
      expect_true(fs::file_exists(boot_sum_path))
      expect_message(
        boot_sum <- summarize_bootstrap_run(.boot_run),
        "Reading in bootstrap summary"
      )

      # Check run heuristics and details (uses model_summaries instead of summary_log)
      boot_mod_sums <- model_summaries(
        get_boot_models(.boot_run),
        .bbi_args = list(no_grd_file = TRUE, no_ext_file = TRUE, no_shk_file = TRUE)
      )

      run_details <- purrr::map_dfr(boot_mod_sums, function(sum){
        as_tibble(
          c(list2(!!ABS_MOD_PATH := sum[[ABS_MOD_PATH]]), sum$bbi_summary[[SUMMARY_DETAILS]])
        ) %>% tidyr::nest("output_files_used" = "output_files_used")
      })
      expect_true(all.equal(boot_sum$run_details, run_details))

      run_heuristics <- purrr::map_dfr(boot_mod_sums, function(sum){
        as_tibble(
          c(list2(!!ABS_MOD_PATH := sum[[ABS_MOD_PATH]]), sum$bbi_summary[[SUMMARY_HEURISTICS]])
        )
      })
      expect_true(all.equal(boot_sum$run_heuristics, run_heuristics))

      # Check run variables
      expect_equal(boot_sum$n_samples, 3)
      expect_equal(boot_sum$seed, 1234)
      expect_equal(boot_sum$strat_cols, c("SEX", "ETN"))
      expect_equal(boot_sum[[ABS_MOD_PATH]], boot_dir)
      expect_equal(
        boot_sum$based_on_model_path, ctl_ext(get_based_on(.boot_run))
      )
      expect_equal(
        boot_sum$based_on_data_set, get_data_path(read_model(get_based_on(.boot_run)))
      )

      # Check helper functions after summarization & before cleanup
      expect_true(check_nonmem_finished(.boot_run))
      expect_true(bootstrap_is_finished(.boot_run))
      expect_false(bootstrap_is_cleaned_up(.boot_run)) # is not cleaned up
      expect_true(bootstrap_can_be_summarized(.boot_run)) # can still be summarized
    })

    test_that("cleanup_bootstrap_run works as expected", {
      cleanup_bootstrap_run(.boot_run, .force = TRUE)

      # Check helper functions after cleanup
      expect_message(check_nonmem_finished(.boot_run), "has been cleaned up")
      expect_message(boot_models <- get_boot_models(.boot_run), "has been cleaned up")
      expect_true(is.null(boot_models))
      expect_true(bootstrap_is_finished(.boot_run))
      expect_true(bootstrap_is_cleaned_up(.boot_run))
      expect_error(
        bootstrap_can_be_summarized(.boot_run),
        "The bootstrap run has been cleaned up"
      )
      expect_error(
        cleanup_bootstrap_run(.boot_run),
        "Bootstrap run has already been cleaned"
      )

      # Make sure the model object and summary can still be read in
      .boot_run <- read_model(file.path(MODEL_DIR_BBI, "1-boot"))
      expect_message(
        boot_sum <- summarize_bootstrap_run(.boot_run),
        "Reading in bootstrap summary"
      )

      # Check model/data file existence
      files_kept <- fs::dir_ls(boot_dir, all = TRUE) %>% fs::path_rel(boot_dir)
      expect_equal(files_kept, c(".gitignore", "bbr_boot_spec.json", "boot_summary.RDS"))

      # Confirm boot spec alterations
      boot_spec <- get_boot_spec(.boot_run)
      expect_true(boot_spec$cleaned_up)
      expect_true(is.null(boot_spec$bootstrap_runs))
    })
  })
