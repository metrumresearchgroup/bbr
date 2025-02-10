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
skip_long_tests("skipping long-running bbi workflow tests (bootstrap)")

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
      .bbi_args = list(parallel = FALSE)
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
      expect_false(check_nonmem_finished(.boot_run))
      expect_false(model_is_finished(.boot_run))
      expect_false(analysis_is_cleaned_up(.boot_run))
      expect_true(is.null(get_analysis_spec(.boot_run)))
    })

    # Read in model for remainder of tests
    .boot_run <- read_model(file.path(MODEL_DIR_BBI, "1-boot"))
    boot_dir <- .boot_run[[ABS_MOD_PATH]]

    test_that("print.bbi_nmboot_model has a custom print method (before setup)", {
      bullets <- capture.output({
        output_lines <- capture.output(print(.boot_run)) %>% suppressMessages()
        expect_message(print(.boot_run), regexp = "Bootstrap Run")
        expect_message(print(.boot_run), regexp = "Bootstrap Args")
        expect_true(any(grepl("Not set up", output_lines)))
      })
    })

    test_that("setup_bootstrap_run fails if filtering expressions cant be parsed", {
      mod2 <- copy_model_from(mod1, "2")
      boot_run2 <- new_bootstrap_run(mod2)
      on.exit(delete_models(list(mod2, boot_run2), .force = TRUE, .tags = NULL))

      # Add additional IGNORE expressions and compare to dplyr filters
      ctl <- get_model_ctl(boot_run2)
      data_rec <- nmrec::select_records(ctl, "data")[[1]]
      data_rec$parse()

      # Set filter that should error
      current_filter <- data_rec$values[[7]]$value
      data_rec$values[[7]]$value <- "(ID.EQ.2X)"
      nmrec::write_ctl(ctl, get_model_path(boot_run2))

      expect_error(
        setup_bootstrap_run(boot_run2, n = 3),
        "ignore/accept list could not be converted to filters"
      )

      # Revert filter and check for warning that the model hasnt been run
      data_rec$values[[7]]$value <- current_filter
      nmrec::write_ctl(ctl, get_model_path(boot_run2))

      expect_warning(
        setup_bootstrap_run(boot_run2, n = 3),
        "has not been submitted"
      )
    })

    # Submit based_on model to use for remainder of tests
    proc1 <- submit_model(mod1, .mode = "local", .wait = TRUE)

    test_that("setup_bootstrap_run fails if n records are different", {
      # If the based on model has been executed, we check that the number of
      # records in the model_summary matches the number of rows in the filtered
      # dataset (`nm_data(mod, filter = TRUE)`).
      mod2 <- copy_model_from(mod1, "2")
      copy_output_dir(mod1, file.path(MODEL_DIR_BBI, get_model_id(mod2)))
      boot_run2 <- new_bootstrap_run(mod2)
      on.exit(delete_models(list(mod2, boot_run2), .force = TRUE, .tags = NULL))

      # Add additional IGNORE expressions and compare to dplyr filters
      ctl <- get_model_ctl(boot_run2)
      data_rec <- nmrec::select_records(ctl, "data")[[1]]
      data_rec$parse()

      # Set filter that should error
      data_rec$values[[7]]$value <- "(ID.EQ.2, SEX.EQ.1)"
      nmrec::write_ctl(ctl, get_model_path(boot_run2))

      expect_error(
        setup_bootstrap_run(boot_run2, n = 3),
        "The filtered dataset does not have the same number of records"
      )
    })

    test_that("setup_bootstrap_run works with user-provided dataset", {
      starting_data <- nm_join(mod1) %>% suppressMessages()
      setup_bootstrap_run(.boot_run, n = 1, data = starting_data, .overwrite = TRUE)
      data_save_path <- file.path(boot_dir, "boot-data.csv")
      expect_true(fs::file_exists(data_save_path))
      expect_equal(data_save_path, get_data_path(.boot_run))

      # Test submission to ensure child models are unaffected by the data path adjustment
      proc <- submit_model(.boot_run, .mode = "local", .wait = TRUE)
      expect_true(check_nonmem_finished(.boot_run))
    })

    test_that("setup_bootstrap_run works as expected", {
      setup_bootstrap_run(.boot_run, n = 3, .overwrite = TRUE, seed = NULL)

      # Check print method - strat_cols are made to be NA if none provided
      bullets <- capture.output({
        output_lines <- capture.output(print(.boot_run)) %>% suppressMessages()
        expect_true(any(grepl("Number of runs: 3", output_lines)))
        expect_true(any(grepl("Stratification Columns: NA", output_lines)))
      })

      # Check boot spec
      boot_spec <- get_analysis_spec(.boot_run)
      expect_true(is.null(boot_spec$cleaned_up))
      expect_true(is.null(boot_spec$seed))
      expect_true(is.null(boot_spec$strat_cols))
      expect_true(tibble::is_tibble(boot_spec$bootstrap_runs))

      # Check helper functions after setup & before submission
      expect_no_message(boot_models <- get_boot_models(.boot_run))
      expect_message(get_model_status(.boot_run), "0 model(s) have finished", fixed = TRUE)
      expect_false(check_nonmem_finished(.boot_run))
      # returns FALSE for each model (not meant to be called this way by user)
      res <- check_nonmem_finished(boot_models)
      expect_false(all(res))
      # Logical and error checks
      expect_false(model_is_finished(.boot_run))
      expect_false(analysis_is_cleaned_up(.boot_run))
      expect_error(
        analysis_can_be_summarized(.boot_run),
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
        .boot_run, n = 3, strat_cols = c("SEX", "ETN"), .overwrite = TRUE,
        seed = 1234
      )

      # Check print method - strat_cols are now displayed (comma separated)
      bullets <- capture.output({
        output_lines <- capture.output(print(.boot_run)) %>% suppressMessages()
        expect_true(any(grepl("Number of runs: 3", output_lines)))
        expect_true(any(grepl("Stratification Columns: SEX, ETN", output_lines)))
      })

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
      boot_spec <- get_analysis_spec(.boot_run)
      expect_equal(boot_spec$seed, 1234)
      expect_equal(boot_spec$strat_cols, c("SEX", "ETN"))
    })

    test_that("submitting bootstrap model objects works", {

      # These calls wont use batch submission since .batch_size < length(boot_models)
      with_bg_env({
        # Run this as a batch submission
        # - There is ~3 second delay before an output directory gets created during batch submission
        # - due to this, we need to sleep before calling wait_for_nonmem
        expect_message(
          proc <- submit_model(.boot_run, .mode = "local", .wait = TRUE, .batch_size = 2),
          "Submitting 3 models in batches of 2", fixed = TRUE
        )
        # There is a delay when running batch (about 4 seconds)
        wait_for_nonmem(.boot_run, .delay = 6)
        expect_true(all(check_nonmem_finished(get_boot_models(.boot_run))))

        # There is another delay before the final status gets added to OUTPUT
        #  - Check for one of the earlier messages
        proc_output <- proc$get_output_file()
        on.exit(fs::file_delete(proc_output), add = TRUE)
        expect_true(
          any(grepl(
            "Submitting 2 models with 1 unique configurations",
            readLines(proc_output), fixed = TRUE
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
      expect_true(model_is_finished(.boot_run))
      expect_false(analysis_is_cleaned_up(.boot_run)) # cannot be cleaned up
      expect_true(analysis_can_be_summarized(.boot_run)) # can now be summarized
      expect_error(
        cleanup_bootstrap_run(.boot_run), "Model has not been summarized yet"
      )

      # Attempt to overwrite
      expect_error(submit_model(.boot_run, .mode = "local"), "Model output already exists")

      # Check that overwriting works
      #  - create fake bootstrap run, since we want to clean up the original
      .boot_fake <- new_bootstrap_run(mod1, .suffix = "fake-boot")
      on.exit(delete_models(.boot_fake, .tags = NULL, .force = TRUE), add = TRUE)
      .boot_fake <- setup_bootstrap_run(.boot_fake, n = 3)
      proc <- submit_model(.boot_fake, .overwrite = TRUE, .mode = "local")

      # Immediately kill. Status will be "Not Run", but the the directory will exist,
      # triggering the .overwrite error
      Sys.sleep(0.5)
      proc[[1]]$process$kill()
      expect_error(submit_model(.boot_fake, .mode = "local"), "Model output already exists")

      # Check that overwriting works
      expect_message(
        proc <- submit_model(.boot_fake, .overwrite = TRUE, .mode = "local"),
        "Overwriting existing bootstrap output directories", fixed = TRUE
      )
      proc[[1]]$process$kill()
    })

    test_that("bootstrap run inclusion in config_log (before summary)", {
      config_df <- config_log(MODEL_DIR_BBI)
      expect_equal(nrow(config_df), 2)
      expect_equal(config_df$run, c("1-boot", "1"))
      # bbi and nonmem versions will be NULL until the run has been summarized
      expect_true(is.na(config_df$bbi_version[config_df$run == "1-boot"]))
      expect_true(is.na(config_df$nm_version[config_df$run == "1-boot"]))
    })

    test_that("summarize_bootstrap_run works as expected", {
      boot_sum_path <- get_analysis_sum_path(.boot_run, .check_exists = FALSE)
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
      expect_true(model_is_finished(.boot_run))
      expect_false(analysis_is_cleaned_up(.boot_run)) # is not cleaned up
      expect_true(analysis_can_be_summarized(.boot_run)) # can still be summarized
    })

    test_that("bootstrap run inclusion in config_log (after summary)", {
      config_df <- config_log(MODEL_DIR_BBI)
      expect_equal(nrow(config_df), 2)
      expect_equal(config_df$run, c("1-boot", "1"))
      # bbi and nonmem versions are populated after the run has been summarized
      expect_false(is.na(config_df$bbi_version[config_df$run == "1-boot"]))
      expect_false(is.na(config_df$nm_version[config_df$run == "1-boot"]))
    })

    test_that("print.bbi_nmboot_model has a custom print method (after setup)", {
      bullets <- capture.output({
        output_lines <- capture.output(print(.boot_run)) %>% suppressMessages()
        expect_message(print(.boot_run), regexp = "Bootstrap Run")
        expect_message(print(.boot_run), regexp = "Bootstrap Args")
        expect_true(any(grepl("Number of runs: 3", output_lines)))
        expect_true(any(grepl("Stratification Columns: SEX, ETN", output_lines)))
      })
    })

    test_that("print.bbi_nmboot_summary has a custom print method", {
      # Check print method of summary object
      boot_sum <- summarize_bootstrap_run(.boot_run)
      bullets <- capture.output({
        output_lines <- capture.output(print(boot_sum)) %>% suppressMessages()
        expect_message(print(boot_sum), regexp = "Based on")
        expect_message(print(boot_sum), regexp = "Run Specifications")
        expect_message(print(boot_sum), regexp = "Bootstrap Run Summary")
        expect_true(any(grepl("Number of runs: 3", output_lines)))
        expect_true(any(grepl("Stratification Columns: SEX, ETN", output_lines)))
        # seed is displayed in summary object only
        expect_true(any(grepl("Seed: 1234", output_lines)))
      })
    })

    test_that("get_boot_models works when the model was run in another location", {

      # Copy model files and output directory of simulation
      new_dir <- tempdir()
      new_dir_path <- file.path(new_dir, basename(.boot_run[[ABS_MOD_PATH]]))
      fs::file_copy(ctl_ext(.boot_run[[ABS_MOD_PATH]]), ctl_ext(new_dir_path), overwrite = TRUE)
      fs::file_copy(yaml_ext(.boot_run[[ABS_MOD_PATH]]), yaml_ext(new_dir_path), overwrite = TRUE)
      fs::dir_copy(.boot_run[[ABS_MOD_PATH]], new_dir_path, overwrite = TRUE)
      fake_boot <- read_model(new_dir_path)
      on.exit(delete_models(fake_boot, .tags = NULL, .force = TRUE))

      # Read in the new _fake_ simulation and make sure the individual models
      # point to the correct location
      boot_models_fake <- get_boot_models(fake_boot)
      purrr::walk(boot_models_fake, function(boot_m){
        expect_equal(
          boot_m[[ABS_MOD_PATH]],
          file.path(new_dir_path, get_model_id(boot_m))
        )
      })

      # Expect error if it cant be found for any reason (i.e. no yaml)
      # This gives `fake_boot` the "Incomplete Run" status when done this way
      # - would be "Not Run" if `delete_models()` was used
      purrr::walk(boot_models_fake, function(boot_m){
        fs::file_delete(yaml_ext(boot_m[[ABS_MOD_PATH]]))
      })

      expect_error(
        get_boot_models(fake_boot),
        "At least one bootstrap run model does not exist"
      )
    })

    test_that("cleanup_bootstrap_run works as expected", {
      cleanup_bootstrap_run(.boot_run, .force = TRUE)

      # Check helper functions after cleanup
      expect_true(check_nonmem_finished(.boot_run))
      expect_message(boot_models <- get_boot_models(.boot_run), "has been cleaned up")
      expect_true(is.null(boot_models))
      expect_true(model_is_finished(.boot_run))
      expect_true(analysis_is_cleaned_up(.boot_run))
      expect_error(
        analysis_can_be_summarized(.boot_run),
        "The bootstrap run has been cleaned up"
      )
      expect_error(
        cleanup_bootstrap_run(.boot_run),
        "bootstrap run has already been cleaned"
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
      boot_spec <- get_analysis_spec(.boot_run)
      expect_true(boot_spec$cleaned_up)
      expect_true(is.null(boot_spec$bootstrap_runs))

      # Cannot be overwritten
      expect_error(
        submit_model(.boot_run, .overwrite = TRUE, .mode = "local"),
        "Model has been cleaned up"
      )

      # Check updated prints
      bullets <- capture.output({
        # model print
        output_lines <- capture.output(print(.boot_run)) %>% suppressMessages()
        expect_true(any(grepl("Cleaned up: TRUE", output_lines)))
        # summary print
        output_lines_sum <- capture.output(print(boot_sum)) %>% suppressMessages()
        expect_true(any(grepl("Cleaned up: TRUE", output_lines_sum)))
      })
    })

    test_that("bootstrap run inclusion in config_log (after cleanup)", {
      config_df <- config_log(MODEL_DIR_BBI)
      expect_equal(nrow(config_df), 2)
      expect_equal(config_df$run, c("1-boot", "1"))
      # config log remains unchanged after cleanup
      expect_false(is.na(config_df$bbi_version[config_df$run == "1-boot"]))
      expect_false(is.na(config_df$nm_version[config_df$run == "1-boot"]))
    })
  })
