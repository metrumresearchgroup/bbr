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
    bbr.verbose = FALSE
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
    })

    test_that("setup_bootstrap_run messages if nm_join cant be used", {
      # Set up bootstrap run object using *non-submitted* model
      expect_message(
        setup_bootstrap_run(.boot_run, n = 3),
        "Defaulting to input data"
      )
    })

    # Submit based_on model to use for remainder of tests
    proc1 <- submit_model(mod1, .mode = "local", .wait = TRUE)

    # TODO: need a test for strat_cols --> need feedback on sensible column choices
    test_that("setup_bootstrap_run works as expected", {
      setup_bootstrap_run(.boot_run, n = 3, .overwrite = TRUE)

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

    test_that("submitting bootstrap model objects works", {
      expect_message(
        submit_model(.boot_run, .mode = "local", .wait = TRUE),
        "Inheriting `bbi.yaml`", fixed = TRUE
      )

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
      boot_sum <- summarize_bootstrap_run(.boot_run)
    })

  })
