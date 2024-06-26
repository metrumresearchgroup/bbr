context("testing simulation functionality and running bbi")

####################################################
# testing simulation submission and nm_join_sim via a composable workflow
# This test file actually runs the bbi calls
# and so it must have working version of both bbi and NONMEM.
# Because of this, it is disabled unless on Metworx.
#
# Additionally, tests in this file rely on each other
# and therefore must be run in order.
####################################################

# can't run on Drone because there's no NONMEM
if (Sys.getenv("METWORX_VERSION") == "" || Sys.getenv("SKIP_BBI_TEST") == "true") {
  skip("test-simulation only runs on Metworx because it needs NONMEM installed")
}
skip_long_tests("skipping long-running bbi workflow tests (simulation)")

# define constants
MODEL_DIR_BBI <- file.path(dirname(ABS_MODEL_DIR), "test-simulation")

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

    # Create base model to simulate with (not yet submitted)
    mod1 <- new_model(
      file.path(MODEL_DIR_BBI, "1"),
      .description = "original test-simulate model",
      .tags = ORIG_TAGS,
      .bbi_args = list(parallel = FALSE)
    )

    test_that("MSF file helpers work correctly", {
      expect_true(is.null(get_msf_path(mod1, .check_exists = FALSE)))
      # Add MSFO=1.MSF record option
      mod1 <- add_msf_opt(mod1)
      expect_equal(
        get_msf_path(mod1, .check_exists = FALSE),
        file.path(get_output_dir(mod1, .check_exists = FALSE), "1.MSF")
      )
    })

    # Submit base model to use for remainder of tests
    submit_model(mod1, .mode = "local", .wait = TRUE)

    # Set simulation args
    join_cols <- c("NUM", "ID")
    sim_cols <- c("DV", "PRED")
    sim_seed <- 4321
    sim_n <- 30

    test_that("new_sim_model made the correct adjustments to control stream", {
      # Make an incomplete simulation model for testing the control stream changes
      # - no spec file is generated, so the parent model (mod2) is unaware of its
      #   presence
      mod2 <- copy_model_from(mod1, "2") %>% update_model_id()
      copy_output_dir(mod1, file.path(MODEL_DIR_BBI, "2"))
      sim_inc <- new_sim_model(
        mod2, n = sim_n, seed = sim_seed,
        sim_cols = sim_cols, .join_col = join_cols
      )
      on.exit(delete_models(list(mod2, sim_inc), .tags = NULL, .force = TRUE), add = TRUE)

      # Does not detect simulation without spec file
      expect_false(has_simulation(mod1))

      # Select records dont exist anymore
      #  - table and simulation records were also removed, but custom ones were
      #    added within the same function
      #  - adjusted data path doesn't need to be checked, as that would have
      #    caused the model submission to fail
      records_rm <- c(
        "estimation", "covariance", "prior", "theta", "thetapv", "omega",
        "omegapd", "sigma", "sigmapd"
      )
      purrr::walk(records_rm, function(rec){
        expect_false(mod_has_record(sim_inc, type = rec))
      })

      # One new table record
      tables <- get_records(sim_inc, "table")
      expect_equal(length(tables), 1)
      table_col_txt <- paste(c(join_cols, sim_cols), collapse = " ")
      sim_tab_name <- paste0(get_model_id(mod2), "-sim.tab")
      expect_equal(
        tables[[1]]$format(),
        as.character(glue("$TABLE ONEHEADER NOPRINT NOAPPEND {table_col_txt} FORMAT=s1PE12.5 FILE={sim_tab_name}\n\n\n"))
      )

      # New simulation record
      sims <- get_records(sim_inc, "sim")
      expect_equal(
        sims[[1]]$format(),
        as.character(glue("$SIMULATION ({sim_seed}) SUBPROBLEMS={sim_n} TRUE=FINAL ONLYSIMULATION\n\n\n"))
      )

      # New msfi record
      msfis <- get_records(sim_inc, "msfi")
      msf_path_rel <- fs::path_rel(get_msf_path(mod2), get_output_dir(sim_inc, .check_exists = FALSE))
      expect_equal(
        msfis[[1]]$format(),
        as.character(glue("$MSFI {msf_path_rel} NOMSFTEST\n\n\n"))
      )
    })

    test_that("add_simulation works as expected", {
      expect_false(has_simulation(mod1))
      expect_error(nm_join_sim(mod1), "No attached simulation found")

      add_simulation(
        mod1, n = sim_n, seed = sim_seed,
        .join_col = join_cols, sim_cols = sim_cols,
        .mode = "local", .wait = TRUE
      )

      # Check for gitignore update
      sim_dir <- get_output_dir(mod1)
      ignore_file <- file.path(sim_dir, ".gitignore")
      ignore_lines <- readLines(ignore_file) %>%
        suppressSpecificWarning("incomplete final line")
      expect_true(any(grepl("/1-sim", ignore_lines)))

      # Confirms the presence of a spec file
      expect_true(has_simulation(mod1))
      sim_spec <- get_sim_spec(mod1)
      expect_equal(sim_spec$seed, sim_seed)
      expect_equal(sim_spec$n_sim, sim_n)

      # Read in simulation model
      sim <- get_simulation(mod1)
      expect_true(all(check_up_to_date(sim)))
      expect_true(all(check_up_to_date(mod1)))

      # Test with model summary object
      add_simulation(
        model_summary(mod1), n = sim_n, seed = sim_seed,
        .join_col = join_cols, sim_cols = sim_cols,
        .mode = "local", .wait = TRUE, .overwrite = TRUE
      )
    })

    test_that("get_simulation works when the model was run in another location", {

      # Copy model files and output directory of simulation
      new_dir <- tempdir()
      new_dir_path <- file.path(new_dir, basename(mod1[[ABS_MOD_PATH]]))
      fs::file_copy(ctl_ext(mod1[[ABS_MOD_PATH]]), ctl_ext(new_dir_path))
      fs::file_copy(yaml_ext(mod1[[ABS_MOD_PATH]]), yaml_ext(new_dir_path))
      fs::dir_copy(mod1[[ABS_MOD_PATH]], new_dir_path)
      fake_mod <- read_model(new_dir_path)
      on.exit(delete_models(fake_mod, .tags = NULL, .force = TRUE))

      # Delete the original simulation to make sure it's not pointing to that
      # (regression test). We will make a new simulation in the next test
      sim <- get_simulation(mod1)
      delete_models(sim, .tags = NULL, .force = TRUE)

      # Read in the new _fake_ simulation
      sim_fake <- get_simulation(fake_mod)

      # Expect error if it cant be found for any reason
      fs::file_delete(yaml_ext(sim_fake[[ABS_MOD_PATH]]))
      expect_error(
        get_simulation(fake_mod),
        "Could not find simulation at the expected file path"
      )
    })

    test_that("add_simulation works with a dataset", {
      # Improper data specification
      bad_data <- nm_data(mod1) %>% dplyr::rename("BLWT"="WT")
      expect_error(
        add_simulation(
          mod1, n = sim_n, seed = sim_seed, data = bad_data,
          .join_col = join_cols, sim_cols = sim_cols,
          .mode = "local", .wait = TRUE, .overwrite = TRUE
        ),
        "The following required input columns were not found"
      )

      # Correct data specification
      add_simulation(
        mod1, n = sim_n, seed = sim_seed, data = nm_data(mod1),
        .join_col = join_cols, sim_cols = sim_cols,
        .mode = "local", .wait = TRUE, .overwrite = TRUE
      )

      # Read in simulation model
      sim <- get_simulation(mod1)
      expect_equal(get_data_path(sim), file.path(MODEL_DIR_BBI, "1", "sim-data.csv"))
      expect_true(all(check_up_to_date(sim)))
      expect_true(all(check_up_to_date(mod1)))
    })

    # Read in model for remainder of tests
    sim <- get_simulation(mod1)

    test_that("nm_join_sim works correctly", {
      # .join_col must match to what was used in `add_simulation`
      expect_error(suppressMessages(nm_join_sim(mod1)), "Column `ID` doesn't exist")
      sim_data <- nm_join_sim(mod1, .join_col = join_cols) %>% suppressMessages()
      expect_equal(
        sort(names(sim_data)),
        unique(sort(c(names(nm_data(mod1)), join_cols, sim_cols, "nn", "DV.DATA")))
      )

      # Can pass in the simulation model, or the nonmem model it's attached to
      expect_true(all.equal(sim_data, suppressMessages(nm_join_sim(sim, .join_col = join_cols))))

      # Select specific columns
      sim_data <- nm_join_sim(mod1, .join_col = join_cols, .cols_keep = join_cols) %>%
        suppressMessages()
      expect_equal(names(sim_data), c(join_cols, sim_cols, "nn"))

      # Test with model summary object
      sum1 <- model_summary(mod1)
      sim_data_sum <- nm_join_sim(sum1, .join_col = join_cols, .cols_keep = join_cols) %>%
        suppressMessages()
      expect_true(all.equal(sim_data, sim_data_sum))
    })

    test_that("print.bbi_nmsim_model has a custom print method", {
      bullets <- capture.output({
        output_lines <- capture.output(print(sim)) %>% suppressMessages()
        expect_message(print(sim), regexp = "Simulation")
        expect_message(print(sim), regexp = "Simulation Args")
        expect_true(any(grepl(glue("Number of Simulations: {sim_n}"), output_lines)))
      })
    })

    test_that("print.bbi_nonmem_model updates when a simulation is added", {
      bullets <- capture.output({
        output_lines <- capture.output(print(mod1)) %>% suppressMessages()
        expect_message(print(mod1), regexp = "NONMEM Model")
        expect_message(print(mod1), regexp = "Attached Simulation")
        expect_true(any(grepl(glue("Status: Finished Running"), output_lines)))
        expect_true(any(grepl(glue("Number of Simulations: {sim_n}"), output_lines)))
      })
    })

    test_that("print.bbi_nonmem_model updates when a simulation is deleted", {
      delete_models(sim, .tags = NULL, .force = TRUE)
      expect_false(has_simulation(mod1)) # ensure spec was deleted too
      bullets <- capture.output({
        output_lines <- capture.output(print(mod1)) %>% suppressMessages()
        expect_false(any(grepl(glue("Status: Finished Running"), output_lines)))
        expect_false(any(grepl(glue("Number of Simulations: {sim_n}"), output_lines)))
      })
    })

  })
