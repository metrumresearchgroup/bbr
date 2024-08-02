
# define constants
MODEL_DIR_BBI <- file.path(dirname(ABS_MODEL_DIR), "test-nmtran-models")
NONMEM_DIR <- Sys.getenv("BBR_TESTS_NONMEM_DIR", "/opt/NONMEM")
REQ_NONMEM_VERSIONS <- c("nm73gf", "nm74gf", "nm75")

# Don't assume NONMEM is available if not on Metworx.
if (Sys.getenv("METWORX_VERSION") == "" || Sys.getenv("SKIP_BBI_TEST") == "true") {
  skip("test-run-nmtran only runs on Metworx because it needs NONMEM installed")
}


# Skip if required NONMEM versions are not installed
if(!all(REQ_NONMEM_VERSIONS %in% basename(fs::dir_ls(NONMEM_DIR)))){
  req_nm_ver_txt <- paste(REQ_NONMEM_VERSIONS, collapse = ", ")
  skip(glue("test-run-nmtran requires the following NONMEM versions: {req_nm_ver_txt}"))
}


# cleanup function
cleanup_bbi <- function(.recreate_dir = FALSE) {
  if (fs::dir_exists(MODEL_DIR_BBI)) fs::dir_delete(MODEL_DIR_BBI)
  if (isTRUE(.recreate_dir)) fs::dir_create(MODEL_DIR_BBI)
}
cleanup_bbi(.recreate_dir = TRUE)


# set options and run tests
withr::with_options(list(
  bbr.bbi_exe_path = read_bbi_path(),
  bbr.verbose = FALSE), {

    # cleanup when done
    on.exit({cleanup_bbi()})

    # clear old bbi.yaml
    if (fs::file_exists(file.path(MODEL_DIR_BBI, "bbi.yaml"))) fs::file_delete(file.path(MODEL_DIR_BBI, "bbi.yaml"))

    # create new bbi.yaml
    bbi_init(
      MODEL_DIR_BBI,
      .nonmem_dir = NONMEM_DIR,
      .nonmem_version = "nm74gf"
    )

    # copy model files into new model dir (not run)
    fs::file_copy(CTL_TEST_FILE, MODEL_DIR_BBI)
    fs::file_copy(YAML_TEST_FILE, MODEL_DIR_BBI)

    describe("run_nmtran", {
      it("nmtran_setup: executable and nonmem version", {
        mod1 <- read_model(file.path(MODEL_DIR_BBI, "1"))
        # Using model object, looks for bbi.yaml
        nmtran_specs <- nmtran_setup(mod1)
        # Confirm executable
        expect_equal(nmtran_specs$nmtran_exe, "/opt/NONMEM/nm74gf/tr/NMTRAN.exe")
        # Confirm NONMEM version
        expect_equal(nmtran_specs$nonmem_version, "nm74gf")

        # Passed nonmem version
        nmtran_specs <- nmtran_setup(mod1, .nonmem_version = "nm75")
        nmtran_specs2 <- nmtran_setup(mod1, list(nm_version = "nm75"))
        # Confirm executable
        expect_equal(nmtran_specs$nmtran_exe, "/opt/NONMEM/nm75/tr/NMTRAN.exe")
        # Confirm NONMEM version
        expect_equal(nmtran_specs$nonmem_version, "nm75")
        expect_equal(nmtran_specs2$nonmem_version, "nm75")


        # Passed config_path
        nmtran_specs <- nmtran_setup(mod1, .config_path = file.path(MODEL_DIR_BBI, "bbi.yaml"))
        # Confirm executable
        expect_equal(nmtran_specs$nmtran_exe, "/opt/NONMEM/nm74gf/tr/NMTRAN.exe")
        # Confirm NONMEM version
        expect_equal(nmtran_specs$nonmem_version, "nm74gf")

        # Incorrect nonmem version
        expect_error(
          nmtran_setup(mod1, .bbi_args = list(nm_version = "nm74")),
          "Must specify a valid `nm_version`"
        )

        # no configuration file found
        expect_error(
          nmtran_setup(mod1, .config_path = file.path(tempfile(), "bbi.yaml")),
          "No bbi configuration was found at"
        )
      })

      it("nmtran_setup: NM-TRAN args", {
        mod1 <- read_model(file.path(MODEL_DIR_BBI, "1"))

        # Default nmfe_options passed as .bbi_args
        nmtran_specs <- nmtran_setup(mod1)
        expect_equal(unname(nmtran_specs$cmd_args), c("0", "0", "2"))

        # Override with .bbi_args
        nmtran_specs <- nmtran_setup(mod1, .bbi_args = list(maxlim = 3, prdefault = TRUE))
        expect_equal(unname(nmtran_specs$cmd_args), c("1", "0", "3"))

        # Override with model yaml
        current_args <- mod1$bbi_args
        mod1 <- add_bbi_args(mod1, list(maxlim = 3, tprdefault = TRUE))
        nmtran_specs <- nmtran_setup(mod1, .bbi_args = NULL)
        expect_equal(unname(nmtran_specs$cmd_args), c("0", "1", "3"))
        mod1 <- replace_all_bbi_args(mod1, current_args)

        # Override with bbi.yaml
        # - note: we only look at the `nmfe_options` in bbi.yaml. If these options
        #   were passed as regular `.bbi_args` (e.g., in a bbi_init() call), they
        #   would not be picked up.
        bbi_yaml_path <- get_bbi_yaml_path(mod1)
        bbi_yaml <- yaml::read_yaml(bbi_yaml_path)
        bbi_yaml$nmfe_options$maxlim <- 3
        bbi_yaml$nmfe_options$prdefault <- TRUE
        yaml::write_yaml(bbi_yaml, bbi_yaml_path)
        nmtran_specs <- nmtran_setup(mod1, .bbi_args = NULL)
        expect_equal(unname(nmtran_specs$cmd_args), c("1", "0", "3"))

        # mimic submit_model: a FALSE value for .bbi_arg has no effect
        # if a TRUE value is found in the bbi.yaml
        nmtran_specs <- nmtran_setup(mod1, .bbi_args = list(prdefault = FALSE))
        expect_equal(unname(nmtran_specs$cmd_args), c("1", "0", "3"))

        # Revert back for any other tests
        bbi_yaml$nmfe_options$maxlim <- 2
        bbi_yaml$nmfe_options$prdefault <- NULL
        yaml::write_yaml(bbi_yaml, bbi_yaml_path)
      })

      it("parse_nmtran_args: parses and formats nmfe options correctly", {
        mod1 <- read_model(file.path(MODEL_DIR_BBI, "1"))
        bbi_yaml_path <- get_bbi_yaml_path(mod1)
        bbi_yaml <- yaml::read_yaml(bbi_yaml_path)

        # Only model yaml
        cmd_args <- parse_nmtran_args(mod1, nmfe_options = bbi_yaml$nmfe_options)
        expect_equal(unname(cmd_args), c("0", "0", "2"))

        # Override with model yaml
        current_args <- mod1$bbi_args
        mod1 <- add_bbi_args(mod1, list(tprdefault = TRUE, maxlim = 3))

        # Test priority of .bbi_args > model yaml > bbi.yaml
        # - TRUE values overwrite FALSE values if both are present

        # Overwrite .bbi_args --> takes precedence
        cmd_args <- mod1 %>% parse_nmtran_args(
          .bbi_args = list(prdefault = TRUE, tprdefault = FALSE, maxlim = 1),
          nmfe_options = bbi_yaml$nmfe_options
        )

        expect_equal(unname(cmd_args), c("1", "0", "1"))

        # Overwrite bbi.yaml --> model yaml defaults take precedence
        cmd_args <- mod1 %>% parse_nmtran_args(
          nmfe_options = list(prdefault = TRUE, tprdefault = FALSE, maxlim = 1)
        )

        expect_equal(unname(cmd_args), c("1", "1", "3"))

        # Check with all three sources
        cmd_args <- mod1 %>% parse_nmtran_args(
          .bbi_args = list(tprdefault = FALSE, maxlim = 2),
          nmfe_options = list(tprdefault = FALSE, maxlim = 1)
        )

        expect_equal(unname(cmd_args), c("0", "0", "2"))

        cmd_args <- mod1 %>% parse_nmtran_args(
          .bbi_args = list(prdefault = TRUE, tprdefault = FALSE, maxlim = 2),
          nmfe_options = list(tprdefault = TRUE, maxlim = 1)
        )

        expect_equal(unname(cmd_args), c("1", "1", "2"))

        # Reset model yaml .bbi_args for other tests
        mod1 <- replace_all_bbi_args(mod1, current_args)
      })

      it("execute_nmtran", {
        # Execute in subdirectory to avoid messing with other tests
        nmtran_dir <- file.path(MODEL_DIR_BBI, "nmtran")
        fs::dir_create(nmtran_dir)
        on.exit(fs::dir_delete(nmtran_dir), add = TRUE)

        # Copy model file into new model dir
        fs::file_copy(CTL_TEST_FILE, file.path(nmtran_dir, "2.ctl"))
        mod2 <- new_model(file.path(nmtran_dir, "2"))

        # create new bbi.yaml
        bbi_init(
          nmtran_dir,
          .nonmem_dir = NONMEM_DIR,
          .nonmem_version = "nm74gf"
        )

        nmtran_specs <- nmtran_setup(mod2)

        nmtran_results <- execute_nmtran(
          nmtran_specs$nmtran_exe, mod_path = get_model_path(mod2),
          dir = nmtran_dir
        )

        # Check attributes
        expect_equal(nmtran_dir, nmtran_results$run_dir)
        expect_equal(nmtran_results$status_val, 0)
        expect_equal(nmtran_results$status, "Finished Running")

        # Test failure
        data_path <- "test/this/path/data.csv"
        modify_data_path_ctl(mod2, data_path)

        nmtran_results <- execute_nmtran(
          nmtran_specs$nmtran_exe, mod_path = basename(get_model_path(mod2)),
          dir = nmtran_dir
        )

        # Check attributes
        expect_true(nmtran_results$status_val > 0)
        expect_equal(nmtran_results$status, "Failed. See errors.")
      })

      it("run_nmtran: integration", {
        # create model
        mod1 <- read_model(file.path(MODEL_DIR_BBI, "1"))

        # Run with no presort
        nmtran_results <- run_nmtran(
          mod1, .bbi_args = list(nm_version = "nm73gf"), clean = FALSE
        )
        on.exit(fs::dir_delete(nmtran_results$run_dir))

        # Check attributes
        expect_equal(get_model_path(mod1), nmtran_results$absolute_model_path)
        expect_equal(nmtran_results$nmtran_model, basename(get_model_path(mod1)))
        expect_equal(nmtran_results$nonmem_version, "nm73gf")
        expect_equal(nmtran_results$status_val, 0)
        expect_equal(nmtran_results$status, "Finished Running")
        expect_true(is.null(nmtran_results$nmtran_presort_exe))


        # Run with presort
        nmtran_results <- run_nmtran(
          mod1, .bbi_args = list(nm_version = "nm75"), clean = FALSE
        )
        on.exit(fs::dir_delete(nmtran_results$run_dir))

        # Check attributes
        expect_equal(get_model_path(mod1), nmtran_results$absolute_model_path)
        expect_equal(nmtran_results$nmtran_model, "1_presort.ctl")
        expect_equal(nmtran_results$nonmem_version, "nm75")
        expect_equal(nmtran_results$status_val, 0)
        expect_equal(nmtran_results$status, "Finished Running")
        expect_false(is.null(nmtran_results$nmtran_presort_exe))
        expect_true(fs::file_exists(nmtran_results$nmtran_presort_exe))
      })
    })
  })
