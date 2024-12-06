context("submit_model(.dry_run=TRUE)")

###################################
# testing single model submission
###################################

model_dir <- ABS_MODEL_DIR
mod_ctl_path <- file.path(model_dir, CTL_FILENAME)

# create fake bbi.yaml
readr::write_file("created_by: test-submit-model", file.path(model_dir, "bbi.yaml"))
on.exit(fs::file_delete(file.path(model_dir, "bbi.yaml")))

default_mode <- getOption("bbr.bbi_exe_mode")
cmd_prefix <- paste("cd", model_dir, ";", read_bbi_path(), "nonmem", "run")

withr::local_options(list(
  bbr.bbi_exe_path = read_bbi_path(),
  bbr.DEV_skip_system_mode_checks = TRUE
))

test_that("submit_model(.dry_run=TRUE) returns correct command string", {
  # correctly parsing yaml
  expect_identical(
    submit_model(MOD1, .dry_run = TRUE)[[PROC_CALL]],
    as.character(glue("{cmd_prefix} {default_mode} {mod_ctl_path} --overwrite --threads=4 --parallel"))
  )

  # switch to local mode
  expect_identical(
    submit_model(MOD1, .mode = "local", .dry_run = TRUE)[[PROC_CALL]],
    as.character(glue("{cmd_prefix} local {mod_ctl_path} --overwrite --threads=4 --parallel"))
  )

  # over-riding yaml arg with passed args
  expect_identical(
    submit_model(MOD1,
      .bbi_args = list(
        "json" = TRUE,
        "threads" = 2,
        "nm_version" = "nm74"
      ),
      .dry_run = TRUE
    )[[PROC_CALL]],
    as.character(glue("{cmd_prefix} {default_mode} {mod_ctl_path} --overwrite --threads=2 --json --nm_version=nm74 --parallel"))
  )
})

test_that("submit_model(.dry_run=TRUE) with bbi_nonmem_model object parses correctly", {
  # correctly parsing yaml
  expect_identical(
    submit_model(MOD1, .dry_run = TRUE)[[PROC_CALL]],
    as.character(glue("{cmd_prefix} {default_mode} {mod_ctl_path} --overwrite --threads=4 --parallel"))
  )

  # over-riding yaml arg with passed arg
  expect_identical(
    submit_model(MOD1, list(threads = 2), .dry_run = TRUE)[[PROC_CALL]],
    as.character(glue("{cmd_prefix} {default_mode} {mod_ctl_path} --overwrite --threads=2 --parallel"))
  )
})

test_that("submit_model() creates correct call for non-NULL .config_path", {
  temp_config <- tempfile(fileext = ".yaml")
  readr::write_file("foo", temp_config)
  temp_config <- normalizePath(temp_config)
  on.exit(fs::file_delete(temp_config))

  res <- submit_model(MOD1, .config_path = temp_config, .dry_run = TRUE)
  expect_identical(
    res[[PROC_CALL]],
    as.character(
      glue(
        "{cmd_prefix} {default_mode} {mod_ctl_path} --overwrite --threads=4 --parallel",
        "--config={temp_config}",
        .sep = " "
      )
    )
  )
})

test_that("submit_model() throws an error if passed `output_dir` bbi arg", {
  expect_error(
    submit_model(MOD1, .bbi_args = list(output_dir = "foo")),
    "is not a valid argument"
  )
})

test_that("submit_model(.mode) inherits option", {
  other_mode <- switch(default_mode,
    sge = "local",
    "sge"
  )
  withr::with_options(list(bbr.bbi_exe_mode = other_mode), {
    expect_identical(
      submit_model(MOD1, .dry_run = TRUE)[[PROC_CALL]],
      as.character(glue("{cmd_prefix} {other_mode} {mod_ctl_path} --overwrite --threads=4 --parallel"))
    )
  })
})

test_that("submit_model(.mode) errors when NULL", {
  withr::with_options(list(bbr.bbi_exe_mode = NULL), {
    expect_error(
      submit_model(MOD1, .dry_run = TRUE),
      regexp = "Nothing was passed.+mode"
    )
  })
})

test_that("submit_model(.mode) errors when invalid", {
  expect_error(
    submit_model(MOD1, .dry_run = TRUE, .mode = "naw"),
    regexp = "Invalid value passed.+mode"
  )
})

test_that("submit_model aborts if .mode='sge' is used with Slurm's qsub", {
  skip_if_old_bbi("3.4.0")

  sbatch <- unname(Sys.which("sbatch"))
  if (identical(sbatch, "") || identical(Sys.getenv("METWORX_VERSION"), "")) {
    skip("not on Metworx with Slurm")
  }

  withr::local_options(list(bbr.DEV_skip_system_mode_checks = FALSE))

  expect_error(
    submit_model(MOD1, .dry_run = TRUE, .mode = "sge"),
    regexp = "Slurm shim"
  )
})

test_that("submit_model aborts if .mode='sge' and qsub is not available", {
  if (!identical(unname(Sys.which("qsub")), "")) {
    skip("qsub is available")
  }

  withr::local_options(list(bbr.DEV_skip_system_mode_checks = FALSE))

  expect_error(
    submit_model(MOD1, .dry_run = TRUE, .mode = "sge"),
    regexp = "qsub is not available"
  )
})

test_that("submit_model aborts if .mode='slurm' is used with incompatible bbi", {
  if (test_bbi_version(read_bbi_path(), "3.4.0")) {
    skip("installed bbi version is compatible with Slurm")
  }

  withr::local_options(list(bbr.DEV_skip_system_mode_checks = FALSE))

  expect_error(
    submit_model(MOD1, .dry_run = TRUE, .mode = "slurm"),
    regexp = "at least version 3.4.0",
    fixed = TRUE
  )
})
