context("submit_model(.dry_run=TRUE)")

###################################
# testing single model submission
###################################



model_dir <- ABS_MODEL_DIR
mod_ctl_path <- file.path(model_dir, CTL_FILENAME)

# create fake bbi.yaml
readr::write_file("created_by: test-submit-model", file.path(model_dir, "bbi.yaml"))
on.exit({ fs::file_delete(file.path(model_dir, "bbi.yaml"))})

withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {
  default_mode <- getOption("bbr.bbi_exe_mode")
  cmd_prefix <- paste("cd", model_dir, ";",
                      read_bbi_path(), "nonmem", "run",
                      default_mode)
  test_that("submit_model(.dry_run=TRUE) returns correct command string",
            {

              # correctly parsing yaml
              expect_identical(
                submit_model(MOD1, .dry_run = TRUE)[[PROC_CALL]],
                as.character(glue("{cmd_prefix} {mod_ctl_path} --overwrite --threads=4 --parallel"))
              )

              # switch to local mode
              expect_identical(
                submit_model(MOD1, .mode = "local", .dry_run = TRUE)[[PROC_CALL]],
                as.character(glue("cd {model_dir} ; {read_bbi_path()} nonmem run local {mod_ctl_path} --overwrite --threads=4 --parallel"))
              )

              # over-riding yaml arg with passed args
              expect_identical(
                submit_model(MOD1,
                             .bbi_args=list(
                               "json" = TRUE,
                               "threads" = 2,
                               "nm_version" = "nm74"
                             ),
                             .dry_run = TRUE)[[PROC_CALL]],
                as.character(glue("{cmd_prefix} {mod_ctl_path} --overwrite --threads=2 --json --nm_version=nm74 --parallel"))
              )
            })

  test_that("submit_model(.dry_run=TRUE) with bbi_nonmem_model object parses correctly",
            {
              # correctly parsing yaml
              expect_identical(
                submit_model(MOD1, .dry_run = TRUE)[[PROC_CALL]],
                as.character(glue("{cmd_prefix} {mod_ctl_path} --overwrite --threads=4 --parallel"))
              )

              # over-riding yaml arg with passed arg
              expect_identical(
                submit_model(MOD1, list(threads=2), .dry_run = TRUE)[[PROC_CALL]],
                as.character(glue("{cmd_prefix} {mod_ctl_path} --overwrite --threads=2 --parallel"))
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
          "{cmd_prefix} {mod_ctl_path} --overwrite --threads=4 --parallel",
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
    other_mode <- switch(default_mode, sge = "local", "sge")
    withr::with_options(list(bbr.bbi_exe_mode = other_mode), {
      expect_identical(
        submit_model(MOD1, .dry_run = TRUE)[[PROC_CALL]],
        as.character(glue("cd {model_dir} ; {read_bbi_path()} nonmem run {other_mode} {mod_ctl_path} --overwrite --threads=4 --parallel"))
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
})

