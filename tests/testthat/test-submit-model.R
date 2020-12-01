context("submit_model(.dry_run=T)")

###################################
# testing single model submission
###################################

# create fake babylon.yaml
readr::write_file("created_by: test-submit-model", "babylon.yaml")
on.exit({ fs::file_delete("babylon.yaml")})

model_dir <- ABS_MODEL_DIR
mod_ctl_path <- file.path(model_dir, CTL_FILENAME)

test_that("submit_model(.dry_run=T) returns correct command string",
          {
            withr::with_options(list(rbabylon.bbi_exe_path = "bbi"), {
              # correctly parsing yaml
              expect_identical(
                submit_model(MOD1, .dry_run = T)[[PROC_CALL]],
                as.character(glue("cd {model_dir} ; bbi nonmem run sge {mod_ctl_path} --overwrite --threads=4"))
              )

              # switch to local mode
              expect_identical(
                submit_model(MOD1, .mode = "local", .dry_run = T)[[PROC_CALL]],
                as.character(glue("cd {model_dir} ; bbi nonmem run local {mod_ctl_path} --overwrite --threads=4"))
              )

              # over-riding yaml arg with passed args
              expect_identical(
                submit_model(MOD1,
                             .bbi_args=list(
                               "json" = T,
                               "threads" = 2,
                               "nm_version" = "nm74"
                             ),
                             .dry_run = T)[[PROC_CALL]],
                as.character(glue("cd {model_dir} ; bbi nonmem run sge {mod_ctl_path} --overwrite --threads=2 --json --nm_version=nm74"))
              )
            })
          })

test_that("submit_model(.dry_run=T) with bbi_nonmem_model object parses correctly",
          {
            withr::with_options(list(rbabylon.bbi_exe_path = "bbi"), {
              # correctly parsing yaml
              expect_identical(
                submit_model(MOD1, .dry_run = T)[[PROC_CALL]],
                as.character(glue("cd {model_dir} ; bbi nonmem run sge {mod_ctl_path} --overwrite --threads=4"))
              )

              # over-riding yaml arg with passed arg
              expect_identical(
                submit_model(MOD1, list(threads=2), .dry_run = T)[[PROC_CALL]],
                as.character(glue("cd {model_dir} ; bbi nonmem run sge {mod_ctl_path} --overwrite --threads=2"))
              )

            })
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
      glue::glue(
        "cd {model_dir} ;",
        "bbi nonmem run sge {mod_ctl_path} --overwrite --threads=4",
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
