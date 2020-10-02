context("submit_model(.dry_run=T)")

###################################
# testing single model submission
###################################

withr::with_options(list(rbabylon.model_directory = NULL), {

  # create fake babylon.yaml
  readr::write_file("created_by: test-submit-model", "babylon.yaml")
  on.exit({ fs::file_delete("babylon.yaml")})

  model_dir <- file.path(getwd(), MODEL_DIR)
  mod_ctl_path <- file.path(model_dir, CTL_FILENAME)

  test_that("submit_model(.dry_run=T) returns correct command string",
            {
              withr::with_options(list(rbabylon.bbi_exe_path = "bbi"), {
                # correctly parsing yaml
                expect_identical(
                  submit_model(MOD1, .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {model_dir} ; bbi nonmem run sge {mod_ctl_path} --overwrite --threads=4 --config=../babylon.yaml"))
                )

                # switch to local mode
                expect_identical(
                  submit_model(MOD1, .mode = "local", .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {model_dir} ; bbi nonmem run local {mod_ctl_path} --overwrite --threads=4 --config=../babylon.yaml"))
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
                  as.character(glue("cd {model_dir} ; bbi nonmem run sge {mod_ctl_path} --overwrite --threads=2 --json --nm_version=nm74 --config=../babylon.yaml"))
                )
              })
            })

  test_that("submit_model(.dry_run=T) with bbi_nonmem_model object parses correctly",
            {
              withr::with_options(list(rbabylon.bbi_exe_path = "bbi"), {
                # correctly parsing yaml
                expect_identical(
                  submit_model(MOD1, .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {model_dir} ; bbi nonmem run sge {mod_ctl_path} --overwrite --threads=4 --config=../babylon.yaml"))
                )

                # over-riding yaml arg with passed arg
                expect_identical(
                  submit_model(MOD1, list(threads=2), .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {model_dir} ; bbi nonmem run sge {mod_ctl_path} --overwrite --threads=2 --config=../babylon.yaml"))
                )

              })
            })

}) # closing withr::with_options
