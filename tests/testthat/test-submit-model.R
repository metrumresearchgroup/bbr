context("submit_model(.dry_run=T)")

MODEL_DIR <- "model-examples"
MODEL_FILE <- "1.ctl"
MODEL_YAML <- yaml_ext(MODEL_FILE)
YAML_PATH <- file.path(MODEL_DIR, MODEL_YAML)
MODEL_PATH <- file.path(MODEL_DIR, MODEL_FILE)
MODEL_ABS_PATH <- file.path(getwd(), MODEL_DIR, MODEL_FILE)


withr::with_options(list(rbabylon.model_directory = NULL), {

  test_that("submit_model(.dry_run=T) returns correct command string",
            {
              withr::with_options(list(rbabylon.bbi_exe_path = "bbi"), {
                # correctly parsing yaml
                expect_identical(
                  submit_model(YAML_PATH, .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE} --overwrite --threads=4"))
                )

                # switch to local mode
                expect_identical(
                  submit_model(YAML_PATH, .mode = "local", .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run local {MODEL_FILE} --overwrite --threads=4"))
                )

                # no extension correctly finds yaml
                expect_identical(
                  submit_model(file.path(MODEL_DIR, tools::file_path_sans_ext(MODEL_FILE)), .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE} --overwrite --threads=4"))
                )

                # over-riding yaml arg with passed args
                expect_identical(
                  submit_model(YAML_PATH,
                               .bbi_args=list(
                                 "json" = T,
                                 "threads" = 2,
                                 "nm_version" = "nm74"
                               ),
                               .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE} --overwrite --threads=2 --json --nm_version=nm74"))
                )
              })
            })

  test_that("submit_model(.dry_run=T) with .ctl input parses correctly",
            {
              # basic defaults
              withr::with_options(list(rbabylon.bbi_exe_path = "bbi"), {
                expect_error(
                  submit_model(MODEL_PATH, .dry_run = T)[[PROC_CALL]],
                  regexp = "delete the YAML file if it does not correspond to this model"
                )

                # copy to a different name and try it
                new_mod_path <- stringr::str_replace(MODEL_PATH, "1", "2")
                fs::file_copy(MODEL_PATH, new_mod_path)

                expect_identical(
                  submit_model(new_mod_path, .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {basename(new_mod_path)}"))
                )

                # cleanup
                fs::file_delete(new_mod_path)
                fs::file_delete(yaml_ext(new_mod_path))
              })
            })


  test_that("submit_model(.dry_run=T) with bbi_nonmem_model object parses correctly",
            {
              MOD1 <- read_model(YAML_PATH)
              withr::with_options(list(rbabylon.bbi_exe_path = "bbi"), {
                # correctly parsing yaml
                expect_identical(
                  submit_model(MOD1, .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE} --overwrite --threads=4"))
                )

                # over-riding yaml arg with passed arg
                expect_identical(
                  submit_model(MOD1, .bbi_args=list(threads=2), .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE} --overwrite --threads=2"))
                )

              })
            })

}) # closing withr::with_options

withr::with_options(list(rbabylon.model_directory = "model-examples"), {
  test_that("submit_model(.dry_run=T) with numeric input parses correctly",
          {
            withr::with_options(list(rbabylon.bbi_exe_path = "bbi"), {
              # correctly parsing yaml
              expect_identical(
                submit_model(1, .dry_run = T)[[PROC_CALL]],
                as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE} --overwrite --threads=4"))
              )
            })
          })
}) # closing withr::with_options
