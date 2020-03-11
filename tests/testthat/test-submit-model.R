context("submit_model(.dry_run=T)")

MODEL_FILE <- "1.ctl"
MODEL_YAML <- yaml_ext(MODEL_FILE)
MODEL_DIR <- "model-examples"
MODEL_ABS_PATH <- file.path(getwd(), MODEL_DIR, MODEL_FILE)

withr::with_options(list(rbabylon.model_directory = NULL), {

  test_that("submit_model(.dry_run=T) returns correct command string",
            {
              # basic defaults
              withr::with_options(list(rbabylon.bbi_exe_path = "bbi"), {
                expect_identical(
                  submit_model(file.path(MODEL_DIR, MODEL_FILE), .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE}"))
                )

                # basic defaults with local
                expect_identical(
                  submit_model(
                    file.path(MODEL_DIR, MODEL_FILE),
                    .mode = "local",
                    .dry_run = T
                  )[[PROC_CALL]],
                  as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run local {MODEL_FILE}"))
                )

                # add some args
                expect_identical(
                  submit_model(
                    file.path(MODEL_DIR, MODEL_FILE),
                    .bbi_args = list(
                      "json" = T,
                      "threads" = 4,
                      "nm_version" = "nm74"
                    ),
                    .dry_run = TRUE
                  )[[PROC_CALL]],
                  as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE} --json --threads=4 --nm_version=nm74"))
                )
              })
            })

  test_that("submit_model(.dry_run=T) for yaml input parses correctly",
            {
              withr::with_options(list(rbabylon.bbi_exe_path = "bbi"), {
                # correctly parsing yaml
                expect_identical(
                  submit_model(file.path(MODEL_DIR, MODEL_YAML), .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE} --overwrite --threads=4"))
                )

                # no extension correctly finds yaml
                expect_identical(
                  submit_model(file.path(MODEL_DIR, tools::file_path_sans_ext(MODEL_FILE)), .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE} --overwrite --threads=4"))
                )

                # over-riding yaml arg with passed arg
                expect_identical(
                  submit_model(file.path(MODEL_DIR, MODEL_YAML), .bbi_args=list(threads=2), .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE} --overwrite --threads=2"))
                )

              })
            })


  test_that("submit_model(.dry_run=T) with bbi_nonmem_model object parses correctly",
            {
              MOD1 <- read_model(file.path(MODEL_DIR, MODEL_YAML))
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
