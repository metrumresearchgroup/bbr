context("submit_model(.dry_run=T)")


MODEL_FILE <- "1.ctl"
MODEL_DIR <- "model-examples"
MODEL_ABS_PATH <- file.path(getwd(), MODEL_DIR, MODEL_FILE)

test_that("submit_model(.dry_run=T) returns correct command string",
          {
            # basic defaults
            withr::with_options(list(rbabylon.bbi_exe_path = "bbi"), {
              expect_identical(
                submit_model(file.path(MODEL_DIR, MODEL_FILE), .dry_run = T)$call,
                as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE}"))
              )

              # basic defaults with local
              expect_identical(
                submit_model(
                  file.path(MODEL_DIR, MODEL_FILE),
                  .mode = "local",
                  .dry_run = T
                )$call,
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
                )$call,
                as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE} --json --threads=4 --nm_version=nm74"))
              )
            })
          })

test_that("submit_model(.dry_run=T) for yaml input parses correctly",
          {
            withr::with_options(list(rbabylon.bbi_exe_path = "bbi"), {
              # correctly parsing yaml
              expect_identical(
                submit_model(file.path(MODEL_DIR, yaml_ext(MODEL_FILE)), .dry_run = T)$call,
                as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE} --overwrite --threads=4"))
              )

              # no extension correctly finds yaml
              expect_identical(
                submit_model(file.path(MODEL_DIR, tools::file_path_sans_ext(MODEL_FILE)), .dry_run = T)$call,
                as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE} --overwrite --threads=4"))
              )

              # over-riding yaml arg with passed arg
              expect_identical(
                submit_model(file.path(MODEL_DIR, yaml_ext(MODEL_FILE)), .bbi_args=list(threads=2), .dry_run = T)$call,
                as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE} --overwrite --threads=2"))
              )

            })
          })
