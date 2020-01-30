context("submit-nonmem-model(.dry_run=T)")

test_that("submit-nonmem-model(.dry_run=T) returns correct command string",
          {
            # basic defaults
            withr::with_options(list(rbabylon.bbi_exe_path = "bbi"), {
              expect_identical(
                submit_nonmem_model("/data/240/001.mod", .dry_run = T)$call,
                "cd /data/240 ; bbi nonmem run sge 001.mod"
              )

              # basic defaults with local
              expect_identical(
                submit_nonmem_model(
                  "/data/240/001.mod",
                  .type = "local",
                  .dry_run = T
                )$call,
                "cd /data/240 ; bbi nonmem run local 001.mod"
              )

              # add some args
              expect_identical(
                submit_nonmem_model(
                  "/data/240/001.mod",
                  .args = list(
                    "json" = T,
                    "threads" = 4,
                    "nm_version" = "nm74"
                  ),
                  .dry_run = TRUE
                )$call,
                "cd /data/240 ; bbi nonmem run sge 001.mod --json --threads=4 --nmVersion=nm74"
              )

              # multiple models
              expect_identical(
                submit_nonmem_model(
                  "/data/240/[001:004].mod",
                  .args = list("overwrite" = TRUE),
                  .dry_run = TRUE
                )$call,
                "cd /data/240 ; bbi nonmem run sge [001:004].mod --overwrite"
              )
            })
          })

test_that("submit-nonmem-model(.dry_run=T) for yaml input parses correctly",
          {
            withr::with_options(list(rbabylon.bbi_exe_path = "bbi"), {
              expect_identical(
                submit_nonmem_model("data/modtest.yaml", .dry_run = T)$call,
                "cd /data/240 ; bbi nonmem run sge 001.mod --overwrite --threads=4 --nmVersion=nm74gf"
              )
            })
          })
