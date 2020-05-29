context("submit_model(.dry_run=T)")

MODEL_DIR <- "model-examples"
MODEL_FILE <- "1.ctl"
YAML_PATH <- file.path(MODEL_DIR, yaml_ext(MODEL_FILE))
YML_PATH  <- file.path(MODEL_DIR, yml_ext(MODEL_FILE))
MODEL_PATH <- file.path(MODEL_DIR, MODEL_FILE)
MODEL_ABS_PATH <- file.path(getwd(), MODEL_DIR, MODEL_FILE)

###################################
# testing single model submission
###################################

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
              # find YAML if it exists
              withr::with_options(list(rbabylon.bbi_exe_path = "bbi"), {
                expect_identical(
                  submit_model(MODEL_PATH, .dry_run = T)[[PROC_CALL]],
                  as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE} --overwrite --threads=4"))
                )

                # copy to a different name and error because no yaml
                new_mod_path <- stringr::str_replace(MODEL_PATH, "1", "2")
                fs::file_copy(MODEL_PATH, new_mod_path)

                expect_error(
                  submit_model(new_mod_path, .dry_run = T)[[PROC_CALL]],
                  regexp = FIND_YAML_ERR_MSG
                )

                # cleanup
                fs::file_delete(new_mod_path)
              })
            })


  test_that("submit_model(.dry_run=T) with .mod input parses correctly",
            {
              withr::with_options(list(rbabylon.bbi_exe_path = "bbi"), {
                # copy to a .mod extensions
                new_mod_path <- stringr::str_replace(MODEL_PATH, "1.ctl", "2.mod")
                fs::file_copy(MODEL_PATH, new_mod_path)
                yaml::write_yaml(list(description = "original acop model",
                                      model_type = "nonmem"),
                                 yaml_ext(new_mod_path))


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
                  submit_model(MOD1, list(threads=2), .dry_run = T)[[PROC_CALL]],
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

#####################################
# testing multiple model submission
#####################################

withr::with_options(list(rbabylon.model_directory = MODEL_DIR, rbabylon.bbi_exe_path = "bbi"), {
  test_that("submit_models(.dry_run=T) with list input simple",
            {
              # read first model
              mod1 <- read_model(1)

              # copy to two new models
              mod2 <- copy_model_from(1, 2, "naw")
              mod3 <- copy_model_from(1, 3, "naw")

              .mods <- list(mod1, mod2, mod3)

              # submit models
              proc_list <- submit_models(.mods, .dry_run = T)
              expect_true(all(purrr::map_lgl(proc_list, function(.proc) { "babylon_process" %in% class(.proc) })))

              # check that there is only one distinct arg set
              expect_equal(length(proc_list), 1)

              # check call
              expect_identical(
                proc_list[[1]][[PROC_CALL]],
                as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE} 2.ctl 3.ctl --overwrite --threads=4"))
              )

              # cleanup after test
              for (m in c("2", "3")) {
                m <- file.path(MODEL_DIR, m)
                if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
                if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
              }

            })

  test_that("submit_models(.dry_run=T) with list input, 2 arg sets",
            {
              # read first model
              mod1 <- read_model(1)

              # copy to two new models
              mod2 <- copy_model_from(1, 2, "naw") %>% add_bbi_args(list(threads = 3))
              mod3 <- copy_model_from(1, 3, "naw") %>% add_bbi_args(list(clean_lvl = 2))

              .mods <- list(mod1, mod2, mod3)

              # submit and test that passed .bbi_args override args in object
              proc_list <- submit_models(.mods, .dry_run = T, .bbi_args = list(threads = 1))
              expect_true(all(purrr::map_lgl(proc_list, function(.proc) { "babylon_process" %in% class(.proc) })))

              # check that there are two distinct arg sets
              expect_equal(length(proc_list), 2)

              # check each call
              expect_identical(
                proc_list[[1]][[PROC_CALL]],
                as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE} 2.ctl --overwrite --threads=1"))
              )
              expect_identical(
                proc_list[[2]][[PROC_CALL]],
                as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge 3.ctl --clean_lvl=2 --overwrite --threads=1"))
              )

              # cleanup after test
              for (m in c("2", "3")) {
                m <- file.path(MODEL_DIR, m)
                if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
                if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
              }

            })

  test_that("submit_models(.dry_run=T) errors with bad input",
            {
              # read first model
              mod1 <- read_model(1)

              # copy to two new models
              mod2 <- copy_model_from(1, 2, "naw")
              mod3 <- copy_model_from(1, 3, "naw")

              # testing when one isn't a model
              fake <- list(naw = 1)
              .mods <- list(mod1, mod2, mod3, fake)

              expect_error(
                submit_models(.mods, .dry_run = T),
                regexp = "must contain only model objects"
              )

              # testing two different kinds of models
              class(fake) <- c("bbi_stan_model", class(fake))
              fake[[YAML_MOD_TYPE]] <- "stan"
              .mods <- list(mod1, mod2, mod3, fake)

              expect_error(
                submit_models(.mods, .dry_run = T),
                regexp = "must contain all the same type of models"
              )

              # cleanup after test
              for (m in c("2", "3")) {
                m <- file.path(MODEL_DIR, m)
                if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
                if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
              }

            })

  test_that("submit_models(.dry_run=T) with character and numeric input yaml",
            {
              # read first model
              mod1 <- read_model(1)

              # copy to two new models
              mod2 <- copy_model_from(1, 2, "naw")
              mod3 <- copy_model_from(1, 3, "naw")

              # try with and without extension
              .test_list <- list(
                c(1, 2, 3),
                c("1", "2", "3"),
                c("1.yaml", "2.yaml", "3.yaml")
              )
              for (.mods in .test_list) {
                proc_list <- submit_models(.mods, .dry_run = T)

                # check that there is only one distinct arg set
                expect_equal(length(proc_list), 1)

                # check call
                expect_identical(
                  proc_list[[1]][[PROC_CALL]],
                  as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {MODEL_FILE} 2.ctl 3.ctl --overwrite --threads=4"))
                )
              }

              # cleanup after test
              for (m in c("2", "3")) {
                m <- file.path(MODEL_DIR, m)
                if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
                if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
              }

            })

  test_that("submit_models(.dry_run=T) with character input ctl",
            {
              # copy control streams
              fs::file_copy(MODEL_PATH, file.path(MODEL_DIR, "2.ctl"))
              fs::file_copy(MODEL_PATH, file.path(MODEL_DIR, "3.ctl"))

              # only test the new ones so it doesn't complain about YAML already existing
              proc_list <- submit_models(c("2.ctl", "3.ctl"), .dry_run = T)

              # check that there is only one distinct arg set
              expect_equal(length(proc_list), 1)

              # check call
              expect_identical(
                proc_list[[1]][[PROC_CALL]],
                as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge 2.ctl 3.ctl"))
              )

              # cleanup after test
              for (m in c("2", "3")) {
                m <- file.path(MODEL_DIR, m)
                if (fs::file_exists(yaml_ext(m))) fs::file_delete(yaml_ext(m))
                if (fs::file_exists(ctl_ext(m))) fs::file_delete(ctl_ext(m))
              }

            })


}) # closing withr::with_options
