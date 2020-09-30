context("submit_models(.dry_run=T)")

#####################################
# testing multiple model submission
#####################################

withr::with_options(list(rbabylon.bbi_exe_path = "bbi",
                         rbabylon.model_directory = normalizePath(MODEL_DIR)), {

  # create fake babylon.yaml
  readr::write_file("created_by: test-submit-models", file.path(MODEL_DIR, "babylon.yaml"))
  on.exit({ fs::file_delete(file.path(MODEL_DIR, "babylon.yaml")) })

  test_that("submit_models(.dry_run=T) with list input simple",
            {
              # copy to two new models
              mod2 <- copy_model_from(MOD1, 2, "naw")
              mod3 <- copy_model_from(MOD1, 3, "naw")
              on.exit({ cleanup() })

              .mods <- list(MOD1, mod2, mod3)

              # submit models
              proc_list <- submit_models(.mods, .dry_run = T)
              expect_true(all(purrr::map_lgl(proc_list, function(.proc) { "babylon_process" %in% class(.proc) })))

              # check that there is only one distinct arg set
              expect_equal(length(proc_list), 1)

              # check call
              expect_identical(
                proc_list[[1]][[PROC_CALL]],
                as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {CTL_FILENAME} 2.ctl 3.ctl --overwrite --threads=4"))
              )
            })

  test_that("submit_models(.dry_run=T) with list input, 2 arg sets",
            {
              # copy to two new models
              mod2 <- copy_model_from(MOD1, 2, "naw") %>% add_bbi_args(list(threads = 3))
              mod3 <- copy_model_from(MOD1, 3, "naw") %>% add_bbi_args(list(clean_lvl = 2))
              on.exit({ cleanup() })

              .mods <- list(MOD1, mod2, mod3)

              # submit and test that passed .bbi_args override args in object
              proc_list <- submit_models(.mods, .dry_run = T, .bbi_args = list(threads = 1))
              expect_true(all(purrr::map_lgl(proc_list, function(.proc) { "babylon_process" %in% class(.proc) })))

              # check that there are two distinct arg sets
              expect_equal(length(proc_list), 2)

              # check each call
              expect_identical(
                proc_list[[1]][[PROC_CALL]],
                as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge {CTL_FILENAME} 2.ctl --overwrite --threads=1"))
              )
              expect_identical(
                proc_list[[2]][[PROC_CALL]],
                as.character(glue("cd {file.path(getwd(), MODEL_DIR)} ; bbi nonmem run sge 3.ctl --clean_lvl=2 --overwrite --threads=1"))
              )
            })

  test_that("submit_models() works for models in different directories", {
    new_dir <- "level2"
    fs::dir_create(file.path(MODEL_DIR, new_dir))
    on.exit(cleanup())

    mod2 <- copy_model_from(
      MOD1,
      file.path(new_dir, CTL_FILENAME),
      "created by test-submit-models.R"
    )
    proc_list <- submit_models(list(MOD1, mod2), .dry_run = TRUE)

    expect_equal(length(proc_list), 2L)

    # should not generate a --config
    expect_false(grepl("--config", proc_list[[1L]][["call"]], fixed = TRUE))

    expect_true(
      grepl(
        "--config=../babylon.yaml",
        proc_list[[2L]][["call"]],
        fixed = TRUE
      )
    )
  })

  test_that("submit_models(.dry_run=T) errors with bad input",
            {
              # copy to two new models
              mod2 <- copy_model_from(MOD1, 2, "naw")
              mod3 <- copy_model_from(MOD1, 3, "naw")
              on.exit({ cleanup() })

              # testing when one isn't a model
              fake <- list(naw = 1)
              .mods <- list(MOD1, mod2, mod3, fake)

              expect_error(
                submit_models(.mods, .dry_run = T),
                regexp = "must contain only model objects"
              )

              # testing two different kinds of models
              class(fake) <- c("bbi_stan_model", class(fake))
              fake[[YAML_MOD_TYPE]] <- "stan"
              .mods <- list(MOD1, mod2, mod3, fake)

              expect_error(
                submit_models(.mods, .dry_run = T),
                regexp = "must contain all the same type of models"
              )
            })
}) # closing withr::with_options

