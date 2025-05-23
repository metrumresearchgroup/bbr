context("submit_models(.dry_run=TRUE)")

#####################################
# testing multiple model submission
#####################################

withr::local_options(list(
  bbr.bbi_exe_path = read_bbi_path(),
  bbr.DEV_skip_system_mode_checks = TRUE
))

# create fake bbi.yaml
readr::write_file("created_by: test-submit-models", file.path(MODEL_DIR, "bbi.yaml"))
on.exit(fs::file_delete(file.path(MODEL_DIR, "bbi.yaml")))

model_dir <- ABS_MODEL_DIR
mod_ctl_path <- purrr::map_chr(
  as.character(1:3),
  ~ file.path(model_dir, fs::path_ext_set(., "ctl"))
)

default_mode <- getOption("bbr.bbi_exe_mode")
cmd_prefix <- paste("cd", model_dir, ";", read_bbi_path(), "nonmem", "run")

test_that("submit_models(.dry_run=TRUE) with list input simple", {
  # copy to two new models
  mod2 <- copy_model_from(MOD1, 2)
  mod3 <- copy_model_from(MOD1, 3)
  on.exit(cleanup())

  .mods <- list(MOD1, mod2, mod3)

  # submit models
  proc_list <- submit_models(.mods, .dry_run = TRUE)
  expect_true(all(purrr::map_lgl(proc_list, function(.proc) {
    "bbi_process" %in% class(.proc)
  })))

  # check that there is only one distinct arg set
  expect_equal(length(proc_list), 1)

  # check call
  expect_identical(
    proc_list[[1]][[PROC_CALL]],
    as.character(glue("{cmd_prefix} {default_mode} {paste(mod_ctl_path, collapse = ' ')} --overwrite --parallel --threads=4"))
  )
})

test_that("submit_models(.dry_run=TRUE) with list input, 2 arg sets", {
  # copy to two new models
  mod2 <- copy_model_from(MOD1, 2) %>% add_bbi_args(list(threads = 3))
  mod3 <- copy_model_from(MOD1, 3) %>% add_bbi_args(list(clean_lvl = 2))
  on.exit(cleanup())

  .mods <- list(MOD1, mod2, mod3)

  # submit and test that passed .bbi_args override args in object
  proc_list <- submit_models(.mods, .dry_run = TRUE, .bbi_args = list(threads = 1))
  expect_true(all(purrr::map_lgl(proc_list, function(.proc) {
    "bbi_process" %in% class(.proc)
  })))

  # check that there are two distinct arg sets
  expect_equal(length(proc_list), 2)

  # check each call
  expect_identical(
    proc_list[[1]][[PROC_CALL]],
    as.character(
      glue(
        "{cmd_prefix} {default_mode} {mod_ctl_path[1]} {mod_ctl_path[2]} --overwrite --threads=1"
      )
    )
  )
  expect_identical(
    proc_list[[2]][[PROC_CALL]],
    as.character(
      glue("{cmd_prefix} {default_mode} {mod_ctl_path[3]} --clean_lvl=2 --overwrite --threads=1")
    )
  )
})

test_that("submit_models() works for models in different directories", {
  new_dir <- file.path(ABS_MODEL_DIR, "level2")
  fs::dir_create(new_dir)

  # create fake bbi.yaml
  readr::write_file("created_by: test-submit-models", file.path(new_dir, "bbi.yaml"))
  on.exit(fs::file_delete(file.path(new_dir, "bbi.yaml")))
  on.exit(cleanup())

  # TODO: use test helper functions, e.g., create_all_models(), once the
  # model_directory option is deprecated
  mod2 <- copy_model_from(
    MOD1,
    file.path(new_dir, MOD_ID),
    "created by test-submit-models.R"
  )
  proc_list <- submit_models(list(MOD1, mod2), .dry_run = TRUE)

  expect_equal(length(proc_list), 2L)
})

test_that("submit_models(.dry_run=TRUE) errors with bad input", {
  # copy to two new models
  mod2 <- copy_model_from(MOD1, 2)
  mod3 <- copy_model_from(MOD1, 3)
  on.exit(cleanup())

  # testing when one isn't a model
  fake <- list(naw = 1)
  .mods <- list(MOD1, mod2, mod3, fake)

  expect_error(
    submit_models(.mods, .dry_run = TRUE),
    regexp = "must contain only model objects"
  )

  # testing two different kinds of models
  class(fake) <- c(BBI_BASE_MODEL_CLASS, class(fake))
  fake[[YAML_MOD_TYPE]] <- "fake"
  .mods <- list(MOD1, mod2, mod3, fake)

  expect_error(
    submit_models(.mods, .dry_run = TRUE),
    regexp = "must contain all the same type of models"
  )
})

test_that("submit_models() works with non-NULL .config_path", {
  temp_config <- tempfile(fileext = ".yaml")
  readr::write_file("foo", temp_config)
  temp_config <- normalizePath(temp_config)
  on.exit(fs::file_delete(temp_config))

  res <- submit_models(
    list(MOD1),
    .config_path = temp_config,
    .dry_run = TRUE
  )

  expect_identical(
    res[[1L]][[PROC_CALL]],
    as.character(
      glue(
        "{cmd_prefix} {default_mode} {mod_ctl_path[[1L]]} --overwrite --parallel --threads=4",
        "--config={temp_config}",
        .sep = " "
      )
    )
  )
})

test_that("submit_models() works if .bbi_args is empty", {
  # set existing arguments to NULL via `.bbi_args`
  res <- submit_models(
    list(MOD1),
    .bbi_args = list(overwrite = NULL, threads = NULL, parallel = TRUE),
    .dry_run = TRUE
  )

  expect_identical(
    res[[1L]][[PROC_CALL]],
    as.character(
      glue("{cmd_prefix} {default_mode} {mod_ctl_path[[1L]]} --parallel")
    )
  )

  # now the case where the YAML file does not contain any CLI arguments
  temp_mod_path <- create_temp_model()
  mod <- read_model(temp_mod_path)
  mod <- replace_all_bbi_args(mod, NULL)

  # create fake bbi.yaml
  readr::write_file("created_by: test-submit-models", file.path(dirname(temp_mod_path), "bbi.yaml"))
  on.exit(fs::file_delete(file.path(dirname(temp_mod_path), "bbi.yaml")))

  res <- submit_models(list(mod), .dry_run = TRUE)

  expect_identical(
    res[[1L]][[PROC_CALL]],
    as.character(
      glue(
        "cd {dirname(temp_mod_path)} ;",
        "{read_bbi_path()} nonmem run {default_mode} {fs::path_ext_set(temp_mod_path, 'ctl')}",
        .sep = " "
      )
    )
  )
})

test_that("submit_models(.mode) inherits option", {
  other_mode <- if (identical(default_mode, "local")) "slurm" else "local"
  withr::with_options(list(bbr.bbi_exe_mode = other_mode), {
    expect_identical(
      submit_models(list(MOD1), .dry_run = TRUE)[[1]][[PROC_CALL]],
      as.character(glue("{cmd_prefix} {other_mode} {ABS_CTL_PATH} --overwrite --parallel --threads=4"))
    )
  })
})
