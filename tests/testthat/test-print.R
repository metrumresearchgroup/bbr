context("testing print methods for bbi objects")

skip_if_not_drone_or_metworx("test-print")

model_dir <- ABS_MODEL_DIR
mod_ctl_path <- file.path(model_dir, CTL_FILENAME)
model_1 <- MOD1

withr::with_options(list(rbabylon.bbi_exe_path = read_bbi_path()), {

  test_that("print.babylon_process works with .wait = TRUE", {
    proc <- bbi_exec("--help", .wait = TRUE)
    res <- capture.output(print(proc))
    expect_identical(res, c(PROC_HELP_STR, "Process finished."))
  })

  test_that("print.babylon_process works with .wait = FALSE", {
    proc <- bbi_exec("--help", .wait = FALSE)
    res <- capture.output(print(proc))
    expect_identical(res, c(PROC_HELP_STR, "Not waiting for process to finish."))
  })

  test_that("print.babylon_process works with dry run", {
    proc <- bbi_dry_run("--help", ".")
    res <- capture.output(print(proc))
    expect_identical(res, c(PROC_HELP_STR, "DRY RUN! Process not actually run."))
  })

  test_that("print.babylon_process(.call_limit) works", {
    # create a bunch of fake models
    temp_dir <- file.path(get_model_working_directory(MOD1), "print-test")
    fs::dir_create(temp_dir)
    on.exit(fs::dir_delete(temp_dir))
    mods <- purrr::map(1:50, ~copy_model_from(MOD1, file.path("print-test", .x)))
    proc <- submit_models(mods, .dry_run = TRUE)

    # check that default has bbi path, at least two model paths, and flags
    call_str <- capture.output(print(proc[[1]]))[2]
    expect_true(str_detect(call_str, read_bbi_path()))
    expect_true(str_detect(call_str, as.character(glue("{temp_dir}/1\\.ctl.+{temp_dir}/2\\.ctl"))))
    expect_true(str_detect(call_str, "--overwrite --threads=4"))

    # check that passing in .call_limit=30 has bbi path, NO model paths, but still has flags
    call_str <- capture.output(print(proc[[1]], .call_limit=30))[2]
    expect_true(str_detect(call_str, read_bbi_path()))
    expect_false(str_detect(call_str, temp_dir))
    expect_true(str_detect(call_str, "--overwrite --threads=4"))
  })


  test_that("print.bbi_nonmem_model contains proper fields if all present", {
    model_1[[YAML_NOTES]] <- list('a' = 'x',
                                  'b' = 'y')

    res <- paste(capture_messages(print(model_1)),
                 collapse = '\n')

    fields <- c('Status',
                'Absolute Model Path',
                'Description',
                'Tags',
                'BBI Args',
                'Notes')

    expect_true(all(purrr::map_lgl(fields, ~ str_detect(res, .x))))
  })

  test_that("print.bbi_nonmem_model run status functions properly", {

    res <- paste(capture_messages(print(model_1)),
                 collapse = '\n')

    expect_true(str_detect(res, "Finished Running"))

    model1_no_output_dir <- model_1
    model1_no_output_dir[[ABS_MOD_PATH]] <- 'some/fake/path'

    res <- paste(capture_messages(print(model1_no_output_dir)),
                 collapse = '\n')

    expect_true(str_detect(res, "Not Run"))

    mod_no_config <- read_model(file.path(MODEL_DIR_X, "1001"))
    res <- paste(capture_messages(print(mod_no_config)),
                 collapse = '\n')

    expect_true(str_detect(res, "Incomplete Run"))

  })

  test_that("print.bbi_nonmem_model handles missing or no length inputs", {

    optional_fields <- c(
      YAML_DESCRIPTION,
      YAML_TAGS,
      YAML_BBI_ARGS,
      YAML_NOTES
    )

    # missing all optional fields
    mod_no_optionals <-
      structure(model_1[!names(model_1) %in% optional_fields],
                class = class(model_1))

    res <- paste(capture_messages(print(mod_no_optionals)),
                 collapse = '\n')

    expect_false(str_detect(res, 'Notes'))
    expect_false(str_detect(res, 'BBI Args'))
    expect_false(str_detect(res, 'Tags'))
    expect_false(str_detect(res, 'Description'))

  })

})
