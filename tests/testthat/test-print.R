context("testing print methods for bbi objects")

skip_if_not_drone_or_metworx("test-print")

model_dir <- ABS_MODEL_DIR
mod_ctl_path <- file.path(model_dir, CTL_FILENAME)
PRINT_REF_DIR <- file.path(REF_DIR, "print-refs")

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

  test_that("print.bbi_nonmem_summary works basic FOCE model", {
    res_str <- file.path(MODEL_DIR, 1) %>%
      read_model() %>%
      model_summary() %>%
      print() %>%
      capture.output()

    test_path <- paste0("/tmp/bbi_print_debugging_", as.character(format(Sys.time(), "%H%M%S")))
    writeLines(res_str, test_path)
    system(glue("aws s3 cp {test_path} s3://metrumrg-sandbox/sethg/"))

    ref_str <- readLines(file.path(PRINT_REF_DIR, "print_nmsum_1.txt"))
    expect_equal(res_str, ref_str)
  })

  # test_that("print.bbi_nonmem_summary works mixture model", {
  #   res_str <- expect_warning({
  #       file.path(MODEL_DIR_X, "iovmm") %>%
  #       read_model() %>%
  #       model_summary() %>%
  #       print() %>%
  #       capture.output()
  #     }, regexp = "param_estimates.+mixture model"
  #   )
  #
  #   ref_str <- readLines(file.path(PRINT_REF_DIR, "print_nmsum_iovmm.txt"))
  #   expect_equal(res_str, ref_str)
  # })
  #
  # test_that("print.bbi_nonmem_summary works Bayes model", {
  #   res_str <- file.path(MODEL_DIR_X, "1001") %>%
  #     read_model() %>%
  #     model_summary(.bbi_args = list(ext_file = "1001.1.TXT")) %>%
  #     print() %>%
  #     capture.output()
  #
  #   ref_str <- readLines(file.path(PRINT_REF_DIR, "print_nmsum_1001.txt"))
  #   expect_equal(res_str, ref_str)
  # })
  #
  # test_that("print.bbi_nonmem_summary works SAEM-IMP model", {
  #   res_str <- file.path(MODEL_DIR_X, "example2_saemimp") %>%
  #     read_model() %>%
  #     model_summary() %>%
  #     print() %>%
  #     capture.output()
  #
  #   ref_str <- readLines(file.path(PRINT_REF_DIR, "print_nmsum_example2_saemimp_noargs.txt"))
  #   expect_equal(res_str, ref_str)
  # })
  #
  # test_that("print.bbi_nonmem_summary works IOV model", {
  #   # load a model summary
  #   res_str <- file.path(MODEL_DIR_X, "acop-iov") %>%
  #     read_model() %>%
  #     model_summary() %>%
  #     print() %>%
  #     capture.output()
  #
  #   ref_str <- readLines(file.path(PRINT_REF_DIR, "print_nmsum_acop-iov_noargs.txt"))
  #   expect_equal(res_str, ref_str)
  # })
  #
  # test_that("print.bbi_nonmem_summary .fixed=TRUE", {
  #   # check with IOV model
  #   res_str <- file.path(MODEL_DIR_X, "acop-iov") %>%
  #     read_model() %>%
  #     model_summary() %>%
  #     print(.fixed = TRUE) %>%
  #     capture.output()
  #
  #   ref_str <- readLines(file.path(PRINT_REF_DIR, "print_nmsum_acop-iov_fixedTRUE.txt"))
  #   expect_equal(res_str, ref_str)
  #
  #   # check with SAEM-IMP model
  #   res_str <- file.path(MODEL_DIR_X, "example2_saemimp") %>%
  #     read_model() %>%
  #     model_summary() %>%
  #     print(.fixed = TRUE) %>%
  #     capture.output()
  #
  #   ref_str <- readLines(file.path(PRINT_REF_DIR, "print_nmsum_example2_saemimp_fixedTRUE.txt"))
  #   expect_equal(res_str, ref_str)
  # })
  #
  # test_that("print.bbi_nonmem_summary .nrow argument", {
  #   # load a model summary
  #   res_str <- file.path(MODEL_DIR_X, "acop-iov") %>%
  #     read_model() %>%
  #     model_summary() %>%
  #     print(.nrow = 15, .fixed = TRUE) %>%
  #     capture.output()
  #
  #   ref_str <- readLines(file.path(PRINT_REF_DIR, "print_nmsum_acop-iov_fixedTRUE_nrow15.txt"))
  #   expect_equal(res_str, ref_str)
  # })
  #
  #
  # test_that("print.bbi_nonmem_summary .off_diag=TRUE", {
  #   .s <- file.path(MODEL_DIR_X, "example2_saemimp") %>%
  #     read_model() %>%
  #     model_summary()
  #
  #   # check without .fixed=TRUE
  #   res_str <- .s %>%
  #     print(.off_diag = TRUE) %>%
  #     capture.output()
  #   ref_str <- readLines(file.path(PRINT_REF_DIR, "print_nmsum_example2_saemimp_offdiagTRUE.txt"))
  #   expect_equal(res_str, ref_str)
  #
  #   # check with .fixed=TRUE
  #   res_str <- .s %>%
  #     print(.off_diag = TRUE, .fixed = TRUE) %>%
  #     capture.output()
  #   ref_str <- readLines(file.path(PRINT_REF_DIR, "print_nmsum_example2_saemimp_fixedTRUE_offdiagTRUE.txt"))
  #   expect_equal(res_str, ref_str)
  # })

})
