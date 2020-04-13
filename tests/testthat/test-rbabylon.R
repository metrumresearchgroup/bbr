context("rbabylon exec functions")

# constants
BBI_EXE_PATH <- "/data/apps/bbi"

test_that("check_status_code works as expected", {
  # nothing happens on status 0
  expect_equal(check_status_code(0, "stdout...", c("arg1", "arg2")), NULL)

  # other codes error
  expect_error(check_status_code(1, "stdout...", c("arg1", "arg2")))
  expect_error(check_status_code(-1, "stdout...", c("arg1", "arg2")))
  expect_error(check_status_code(225, "stdout...", c("arg1", "arg2")))
})

test_that("bbi_dry_run() correctly returns object", {
  PROC_CLASS_LIST <- c("babylon_process", "list")
  cmd_args <- c("naw", "dawg")
  dir <- "fake/dir"

  # run dry run
  res <- bbi_dry_run(cmd_args, dir)

  # compare to expected
  expect_identical(res[[PROC_PROCESS]], "DRY_RUN")
  expect_identical(res[[PROC_STDOUT]], "DRY_RUN")
  expect_identical(res[[PROC_BBI]], getOption("rbabylon.bbi_exe_path"))
  expect_identical(res[[PROC_CMD_ARGS]], cmd_args)
  expect_identical(res[[PROC_WD]], dir)
  expect_identical(class(res), PROC_CLASS_LIST)
})

if (Sys.getenv("METWORX_VERSION") == "" && Sys.getenv("DRONE") != "true") {
  skip("bbi_init only runs on Metworx or Drone")
} else {

  test_that("check_bbi_exe() correctly errors or finds paths", {
    FAKE_BBI_PATH <- "/tmp/fake/bbi"

    # should fail because path doesn't exist
    expect_error(check_bbi_exe(FAKE_BBI_PATH))

    # should pass
    expect_invisible(check_bbi_exe(BBI_EXE_PATH))
  })


  test_that("bbi_init creates babylon.yaml", {
    # create yaml

      withr::with_options(list(rbabylon.bbi_exe_path = '/data/apps/bbi'), {
        bbi_init(".", ".", .no_default_version=TRUE)
      })

    # read in yaml and check that it has a babylon key
    bbi_yaml <- yaml::read_yaml("babylon.yaml")
    expect_true("babylon_binary" %in% names(bbi_yaml))

    # delete yaml
    fs::file_delete("babylon.yaml")

  })

  test_that("bbi_init errors with invalid .nonmem_version", {
    # fails if don't specify anything
    expect_error(bbi_init(".", "."), regexp = "Must specify a `.nonmem_version`")

    # fails if what you specify isn't in the babylon.yaml (i.e. isn't a valid NONMEM installation)
    if (Sys.getenv("METWORX_VERSION") == "") {
      skip("bbi_init only runs on Metworx")
    } else {
      withr::with_options(list(rbabylon.bbi_exe_path = '/data/apps/bbi'), {
        expect_error(bbi_init(".", ".", "naw"), regexp = "Must specify a valid `.nonmem_version`")
        fs::file_delete("babylon.yaml")
      })
    }
  })

}
