context("bbr exec functions")

# constants
BBI_EXE_PATH <- read_bbi_path()

test_that("check_status_code works as expected", {
  # nothing happens on status 0
  expect_equal(check_status_code(0, "stdout...", c("arg1", "arg2")), NULL)

  # other codes error
  expect_error(check_status_code(1, "stdout...", c("arg1", "arg2")))
  expect_error(check_status_code(-1, "stdout...", c("arg1", "arg2")))
  expect_error(check_status_code(225, "stdout...", c("arg1", "arg2")))
})

test_that("bbi_dry_run() correctly returns object", {
  PROC_CLASS_LIST <- c("bbi_process", "list")
  cmd_args <- c("naw", "dawg")
  dir <- "fake/dir"

  # run dry run
  res <- bbi_dry_run(cmd_args, dir)

  # compare to expected
  expect_identical(res[[PROC_PROCESS]], "DRY_RUN")
  expect_identical(res[[PROC_STDOUT]], "DRY_RUN")
  expect_identical(res[[PROC_BBI]], getOption("bbr.bbi_exe_path"))
  expect_identical(res[[PROC_CMD_ARGS]], cmd_args)
  expect_identical(res[[PROC_WD]], dir)
  expect_identical(class(res), PROC_CLASS_LIST)
})


skip_if_not_drone_or_metworx("bbi_init")

test_that("check_bbi_exe() correctly errors or finds paths", {
  FAKE_BBI_PATH <- "/tmp/fake/bbi"

  # should fail because path doesn't exist
  expect_error(check_bbi_exe(FAKE_BBI_PATH))

  # should pass
  expect_invisible(check_bbi_exe(BBI_EXE_PATH))
})

test_that("check_bbi_exe() errors on too low version", {
  skip_if_over_rate_limit()
  skip_if(getOption("rbabylon.DEV_no_min_version"))

  withr::with_options(list("bbr.bbi_min_version" = package_version("100.0.0")), {
    # should fail because version number is less than 100.0.0
    CACHE_ENV$bbi_exe_paths[[BBI_EXE_PATH]] <- NULL
    expect_error(check_bbi_exe(BBI_EXE_PATH), regexp = "minimum supported version of bbi is 100\\.0\\.0")
    expect_null(CACHE_ENV$bbi_exe_paths[[BBI_EXE_PATH]])
  })
})

test_that("bbi_init creates bbi.yaml", {
  # create yaml

  withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {
    bbi_init(".", ".", .no_default_version=TRUE)
  })

  # read in yaml and check that it has a bbi key
  bbi_yaml <- yaml::read_yaml("bbi.yaml")
  expect_true("bbi_binary" %in% names(bbi_yaml))

  # delete yaml
  fs::file_delete("bbi.yaml")

})

test_that("bbi_init errors with non-existent .dir", {
  expect_error(bbi_init("naw", "."), regexp = "Cannot find.+naw")
})

test_that("bbi_init errors with invalid .nonmem_version", {
  # fails if don't specify anything
  expect_error(bbi_init(".", "."), regexp = "Must specify a `.nonmem_version`")

  # fails if what you specify isn't in the bbi.yaml (i.e. isn't a valid NONMEM installation)
  withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {
    expect_error(bbi_init(".", ".", "naw"), regexp = "Must specify a valid `.nonmem_version`")
    fs::file_delete("bbi.yaml")
  })
})
