context("rbabylon exec functions")

test_that("check_status_code works as expected", {
  # nothing happens on status 0
  expect_equal(check_status_code(0, "stdout...", c("arg1", "arg2")), NULL)

  # other codes error
  expect_error(check_status_code(1, "stdout...", c("arg1", "arg2")))
  expect_error(check_status_code(-1, "stdout...", c("arg1", "arg2")))
  expect_error(check_status_code(225, "stdout...", c("arg1", "arg2")))
})

test_that("check_bbi_exe() correctly errors or finds paths", {
  FAKE_BBI_PATH <- "/tmp/fake/bbi"

  # should fail because path doesn't exist
  expect_error(check_bbi_exe(FAKE_BBI_PATH))

  # should pass because ping should exist
  expect_invisible(check_bbi_exe("ping"))
})


test_that("bbi_init creates babylon.yaml", {
  # create yaml
  if (Sys.getenv("METWORX_VERSION") == "") {
    skip("bbi_init only runs on Metworx")
  } else {
    withr::with_options(list(rbabylon.bbi_exe_path = '/data/apps/bbi'), {
      bbi_init(".", ".", .no_default_version=TRUE)
    })
  }

  # read in yaml and check that it has a babylon key
  bbi_yaml <- yaml::read_yaml("babylon.yaml")
  expect_true("babylon_binary" %in% names(bbi_yaml))

  # delete yaml
  fs::file_delete("babylon.yaml")

})

test_that("bbi_init errors with invalid .nonmem_version", {
  # create yaml
  expect_error(bbi_init(".", "."))
  expect_error(bbi_init(".", ".", "naw"))

  # delete yaml
  fs::file_delete("babylon.yaml")
})
