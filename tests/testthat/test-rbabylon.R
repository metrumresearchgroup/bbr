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
  bbi_init(".", ".")

  # read in yaml and check that it has a babylon key
  bbi_yaml <- yaml::read_yaml("babylon.yaml")
  expect_true("babylonbinary" %in% names(bbi_yaml))

  # delete yaml
  fs::file_delete("babylon.yaml")

})
