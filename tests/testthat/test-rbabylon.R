context("rbabylon exec functions")

test_that("check_status_code works as expected", {
  # nothing happens on status 0
  expect_equal(check_status_code(list(status=0)), NULL)

  # other codes error
  expect_error(check_status_code(list(status=1)))
  expect_error(check_status_code(list(status=-1)))
  expect_error(check_status_code(list(status=225)))
})

test_that("check_bbi_exe() correctly errors or finds paths", {
  FAKE_BBI_PATH <- "/tmp/fake/bbi"

  # should fail because path doesn't exist
  expect_error(check_bbi_exe(FAKE_BBI_PATH))

  # should pass because ping should exist
  expect_invisible(check_bbi_exe("ping"))
})
