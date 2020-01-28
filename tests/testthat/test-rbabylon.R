context("rbabylon exec functions")

test_that("check_status_code works as expected", {
  # nothing happens on status 0
  expect_equal(check_status_code(list(status=0)), NULL)

  # other codes error
  expect_error(check_status_code(list(status=1)))
  expect_error(check_status_code(list(status=-1)))
  expect_error(check_status_code(list(status=225)))
})
