context("Reading NONMEM output files into R")

# load reference objects
source("data/read-output-reference.R")


test_that("check_file returns correctly", {
  # default is to print and return nothing
  expect_invisible(check_file(LST_TEST_FILE))

  # check with .return=T
  res <- check_file(LST_TEST_FILE, .print = FALSE, .return = TRUE)
  expect_identical(res, LST_REF_DEFAULT)
})

# check heads and tails
.test_cases <- list(
  list(head_test = 0, tail_test = 5, ref = LST_REF_0_5),
  list(head_test = 5, tail_test = 0, ref = LST_REF_5_0),
  list(head_test = 1, tail_test = 5, ref = LST_REF_1_5),
  list(head_test = 5, tail_test = 1, ref = LST_REF_5_1),
  list(head_test = 10000000, tail_test = 0,    ref = LST_FULL_VEC),
  list(head_test = 0, tail_test = 10000000,    ref = LST_FULL_VEC),
  list(head_test = 100000, tail_test = 100000, ref = LST_FULL_VEC)
)
for (.tc in .test_cases) {
  print(glue::glue("check_file head={.tc[['head_test']]} tail={.tc[['tail_test']]}"))
  test_that(glue::glue("check_file head={.tc[['head_test']]} tail={.tc[['tail_test']]}"), {
    res <- check_file(LST_TEST_FILE, .print = FALSE, .return = TRUE, .head = .tc[['head_test']], .tail = .tc[['tail_test']])
    expect_identical(res, .tc[['ref']])
  })
}

check_file("model-examples/1/1.lst", .print = FALSE, .return = TRUE)


test_that("check_nonmem_table_output() output matches ref df", {
  df <- check_nonmem_table_output("model-examples/1/1.ext", .x_var = "ITERATION")
  ref_df <- readRDS("data/acop_ext_ref_floorNULL_200211.rds")
  expect_identical(df, ref_df)
})

test_that("check_nonmem_table_output(.x_floor=0) works", {
  df <- check_nonmem_table_output("model-examples/1/1.ext", .x_var = "ITERATION", .x_floor = 0)
  ref_df <- readRDS("data/acop_ext_ref_floor0_200211.rds")
  expect_identical(df, ref_df)
})


