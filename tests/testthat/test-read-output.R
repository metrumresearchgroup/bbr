context("Reading NONMEM output files into R")

test_that("check_nonmem_table_output() output matches ref df", {
  df <- check_nonmem_table_output("data/acop.ext", .x_var = "ITERATION")
  ref_df <- readRDS("data/acop_ext_ref_floorNULL_200211.rds")
  expect_identical(df, ref_df)
})

test_that("check_nonmem_table_output(.x_floor=0) works", {
  df <- check_nonmem_table_output("data/acop.ext", .x_var = "ITERATION", .x_floor = 0)
  ref_df <- readRDS("data/acop_ext_ref_floor0_200211.rds")
  expect_identical(df, ref_df)
})


