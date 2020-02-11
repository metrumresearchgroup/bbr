context("Reading NONMEM output files into R")

test_that("check_nonmem_output_file() output matches ref df", {
  df <- check_nonmem_output_file("data/acop.ext")
  ref_df <- readRDS("data/acop_ext_ref_floor0_200211.rds")
  expect_identical(df, ref_df)
})

test_that("check_nonmem_output_file(.iter_floor=NULL) works", {
  df <- check_nonmem_output_file("data/acop.ext", .iter_floor = NULL)
  ref_df <- readRDS("data/acop_ext_ref_floorNULL_200211.rds")
  expect_identical(df, ref_df)
})
