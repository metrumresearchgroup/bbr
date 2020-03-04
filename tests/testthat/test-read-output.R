context("Reading NONMEM output files into R")

# load reference objects
source("data/read-output-reference.R")


test_that("check_file returns correctly", {
  # default is to print and return nothing
  null_output <- capture.output(
    expect_invisible(check_file(LST_TEST_FILE))
  )

  # check with .return=T
  res <- check_file(LST_TEST_FILE, .print = FALSE, .return = TRUE)
  expect_identical(res, LST_REF_DEFAULT)
})

#######################################
# check heads and tails functionality
#######################################

# check_file() tests
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
  test_that(glue::glue("check_file head={.tc[['head_test']]} tail={.tc[['tail_test']]}"), {
    res <- check_file(LST_TEST_FILE, .print = FALSE, .return = TRUE, .head = .tc[['head_test']], .tail = .tc[['tail_test']])
    expect_identical(res, .tc[['ref']])
  })
}

# check OUTPUT
withr::with_file(OUTPUT_FILE, {
  readr::write_lines(LST_FULL_VEC, OUTPUT_FILE)

  .test_cases <- list(
    list(.test_arg = OUTPUT_DIR, .test_name = "tail_output() character dir"),
    list(.test_arg = OUTPUT_FILE, .test_name = "tail_output() character file"),
    list(.test_arg = RES1, .test_name = "tail_output() res object")
  )
  for (.tc in .test_cases) {
    test_that(.tc[[".test_name"]], {
      res <- tail_output(.tc[[".test_arg"]], .print = FALSE, .return = TRUE)
      expect_identical(res, LST_REF_DEFAULT)
    })
  }
})

# check .lst
.test_cases <- list(
  list(.test_arg = OUTPUT_DIR, .test_name = "tail_lst() character dir"),
  list(.test_arg = LST_TEST_FILE, .test_name = "tail_lst() character file"),
  list(.test_arg = RES1, .test_name = "tail_lst() res object")
)
for (.tc in .test_cases) {
  test_that(.tc[[".test_name"]], {
    res <- tail_lst(.tc[[".test_arg"]], .print = FALSE, .return = TRUE)
    expect_identical(res, LST_REF_DEFAULT)
  })
}


#######################################
# check output directory functionality
#######################################

# test regular output functionality
.test_cases <- list(
  list(.test_arg = OUTPUT_DIR, .test_name = "check_output_dir() character dir"),
  list(.test_arg = RES1, .test_name = "check_output_dir() res object")
)
for (.tc in .test_cases) {
  test_that(.tc[[".test_name"]], {
    res <- check_output_dir(.tc[[".test_arg"]])
    expect_identical(basename(res), basename(OUTPUT_DIR_LS))
  })
}

# test output functionality with filter
.test_cases <- list(
  list(.test_arg = OUTPUT_DIR, .test_name = "check_output_dir() character dir with filter"),
  list(.test_arg = RES1, .test_name = "check_output_dir() res object with filter")
)
for (.tc in .test_cases) {
  test_that(.tc[[".test_name"]], {
    res <- check_output_dir(.tc[[".test_arg"]], .filter = CTL_FILTER)
    expect_identical(basename(res), basename(CTL_FILTER_RES))
  })
}


#######################################
# check table functionality
#######################################

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

# test check_ext
.test_cases <- list(
  list(.test_arg = OUTPUT_DIR, .test_name = "check_ext() character dir default .iter_floor"),
  list(.test_arg = RES1, .test_name = "check_ext() res object default .iter_floor")
)
for (.tc in .test_cases) {
  test_that(.tc[[".test_name"]], {
    df <- check_ext(.tc[[".test_arg"]])
    ref_df <- readRDS(EXT_REF_FLOOR_0)
    expect_identical(df, ref_df)

    ref_df <- readRDS(EXT_REF_FLOOR_NULL)
    expect_failure(expect_identical(df, ref_df))
  })
}

.test_cases <- list(
  list(.test_arg = OUTPUT_DIR, .test_name = "check_ext() character dir default .iter_floor NULL"),
  list(.test_arg = RES1, .test_name = "check_ext() res object .iter_floor NULL")
)
for (.tc in .test_cases) {
  test_that(.tc[[".test_name"]], {
    df <- check_ext(.tc[[".test_arg"]], .iter_floor = NULL)
    ref_df <- readRDS(EXT_REF_FLOOR_NULL)
    expect_identical(df, ref_df)

    ref_df <- readRDS(EXT_REF_FLOOR_0)
    expect_failure(expect_identical(df, ref_df))
  })
}

# test check_grd
.test_cases <- list(
  list(.test_arg = OUTPUT_DIR, .test_name = "check_grd() character dir default .iter_floor"),
  list(.test_arg = RES1, .test_name = "check_grd() res object default .iter_floor")
)
for (.tc in .test_cases) {
  test_that(.tc[[".test_name"]], {
    df <- check_grd(.tc[[".test_arg"]])
    ref_df <- readRDS(GRD_REF_FLOOR_0)
    expect_identical(df, ref_df)

    ref_df <- readRDS(GRD_REF_FLOOR_10)
    expect_failure(expect_identical(df, ref_df))

    ref_df <- readRDS(GRD_REF_FLOOR_NULL)
    expect_failure(expect_identical(df, ref_df))
  })
}

.test_cases <- list(
  list(.test_arg = OUTPUT_DIR, .test_name = "check_grd() character dir .iter_floor 10"),
  list(.test_arg = RES1, .test_name = "check_grd() res object .iter_floor 10")
)
for (.tc in .test_cases) {
  test_that(.tc[[".test_name"]], {
    df <- check_grd(.tc[[".test_arg"]], .iter_floor = 10)
    ref_df <- readRDS(GRD_REF_FLOOR_10)
    expect_identical(df, ref_df)

    ref_df <- readRDS(GRD_REF_FLOOR_0)
    expect_failure(expect_identical(df, ref_df))

    ref_df <- readRDS(GRD_REF_FLOOR_NULL)
    expect_failure(expect_identical(df, ref_df))
  })
}

.test_cases <- list(
  list(.test_arg = OUTPUT_DIR, .test_name = "check_grd() character dir .iter_floor NULL"),
  list(.test_arg = RES1, .test_name = "check_grd() res object .iter_floor NULL")
)
for (.tc in .test_cases) {
  test_that(.tc[[".test_name"]], {
    df <- check_grd(.tc[[".test_arg"]], .iter_floor = NULL)
    ref_df <- readRDS(GRD_REF_FLOOR_NULL)
    expect_identical(df, ref_df)

    ref_df <- readRDS(GRD_REF_FLOOR_0)
    expect_failure(expect_identical(df, ref_df))

    ref_df <- readRDS(GRD_REF_FLOOR_10)
    expect_failure(expect_identical(df, ref_df))
  })
}

