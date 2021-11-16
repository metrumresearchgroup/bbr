context("Reading NONMEM output files into R")

withr::local_options(list(lifecycle_verbosity = "quiet"))

################################################################################################
# load reference character vectors for checking the contents of output files
################################################################################################

# .lst file
LST_FULL_VEC <- readLines(LST_TEST_FILE)
LST_STEM <- file.path(REF_DIR, "read-output-refs", "1_lst_ref_")

LST_REF_DEFAULT <- readLines(paste0(LST_STEM, "default.txt"))
LST_REF_0_5 <- readLines(paste0(LST_STEM, "0_5.txt"))
LST_REF_5_0 <- readLines(paste0(LST_STEM, "5_0.txt"))
LST_REF_1_5 <- readLines(paste0(LST_STEM, "1_5.txt"))
LST_REF_5_1 <- readLines(paste0(LST_STEM, "5_1.txt"))

# directory ls stuff
OUTPUT_DIR_LS <- fs::dir_ls(OUTPUT_DIR)
CTL_FILTER <- ".ctl"
CTL_FILTER_RES <- as.character(grep(CTL_FILTER, OUTPUT_DIR_LS, value = TRUE))

# table output
EXT_STEM <- file.path(REF_DIR, "read-output-refs", "1_ext_ref_")
EXT_REF_FLOOR_0 <- paste0(EXT_STEM, "floor0.R")
EXT_REF_FLOOR_NULL <- paste0(EXT_STEM, "floorNULL.R")

GRD_STEM <- file.path(REF_DIR, "read-output-refs", "1_grd_ref_")
GRD_REF_FLOOR_0 <- paste0(GRD_STEM, "floor0.R")
GRD_REF_FLOOR_10 <- paste0(GRD_STEM, "floor10.R")
GRD_REF_FLOOR_NULL <- paste0(GRD_STEM, "floorNULL.R")

################
# tests
################

test_that("check_file returns correctly [BBR-ROT-001]", {
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
  test_that(glue::glue("check_file head={.tc[['head_test']]} tail={.tc[['tail_test']]} [BBR-ROT-002]"), {
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
    list(.test_arg = MOD1, .test_name = "tail_output() model object")
  )
  for (.tc in .test_cases) {
    test_that(paste(.tc[[".test_name"]], "[BBR-ROT-003]"), {
      res <- tail_output(.tc[[".test_arg"]], .print = FALSE, .return = TRUE)
      expect_identical(res, LST_REF_DEFAULT)
    })
  }
})

test_that("tail_output() doesn't error with finished NONMEM run [BBR-ROT-003]", {
  expect_message(tail_output(MOD1), regexp = "already finished running")
})

# check .lst
.test_cases <- list(
  list(.test_arg = OUTPUT_DIR, .test_name = "tail_lst() character dir"),
  list(.test_arg = LST_TEST_FILE, .test_name = "tail_lst() character file"),
  list(.test_arg = MOD1, .test_name = "tail_lst() model object")
)
for (.tc in .test_cases) {
  test_that(paste(.tc[[".test_name"]], "[BBR-ROT-004]"), {
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
  list(.test_arg = MOD1, .test_name = "check_output_dir() model object")
)
for (.tc in .test_cases) {
  test_that(paste(.tc[[".test_name"]], "[BBR-ROT-005]"), {
    res <- check_output_dir(.tc[[".test_arg"]])
    expect_identical(basename(res), basename(OUTPUT_DIR_LS))
  })
}

# test output functionality with regexpr passed through to dir_ls
.test_cases <- list(
  list(.test_arg = OUTPUT_DIR, .test_name = "check_output_dir() character dir with filter"),
  list(.test_arg = MOD1, .test_name = "check_output_dir() model object with filter")
)
for (.tc in .test_cases) {
  test_that(paste(.tc[[".test_name"]], "[BBR-ROT-006]"), {
    res <- check_output_dir(.tc[[".test_arg"]], regexp = CTL_FILTER)
    expect_identical(basename(res), basename(CTL_FILTER_RES))
  })
}


#######################################
# check table functionality
#######################################

test_that("check_nonmem_table_output() output matches ref df [BBR-ROT-007]", {
  df <- check_nonmem_table_output(file.path(MOD1_PATH, paste0(MOD_ID, ".ext")), .x_var = "ITERATION")
  ref_df <- dget(EXT_REF_FLOOR_NULL)

  # This mutate call is because of bug in testthat::expect_equal
  # we can remove this when we switch to testthat 3e
  df    <-      df %>% mutate(OBJ = round(OBJ, 5))
  ref_df <- ref_df %>% mutate(OBJ = round(OBJ, 5))
  expect_equal(df, ref_df)

})

test_that("check_nonmem_table_output(.x_floor=0) works [BBR-ROT-008]", {
  df <- check_nonmem_table_output(file.path(MOD1_PATH, paste0(MOD_ID, ".ext")), .x_var = "ITERATION", .x_floor = 0)
  ref_df <- dget(EXT_REF_FLOOR_0)

  # This mutate call is because of bug in testthat::expect_equal
  # we can remove this when we switch to testthat 3e
  df    <-      df %>% mutate(OBJ = round(OBJ, 5))
  ref_df <- ref_df %>% mutate(OBJ = round(OBJ, 5))
  expect_equal(df, ref_df)
})

# test check_ext
.test_cases <- list(
  list(.test_arg = OUTPUT_DIR, .test_name = "check_ext() character dir default .iter_floor"),
  list(.test_arg = MOD1, .test_name = "check_ext() model object default .iter_floor")
)
for (.tc in .test_cases) {
  test_that(paste(.tc[[".test_name"]], "[BBR-ROT-009]"), {
    df <- check_ext(.tc[[".test_arg"]])
    ref_df <- dget(EXT_REF_FLOOR_0)

    # This mutate call is because of bug in testthat::expect_equal
    # we can remove this when we switch to testthat 3e
    df    <-      df %>% mutate(OBJ = round(OBJ, 5))
    ref_df <- ref_df %>% mutate(OBJ = round(OBJ, 5))
    expect_equal(df, ref_df)

    ref_df <- dget(EXT_REF_FLOOR_NULL)
    expect_failure(expect_equal(df, ref_df))
  })
}

test_that("check_ext() summary object [BBR-ROT-010]", {
  skip_if_not_drone_or_metworx("check_ext() summary object")
  df <- check_ext(SUM1)
  ref_df <- dget(EXT_REF_FLOOR_0)

  # This mutate call is because of bug in testthat::expect_equal
  # we can remove this when we switch to testthat 3e
  df    <-      df %>% mutate(OBJ = round(OBJ, 5))
  ref_df <- ref_df %>% mutate(OBJ = round(OBJ, 5))
  expect_equal(df, ref_df)
})

.test_cases <- list(
  list(.test_arg = OUTPUT_DIR, .test_name = "check_ext() character dir default .iter_floor NULL"),
  list(.test_arg = MOD1, .test_name = "check_ext() model object .iter_floor NULL")
)
for (.tc in .test_cases) {
  test_that(paste(.tc[[".test_name"]], "[BBR-ROT-011]"), {
    df <- check_ext(.tc[[".test_arg"]], .iter_floor = NULL)
    ref_df <- dget(EXT_REF_FLOOR_NULL)

    # This mutate call is because of bug in testthat::expect_equal
    # we can remove this when we switch to testthat 3e
    df    <-      df %>% mutate(OBJ = round(OBJ, 5))
    ref_df <- ref_df %>% mutate(OBJ = round(OBJ, 5))
    expect_equal(df, ref_df)

    ref_df <- dget(EXT_REF_FLOOR_0)
    expect_failure(expect_equal(df, ref_df))
  })
}

# test check_grd
.test_cases <- list(
  list(.test_arg = OUTPUT_DIR, .test_name = "check_grd() character dir default .iter_floor"),
  list(.test_arg = MOD1, .test_name = "check_grd() model object default .iter_floor")
)
for (.tc in .test_cases) {
  test_that(paste(.tc[[".test_name"]], "[BBR-ROT-012]"), {
    df <- check_grd(.tc[[".test_arg"]])
    ref_df <- dget(GRD_REF_FLOOR_0)
    expect_equal(df, ref_df)

    ref_df <- dget(GRD_REF_FLOOR_10)
    expect_failure(expect_equal(df, ref_df))

    ref_df <- dget(GRD_REF_FLOOR_NULL)
    expect_failure(expect_equal(df, ref_df))
  })
}

test_that("check_grd() summary object [BBR-ROT-013]", {
  skip_if_not_drone_or_metworx("check_grd() summary object")
  df <- check_grd(SUM1)
  ref_df <- dget(GRD_REF_FLOOR_0)
  expect_equal(df, ref_df)
})

.test_cases <- list(
  list(.test_arg = OUTPUT_DIR, .test_name = "check_grd() character dir .iter_floor 10"),
  list(.test_arg = MOD1, .test_name = "check_grd() model object .iter_floor 10")
)
for (.tc in .test_cases) {
  test_that(paste(.tc[[".test_name"]], "[BBR-ROT-014]"), {
    df <- check_grd(.tc[[".test_arg"]], .iter_floor = 10)
    ref_df <- dget(GRD_REF_FLOOR_10)
    expect_equal(df, ref_df)

    ref_df <- dget(GRD_REF_FLOOR_0)
    expect_failure(expect_equal(df, ref_df))

    ref_df <- dget(GRD_REF_FLOOR_NULL)
    expect_failure(expect_equal(df, ref_df))
  })
}

.test_cases <- list(
  list(.test_arg = OUTPUT_DIR, .test_name = "check_grd() character dir .iter_floor NULL"),
  list(.test_arg = MOD1, .test_name = "check_grd() model object .iter_floor NULL")
)
for (.tc in .test_cases) {
  test_that(paste(.tc[[".test_name"]], "[BBR-ROT-015]"), {
    df <- check_grd(.tc[[".test_arg"]], .iter_floor = NULL)
    ref_df <- dget(GRD_REF_FLOOR_NULL)
    expect_equal(df, ref_df)

    ref_df <- dget(GRD_REF_FLOOR_0)
    expect_failure(expect_equal(df, ref_df))

    ref_df <- dget(GRD_REF_FLOOR_10)
    expect_failure(expect_equal(df, ref_df))
  })
}
