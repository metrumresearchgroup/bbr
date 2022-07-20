context("Utility functions for building args, etc.")

################
# parsing args
################

test_that("check_bbi_args parses correctly [BBR-UTL-001]", {
  # check some that should parse correctly
  .arg_list <- list(
    list(list("json" = T, "threads" = 4, "nm_version" = "nm74", "parallel" = T), c("--json", "--threads=4", "--nm_version=nm74", "--parallel")), # check flag conversion
    list(list("json" = T, "threads" = 4, debug=F, "parallel" = T), c("--json", "--threads=4","--parallel")), # check bool=F not passed through
    list(list("json" = T, "threads" = 4, debug=T, "parallel" = T), c("--json", "--threads=4", "--debug", "--parallel"))  # check same bool=T is passed through
  )

  for (.a in .arg_list) {
    expect_identical(check_bbi_args(.a[[1]]), .a[[2]])
  }

  # check some that should error
  .arg_list <- list(
    list("json" = T, "threads" = 4, "json" = F), # need to pass unique keys (json passed twice)
    list("json" = T, "threads" = 4, "naw" = "naw"), # trying to pass a key that doesn't exist in BBI_ARGS
    list("json" = T, "threads" = 4, "debug" = "naw") # passing the wrong type of value (char instead of bool)
  )

  for (.a in .arg_list) {
    expect_error(check_bbi_args(.a))
  }
})


test_that("format_cmd_args parses correctly [BBR-UTL-002]", {
  # check some that should parse correctly
  .arg_list <- list(
    list(list("json" = T, "threads" = 4), c("json", "threads=4")), # check basic nonmem args
    list(list("json" = T, "threads" = 4, "naw" = "naw"), c("json", "threads=4", "naw=naw")), # check one that's not a nonmem arg
    list(list("json" = T, "threads" = 4, debug=F), c("json", "threads=4")), # check bool=F not passed through
    list(list("json" = T, "threads" = 4, debug=T), c("json", "threads=4", "debug")) # check same bool=T is passed through
  )

  for (.a in .arg_list) {
    expect_identical(format_cmd_args(.a[[1]]), .a[[2]])
  }

  # check some that should error
  .arg_list <- list(
    list(T, 4), # need to pass named list
    list("json" = T, "threads" = 4, "json" = F) # need to pass unique keys (json passed twice)
  )

  for (.a in .arg_list) {
    expect_error(format_cmd_args(.a))
  }

  # check with .collapse=T
  .arg_list <- list(
    list(list("json" = T, "threads" = 4, "naw" = "naw"), "json threads=4 naw=naw"),
    list(list("--json" = T, "--threads" = 4, "--naw" = "naw"), "--json --threads=4 --naw=naw")
  )

  for (.a in .arg_list) {
    expect_identical(format_cmd_args(.a[[1]], .collapse=T), .a[[2]])
  }
})

#####################
# list manipulation
#####################

test_that("parse_args_list() merges lists as expected [BBR-UTL-003]", {
  # override `naw` with .func_args
  expect_identical(parse_args_list(.func_args = LIST1, .yaml_args = LIST2), list(naw=4, saw="hey", paw=6))
})

test_that("parse_args_list() handles NULL as expected [BBR-UTL-004]", {
  expect_identical(parse_args_list(NULL, LIST2), LIST2)
  expect_identical(parse_args_list(LIST1, NULL), LIST1)
  expect_identical(parse_args_list(NULL, NULL), list())
})

test_that("parse_args_list() correctly fails if .func_args isn't named [BBR-UTL-005]", {
  # correctly fails if .func_args isn't named
  expect_error(parse_args_list(list(4,5,6), LIST2))
})


test_that("combine_list_objects() merges lists as expected [BBR-UTL-006]", {
  expect_identical(combine_list_objects(.new_list = LIST1, .old_list = LIST2), list(naw=4, paw=6, saw="hey"))
})

test_that("combine_list_objects() merges with append=TRUE [BBR-UTL-007]", {
  expect_identical(combine_list_objects(.new_list = LIST1, .old_list = LIST2, .append = TRUE), list(naw=c(4, 5), paw=6, saw="hey"))
})

test_that("combine_list_objects() correctly fails if .func_args isn't named [BBR-UTL-008]", {
  # correctly fails if .func_args isn't named
  expect_error(combine_list_objects(list(4,5,6), LIST2))
  expect_error(combine_list_objects(LIST1, list(4,5,6)))
})

######################
# assorted utilities
######################

test_that("check_required_keys() works correctly [BBR-UTL-009]", {
  req_keys <- c("hey", "aww", "naw")
  expect_true(check_required_keys(list(hey = 1, aww = 2, naw = 3), req_keys))
  expect_false(check_required_keys(list(hey = 1, aww = 2), req_keys))
})


test_that("strict_mode_error() works correctly [BBR-UTL-010]", {
  withr::with_options(list(bbr.strict = TRUE), {
    expect_error(strict_mode_error("hello"))
  })
  withr::with_options(list(bbr.strict = FALSE), {
    expect_warning(strict_mode_error("hello"))
  })
  withr::with_options(list(bbr.strict = "oops"), {
    expect_warning(strict_mode_error("hello"))
  })
})

test_that("suppressSpecificWarning() works [BBR-UTL-011]", {
  # log() of a negative number raises a warning
  x <- suppressSpecificWarning(log(-1), "NaNs produced")
  expect_true(is.nan(x))
})

test_that("warning raised when threads > 1 and parallel is FALSE [BBR-UTL-014]", {
  withr::with_tempdir({
    fs::dir_create(tempdir(), "test_path")
    files_to_copy <- file.path(ABS_MODEL_DIR, c("1.ctl"))

    fs::file_copy(system.file("extdata", "acop.csv", package = "bbr"), file.path(tempdir(), "test_path"))
    purrr::walk(files_to_copy, fs::file_copy, file.path(tempdir(), "test_path"))
    fs::dir_copy(file.path(ABS_MODEL_DIR, "1"), file.path(tempdir(), "test_path"))
    ctl <- read_lines(file.path(tempdir(),"test_path", "1.ctl")) %>%  stringr::str_remove("../../../../extdata/")
    write_lines(ctl, file.path(tempdir(),"test_path", "1.ctl"))

    mod1 <- new_model(file.path(tempdir(), "test_path", "1"), .description = "original test-workflow-bbi model",
                      .tags = ORIG_TAGS,.bbi_args = list(overwrite = TRUE, threads = 2))

    bbi_init(file.path(tempdir(), "test_path" ), "/opt/NONMEM", "nm74gf")

    #Appends --parallel to bbi_args when submitted
    res <- capture.output(submit_model(mod1))

    #Testing that check_bbi_args is appending --parallel when not passed
    expect_identical("--parallel", str_subset(res,"--parallel") %>% str_extract("--parallel"))
    fs::file_delete(file.path(tempdir(), "test_path", "1.yaml"))


    mod1 <-  new_model(file.path(tempdir(), "test_path", "1"), .description = "original test-workflow-bbi model",
                       .tags = ORIG_TAGS,.bbi_args = list(overwrite = TRUE, threads = 2, parallel = FALSE))

    #When `threads` > 1 and parallel is False raise a warning
    expect_warning(submit_model(mod1))

  })
})



