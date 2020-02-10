context("Utility functions for building args, etc.")

test_that("check_nonmem_args parses correctly", {
  # check some that should parse correctly
  .arg_list <- list(
    list(list("json" = T, "threads" = 4, "nm_version" = "nm74"), c("--json", "--threads=4", "--nmVersion=nm74")), # check `nm_version` converted to `--nmVersion`
    list(list("json" = T, "threads" = 4, debug=F), c("--json", "--threads=4")), # check bool=F not passed through
    list(list("json" = T, "threads" = 4, debug=T), c("--json", "--threads=4", "--debug"))  # check same bool=T is passed through
  )

  for (.a in .arg_list) {
    expect_identical(check_nonmem_args(.a[[1]]), .a[[2]])
  }

  # check some that should error
  .arg_list <- list(
    list("json" = T, "threads" = 4, "json" = F), # need to pass unique keys (json passed twice)
    list("json" = T, "threads" = 4, "naw" = "naw"), # trying to pass a key that doesn't exist in NONMEM_ARGS
    list("json" = T, "threads" = 4, "debug" = "naw") # passing the wrong type of value (char instead of bool)
  )

  for (.a in .arg_list) {
    expect_error(check_nonmem_args(.a))
  }
})


test_that("format_cmd_args parses correctly", {
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


test_that("parse_mod_yaml() returns expected list", {
  expect_identical(parse_mod_yaml("data/modtest.yaml"),
                   list(
                     model_path = "inst/nonmem/acop.mod",
                     description = "acop model for testing",
                     based_on = NULL,
                     tags = c("acop tag", "other tag"),
                     bbi_args = list(
                                    overwrite = TRUE,
                                    threads = 4L,
                                    nm_version = "nm74gf")
                    )
                   )

  expect_error(parse_mod_yaml("data/modtest_no_modpath.yaml"))
})


test_that("parse_args_list() merges lists as expected", {
  .func_args <- list(naw=4, paw=6)
  .yaml_args <- list(naw=T, saw="hey")

  # override `naw` from .func_args
  expect_identical(parse_args_list(.func_args, .yaml_args), list(naw=T, paw=6, saw="hey"))

  # correctly handles nulls
  expect_identical(parse_args_list(NULL, .yaml_args), .yaml_args)
  expect_identical(parse_args_list(.func_args, NULL), .func_args)
  expect_identical(parse_args_list(NULL, NULL), NULL)

  # correctly fails if .func_args isn't named
  expect_error(parse_args_list(list(4,5,6), .yaml_args))

})


test_that("print_nonmem_args() doesn't error", {
  expect_invisible(print_nonmem_args())
})
