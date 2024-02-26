
test_that("nm_file() works: model object [BBR-NMF-001]", {
  .d <- nm_file(MOD1, ".cov")
  expect_equal(ncol(.d), MOD1_PARAM_COUNT+1)
  expect_equal(nrow(.d), MOD1_PARAM_COUNT)
})

test_that("nm_file() works: summary object [BBR-NMF-001]", {
  skip_if_not_drone_or_metworx("nm_file() summary object")
  .d <- nm_file(SUM1, ".cov")
  expect_equal(ncol(.d), MOD1_PARAM_COUNT+1)
  expect_equal(nrow(.d), MOD1_PARAM_COUNT)
})

test_that("nm_file() works: file path [BBR-NMF-001]", {
  .d <- nm_file(build_path_from_model(MOD1, ".cov"))
  expect_equal(ncol(.d), MOD1_PARAM_COUNT+1)
  expect_equal(nrow(.d), MOD1_PARAM_COUNT)
})

test_that("nm_grd() works [BBR-NMF-003]", {
  skip_if_not_drone_or_metworx("nm_file() summary object")
  withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {
    .d <- nm_grd(MOD1)
    expect_equal(ncol(.d), MOD1_PARAM_COUNT_FIXED+1)
    expect_true(nrow(.d) > 5) # this changes enough, not worth testing exactly
    expect_true(all(
      names(.d)[1] == "ITERATION",
      stringr::str_detect(names(.d)[2:ncol(.d)], "THETA|SIGMA|OMEGA")
    ))
  })
})

test_that("nm_grd() works: .rename=FALSE [BBR-NMF-003]", {
  .d <- nm_grd(MOD1, .rename = FALSE)
  expect_equal(ncol(.d), MOD1_PARAM_COUNT_FIXED+1)
  expect_true(nrow(.d) > 5) # this changes enough, not worth testing exactly
  expect_true(all(
    names(.d)[1] == "ITERATION",
    stringr::str_detect(names(.d)[2:ncol(.d)], "GRD")
  ))
})

test_that("nm_file() with multiple tables warns and returns NULL [BBR-NMF-004]", {
  .m <- read_model(file.path(MODEL_DIR_X,"example2_saemimp"))
  expect_warning(
    .d <- nm_file(.m, ".ext"),
    regexp = "does not support files with multiple tables"
  )
  expect_null(.d)
})

test_that("nm_file() with multiple tables swallows fread cleanup warning [BBR-NMF-004]", {
  .m <- read_model(file.path(MODEL_DIR_X,"example2_saemimp"))
  expect_warning(
    .d <- nm_file(.m, ".ext"),
    regexp = "does not support files with multiple tables"
  )
  expect_null(.d)

  # run again on a different file to check that fread() _doesn't_ raise cleanup warning
  .d <- nm_file(MOD1, ".cov")
  expect_equal(ncol(.d), MOD1_PARAM_COUNT+1)
  expect_equal(nrow(.d), MOD1_PARAM_COUNT)

  # run again to see that other warnings come through
  .ff <- build_path_from_model(MOD1, ".fake")
  readr::write_lines("TABLE NO. 1\na b\n1 2 3\n", .ff)
  withr::defer(if(fs::file_exists(.ff)) fs::file_delete(.ff))
  expect_warning(
    .d <- nm_file(.ff),
    regexp = "Detected 2 column names but the data has 3 columns"
  )
  expect_equal(ncol(.d), 3)
  expect_equal(nrow(.d), 1)
})



test_that("nm_data() works [BBR-NMF-005]", {
  expect_message({
    .d <- nm_data(MOD1)
  }, regexp = "Reading.+acop")

  expect_equal(ncol(.d), DATA_TEST_COLS)
  expect_equal(nrow(.d), DATA_TEST_ROWS)
})

test_that("nm_data() works without needing to submit the model", {
  clean_test_enviroment(create_rlg_models)
  mod <- read_model(NEW_MOD3)

  expect_message({
    .d <- nm_data(mod)
  }, regexp = "Reading.+acop")

  expect_equal(ncol(.d), DATA_TEST_COLS)
  expect_equal(nrow(.d), DATA_TEST_ROWS)
})

test_that("nm_tab() works [BBR-NMF-006]", {
  .d <- nm_tab(MOD1)
  expect_equal(ncol(.d), 8)
  expect_equal(nrow(.d), DATA_TEST_ROWS_IGNORE)
})

test_that("nm_par_tab() works [BBR-NMF-007]", {
  .d <- nm_par_tab(MOD1)
  expect_equal(ncol(.d), 6)
  expect_equal(nrow(.d), DATA_TEST_ROWS_IGNORE)
})

test_that("nm-file has handling for  duplicate columns [BBR-NMF-008]", {
  withr::with_tempdir({
    withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {

      on.exit(if(file.path(tempdir(),"basic") %>% dir.exists()) fs::dir_delete(file.path(tempdir(),"basic") ))

      fs::dir_copy(system.file("model", "nonmem", "basic", package = "bbr"), tempdir())

      ctl_file <- read_file(file.path(tempdir(), "basic", "1.ctl"))

      ctl_file <- str_replace(ctl_file,"CWRES", "CWRES CWRES")

      write_file(ctl_file, file.path(tempdir(), "basic", "1.ctl"))

      read_lines(file.path(tempdir(),"basic","1", "1.tab")) %>%
        stringr::str_replace("DV", "CWRES") %>%
        write_lines(file.path(tempdir(),"basic","1", "1.tab"))

      expect_warning((nm_file(read_model(file.path(tempdir(), "basic", "1")), .suffix = ".tab")),
                     'Duplicate names will be repaired')

    }
    )
  })
})

test_that("nm_file() preserves column casing [BBR-NMF-009]", {
  cov_path <- build_path_from_model(MOD1, ".cov")
  tmp_path <- tempfile()
  withr::defer({
    unlink(tmp_path)
  })

  # lowercase one of the column names
  cov_lines <- readr::read_lines(cov_path) %>% stringr::str_replace("NAME", "name")
  readr::write_lines(cov_lines, tmp_path)

  .d <- nm_file(tmp_path)
  expect_equal(ncol(.d), MOD1_PARAM_COUNT+1)
  expect_equal(nrow(.d), MOD1_PARAM_COUNT)
  expect_equal(names(.d)[1], "name")
})

