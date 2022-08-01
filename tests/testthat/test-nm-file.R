
test_that("nm_file() works: model object [BBR-NMF-001]", {
  .d <- nm_file(MOD1, ".cov")
  expect_equal(ncol(.d), MOD1_PARAM_COUNT+1)
  expect_equal(nrow(.d), MOD1_PARAM_COUNT)
})

test_that("nm_file() works: summary object [BBR-NMF-001]", {
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
  skip_if_not_drone_or_metworx("test-model-summary")

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
      fs::file_delete(file.path(tempdir(),"basic","1", "1.ctl"))

      fs::file_copy(system.file("extdata", "acop.csv", package = "bbr"), file.path(tempdir(), "basic"))


      mod1 <- new_model(file.path(tempdir(), "basic", "1"), .overwrite = TRUE)
      fs::dir_copy(system.file("model", "nonmem", "basic", "1", package = "bbr"), file.path(tempdir(), "basic"))

      ctl_file <- read_lines(file.path(tempdir(), "basic", "1.ctl")) %>% stringr::str_replace("../../../../extdata/acop.csv", "acop.csv" )
      write_lines(ctl_file, file.path(tempdir(), "basic", "1.ctl"))

      bbi <- jsonlite::read_json(file.path(tempdir(), "basic", "1", "bbi_config.json"))
      bbi$data_path <- "/acop.csv"
      jsonlite::write_json(bbi, file.path(tempdir(), "basic", "1", "bbi_config.json"))
      fs::file_move(file.path(tempdir(), "basic", "acop.csv"), file.path(tempdir(), "basic", "1", "acop.csv"))
      fs::file_delete(file.path(tempdir(), "basic","1", "1.ctl"))

      # Generates Example Data
      set.seed(123)
      test_row <- sample(1.5:20, size = 52, replace = TRUE)
      test_row <- matrix(unlist(test_row), ncol = 52, nrow = 1) %>% as_tibble()
      colnames(test_row) <- c("C","NUM","STUDYN","ID","TIME","TAD","TAFD","EVID","AMT","RATE", "BLQF",
                              "ALQF","QNSF", "DV", "LDV", "DISID","FORM", "AGE" ,"SEX","RACE","WTT","WTBL"  ,
                              "EGFRBL", "AS401TBL", "PiADAC34","MDV","TADN","DOSE" ,"LDOS"  ,"DOSEN" ,"dur","DOSEGRP", "CYCLEN",
                              "DAYN","VISITN"  , "PCDY", "PCTPTNUM", "EXIDTFL"  ,"EXIINTFL" ,"HPNC1D1","HIMC1D1", "MHPC1D1","HPNC1D35",
                              "HIMC1D35","MHPC1D35", "CMT" ,"SUBJ", "VISITC", "EPOCH","CYCDAY" ,"PCTPT" ,"DATETIME")
      test_row$ID <- 1

      #Generates Table File
      tab <- "TABLE NO.  1
 ID          TIME        TAD         AMT         RATE        EVID        DV          IPRED       CWRESI      NPDE        V           CL          F1          LOGTVCL     ETA1        HFLF        ADAna       ADA1        ADA8        ADA8        ADA800      ADA8K       ADA80K      ADA800K     ADA8M       AS401TBL    DV          PRED        RES         WRES
  1.0000E+00  0.0000E+00  0.0000E+00  1.0200E+03  4.0800E+03  1.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  6.1779E+00  1.8436E+01  8.1040E-02  1.3000E+00  1.3097E-01  2.3222E-01  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  1.0000E+00  0.0000E+00  0.0000E+00  0.0000E+00  8.0000E+03  0.0000E+00         NaN  0.0000E+00  0.0000E+00


"
      write_file(tab, file.path(tempdir(), "basic", "1", "1.tab"))

      readr::write_csv(test_row, file.path(tempdir(), "basic", "1","acop.csv"))


      res <- capture_warning(nm_join(mod1, .files = "1.tab", .join_col = "ID"))

      expect_equal(res$message, '1.tab had the following duplicated columns: ADA8, DV\nDuplicate names will be repaired with `make.unique()`')

    }
    )
  })
})



