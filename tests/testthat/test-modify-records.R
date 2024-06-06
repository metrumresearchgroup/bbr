context("Modify records functions")

withr::local_options(list(
  bbr.bbi_exe_path = read_bbi_path(),
  bbr.verbose = TRUE
))

# Note: not all functions in modify-records.R are tested (i.e. soft wrappers)
describe("Modify record helpers", {

  mod_complex <- read_model(file.path(MODEL_DIR_X, "example2_saemimp"))
  mod_complex2 <- read_model(file.path(MODEL_DIR_X, "iovmm"))
  mod_complex3 <- read_model(file.path(MODEL_DIR_X, "acop-onlysim"))

  it("mod_has_record: targeted checks for specific records", {
    # Model 1
    expect_true(mod_has_record(MOD1, "theta"))
    expect_true(mod_has_record(MOD1, "omega"))
    expect_true(mod_has_record(MOD1, "sigma"))
    expect_true(mod_has_record(MOD1, "pk"))
    expect_true(mod_has_record(MOD1, "est"))
    expect_true(mod_has_record(MOD1, "error"))
    expect_true(mod_has_record(MOD1, "table"))

    expect_false(mod_has_record(MOD1, "omegapd"))
    expect_false(mod_has_record(MOD1, "thetapv"))
    expect_false(mod_has_record(MOD1, "prior"))

    # Model 2
    expect_true(mod_has_record(mod_complex, "prior"))
    expect_true(mod_has_record(mod_complex, "subroutine"))
    expect_true(mod_has_record(mod_complex, "cov"))

    expect_false(mod_has_record(mod_complex, "table"))
    expect_false(mod_has_record(mod_complex, "omegapd"))
    expect_false(mod_has_record(mod_complex, "thetapv"))

    # Model 3
    expect_true(mod_has_record(mod_complex2, "mix"))

    # Model 4
    expect_true(mod_has_record(mod_complex3, "sim"))
  })

  it("remove_records", {
    mod2 <- copy_model_from(MOD1)
    on.exit(delete_models(mod2, .tags = NULL, .force = TRUE))
    records_rm <- c(
      "estimation", "covariance", "theta", "omega", "sigma",  "error", "pk", "table"
    )
    purrr::walk(records_rm, function(rec){
      expect_true(mod_has_record(mod2, type = rec))
    })

    purrr::walk(records_rm, function(rec){
      remove_records(mod2, type = rec)
    })

    purrr::walk(records_rm, function(rec){
      expect_false(mod_has_record(mod2, type = rec))
    })
  })

  it("add_new_record", {
    mod2 <- copy_model_from(MOD1)
    on.exit(delete_models(mod2, .tags = NULL, .force = TRUE))
    expect_true(length(get_records(mod2, "table")) == 6)
    table_lines <- glue("ONEHEADER NUM DV CL FILE=newtab.tab")
    # Added after COV record (i.e. the first table)
    add_new_record(mod2, "table", lines = table_lines, after = "cov")
    table_recs <- get_records(mod2, "table")
    expect_true(length(table_recs) == 7)
    expect_equal(
      table_recs[[1]]$format(), paste0("$TABLE ",table_lines, "\n\n")
    )
    # Added to the end of the file
    add_new_record(mod2, "table", lines = table_lines, after = NULL)
    table_recs <- get_records(mod2, "table")
    expect_true(length(table_recs) == 8)
    expect_equal(
      table_recs[[1]]$format(), table_recs[[8]]$format()
    )

    # Add blank record
    add_new_record(mod2, "table", lines = NULL, after = NULL)
    table_recs <- get_records(mod2, "table")
    expect_true(length(table_recs) == 9)
    expect_equal(table_recs[[9]]$format(), "$TABLE \n\n")
  })

  it("modify_data_path_ctl works", {
    mod2 <- copy_model_from(MOD1)
    on.exit(delete_models(mod2, .tags = NULL, .force = TRUE))

    new_data_path <- "some/path/data.csv"
    modify_data_path_ctl(mod2, new_data_path)
    expect_equal(
      get_data_path(mod2, .check_exists = FALSE),
      file.path(fs::path_real(MODEL_DIR), new_data_path)
    )
  })

  it("modify_prob_statement works", {
    mod2 <- copy_model_from(MOD1)
    on.exit(delete_models(mod2, .tags = NULL, .force = TRUE))

    # Test fetching of problem statement
    expect_equal(
      modify_prob_statement(mod2, prob_text = NULL),
      "From bbr: see 2.yaml for details"
    )

    # Test setting a new problem statement
    new_prob <- "new prob statement"
    modify_prob_statement(mod2, new_prob)
    expect_equal(
      modify_prob_statement(mod2, prob_text = NULL),
      new_prob
    )
  })

  it("get_input_columns works", {
    mod2 <- copy_model_from(MOD1)
    on.exit(delete_models(mod2, .tags = NULL, .force = TRUE))

    input_cols_data <- get_input_columns(mod2)
    input_cols_ctl <- get_input_columns(mod2, from_data = FALSE)
    expect_equal(unname(input_cols_ctl), input_cols_data)
    expect_equal(input_cols_data, names(nm_data(mod2)))

    # Case where control stream has renaming (renames are returned as names of the vector)
    input_cols_ctl <- get_input_columns(mod_complex, from_data = FALSE)
    expect_equal(setdiff(names(input_cols_ctl), input_cols_ctl), c("DV", "AMT"))
  })

  it("get_table_columns works", {
    mod2 <- copy_model_from(MOD1, "2")
    fs::dir_copy(MOD1[[ABS_MOD_PATH]], file.path(MODEL_DIR, "2"))
    on.exit(delete_models(mod2, .tags = NULL, .force = TRUE))

    tab_cols_data <- get_table_columns(mod2)
    tab_cols_ctl <- get_table_columns(mod2, from_data = FALSE)

    # These tables should return identical columns
    purrr::walk2(
      tab_cols_data[3:length(tab_cols_data)],
      tab_cols_ctl[3:length(tab_cols_ctl)],
      function(tab_data, tab_ctl) expect_equal(tab_data, tab_ctl)
    )

    # Difference caused by _lack of_ `NOAPPEND` flag --> other cols get added
    expect_true(rlang::is_empty(setdiff(tab_cols_ctl[[1]], tab_cols_data[[1]])))
    expect_equal(setdiff(tab_cols_data[[1]], tab_cols_ctl[[1]]), c("DV", "PRED", "RES", "WRES"))

    # Difference caused by selecting columns via a formula
    expect_equal(setdiff(tab_cols_ctl[[2]], tab_cols_data[[2]]), "ETAS(1:LAST)")
    expect_equal(setdiff(tab_cols_data[[2]], tab_cols_ctl[[2]]), c("ETA1", "ETA2"))
  })
})
