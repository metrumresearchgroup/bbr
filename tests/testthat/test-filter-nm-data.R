test_that("translate_nm_operator translates NONMEM operators", {
  # label.EQN.value and label.NEN.value are supported after NONMEM 7.3
  nm_r_translations <- list(
    equal = c("A.EQ.1", "A.EQN.1", "A==1", "A=1"),
    not_equal = c("B.NE.1", "B.NEN.1", "B/=1"),
    greater_than = c("C.GE.1", "C.GT.1"),
    less_than = c("D.LE.1", "D.LT.1")
  )

  expect_equal(unique(translate_nm_operator(nm_r_translations$equal)), "A==1")
  expect_equal(unique(translate_nm_operator(nm_r_translations$not_equal)), "B!=1")
  expect_equal(translate_nm_operator(nm_r_translations$greater_than), c("C>=1", "C>1"))
  expect_equal(translate_nm_operator(nm_r_translations$less_than), c("D<=1", "D<1"))
})

test_that("translate_nm_expr() translates NONMEM filter expressions", {
  test_exprs <- c("SEX==1", "ID.EQ.2", "WT/=70", "AGE.NE.30", "A=1", "WT.GT.40", "B.LE.20")

  expect_equal(
    translate_nm_expr(test_exprs, type = 'accept'),
    c("SEX==1", "ID==2", "WT!=70", "AGE!=30", "A==1", "WT>40", "B<=20")
  )

  expect_equal(
    translate_nm_expr(test_exprs, type = 'ignore'),
    c("SEX!=1", "ID!=2", "WT==70", "AGE==30", "A!=1", "WT<40", "B>=20")
  )


  # Use of `@`, `#`, or form `IGNORE=C2` require `data_cols` to be specified
  # - only `data_cols[1]` is technically needed
  data_cols <- c("C", "ID", "TIME", "EVID", "DV", "BLQ")
  expect_equal(
    translate_nm_expr("#", data_cols = data_cols),
    paste0("!grepl('^#', ", data_cols[1], ")")
  )

  expect_equal(
    translate_nm_expr("c2", data_cols = data_cols),
    paste0(data_cols[1], "!='c2'")
  )

  expect_equal(
    translate_nm_expr("@", data_cols = data_cols),
    paste0("!grepl('^[A-Za-z@]', ", data_cols[1], ")")
  )
})

test_that("filter_nm_data() filters input data using IGNORE/ACCEPT options", {

  filtered_data <- filter_nm_data(MOD1)

  # Check expected filters
  expect_equal(attributes(filtered_data)$n_records_dropped, 20)
  expect_equal(nrow(filtered_data), DATA_TEST_ROWS_IGNORE)

  # Check that nm_data(.mod, filter=TRUE) works the same way
  expect_equal(nrow(nm_data(MOD1, filter = TRUE)), DATA_TEST_ROWS_IGNORE)

  mod2 <- copy_model_from(MOD1, "2")
  on.exit(delete_models(mod2, .force = TRUE, .tags = NULL))

  # Add additional IGNORE expressions and compare to dplyr filters
  ctl <- get_model_ctl(mod2)
  data_rec <- nmrec::select_records(ctl, "data")[[1]]
  data_rec$parse()
  data_rec$values[[7]]$value <- "(ID.EQ.2, SEX=1, WT.LE.50)"
  nmrec::write_ctl(ctl, get_model_path(mod2))

  # Check expected expressions
  input_data <- nm_data(mod2) %>% suppressMessages()
  nm_exprs <- get_data_filter_exprs(mod2)
  r_filters <- translate_nm_expr(
    nm_expr = nm_exprs$exprs, type = nm_exprs$type, data_cols = names(input_data)
  )
  filter_expression <- paste(r_filters, collapse = " & ")
  expect_equal(
    filter_expression, "!grepl('^[A-Za-z@]', ID) & ID!=2 & SEX!=1 & WT>=50"
  )

  # Check that filter expression works correctly
  filtered_data <- filter_nm_data(mod2)
  expect_equal(
    nrow(filtered_data),
    input_data %>% dplyr::filter(eval(parse(text = filter_expression))) %>% nrow()
  )

  # Check that relevant rows have been filtered out
  removed_records <- invert_operator(r_filters[2])
  expect_equal(
    filtered_data %>% dplyr::filter(eval(parse(text = removed_records))) %>% nrow(),
    0
  )
})

test_that("filter_nm_data() errors out when using both IGNORE and ACCEPT options", {
  mod2 <- copy_model_from(MOD1, "2")
  on.exit(delete_models(mod2, .force = TRUE, .tags = NULL))

  # Add additional IGNORE expressions and compare to dplyr filters
  ctl <- get_model_ctl(mod2)
  data_rec <- nmrec::select_records(ctl, "data")[[1]]
  data_rec$parse()

  # Create new ACCEPT option
  nmrec::set_record_option(data_rec, "accept", "(SEX.EQ.1)")
  nmrec::write_ctl(ctl, get_model_path(mod2))

  expect_error(
    filter_nm_data(mod2),
    "ACCEPT list and IGNORE list may not both be used"
  )
})

test_that("filter_nm_data() works with no filters", {
  mod2 <- copy_model_from(MOD1, "2")
  on.exit(delete_models(mod2, .force = TRUE, .tags = NULL))

  # Add additional IGNORE expressions and compare to dplyr filters
  ctl <- get_model_ctl(mod2)
  data_rec <- nmrec::select_records(ctl, "data")[[1]]
  data_rec$parse()

  # Create new ACCEPT option
  data_rec$values[[5]]$value <- NULL
  data_rec$values[[7]]$value <- NULL
  nmrec::write_ctl(ctl, get_model_path(mod2))

  # Expect identical to `nm_data()` (minus the attribute)
  filtered_data <- filter_nm_data(mod2)
  expect_equal(attributes(filtered_data)$n_records_dropped, 0)
  expect_true(all.equal(nm_data(mod2), filtered_data, check.attributes = FALSE))
})
