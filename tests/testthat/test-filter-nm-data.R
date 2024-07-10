
test_that("translate_nm_expr() translates NONMEM filter expressions", {
  test_exprs <- "SEX==1, ID.EQ.2, WT/=70, AGE.NE.30, A=1, WT.GT.40, B.LE.20"

  expect_equal(
    translate_nm_expr(test_exprs, type = 'accept'),
    "SEX==1 & ID==2 & WT!=70 & AGE!=30 & A==1 & WT>40 & B<=20"
  )

  expect_equal(
    translate_nm_expr(test_exprs, type = 'ignore'),
    "SEX!=1 & ID!=2 & WT==70 & AGE==30 & A!=1 & WT<40 & B>=20"
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
  data_rec <- read_data_record(mod2)
  ignore_exprs <- get_data_filter_exprs(data_rec, type = "ignore")
  ignore_filters <- purrr::map_chr(ignore_exprs, translate_nm_expr, data_cols = names(input_data))
  filter_expression <- paste(ignore_filters, collapse = " & ")
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
  removed_records <- invert_operator(ignore_filters[2])
  expect_equal(
    filtered_data %>% dplyr::filter(eval(parse(text = removed_records))) %>% nrow(),
    0
  )
})
