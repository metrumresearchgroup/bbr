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
    translate_nm_expr(test_exprs, type = 'accept') %>% names(),
    c("SEX==1", "ID==2", "WT!=70", "AGE!=30", "A==1", "WT>40", "B<=20")
  )

  expect_equal(
    translate_nm_expr(test_exprs, type = 'ignore') %>% names(),
    c("SEX!=1", "ID!=2", "WT==70", "AGE==30", "A!=1", "WT<40", "B>=20")
  )


  # Use of `@`, `#`, or form `IGNORE=C2` require `data_cols` to be specified
  # - only `data_cols[1]` is technically needed
  data_cols <- c("C", "ID", "TIME", "EVID", "DV", "BLQ")
  expect_equal(
    translate_nm_expr("#", data_cols = data_cols) %>% unname(),
    paste0("!grepl('^#', ", data_cols[1], ")")
  )

  expect_equal(
    translate_nm_expr("C", data_cols = data_cols) %>% names(),
    paste0(data_cols[1], "!='C'")
  )

  # Extra `\\` is added for escape purposes when the expression is later parsed
  expect_equal(
    translate_nm_expr("@", data_cols = data_cols) %>% unname(),
    paste0("!grepl('^\\\\s*[A-Za-z@]', ", data_cols[1], ")")
  )

  # Using accept with these filters errors out
  expect_error(
    translate_nm_expr("#", type = "accept", data_cols = data_cols),
    "only supported for IGNORE"
  )
  expect_error(
    translate_nm_expr("@", type = "accept", data_cols = data_cols),
    "only supported for IGNORE"
  )
  expect_error(
    translate_nm_expr("C", type = "accept", data_cols = data_cols),
    "ACCEPT option only supports"
  )

  # Error out for unsupported logical operators
  test_exprs_bad <- c(test_exprs, "GEN=1 .AND. AGE > 60")
  expect_error(
    translate_nm_expr(test_exprs_bad, type = 'accept'),
    "The following logical operators are not supported"
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

  ## Check expected expressions ##
  input_data <- nm_data(mod2) %>% suppressMessages()
  nm_exprs <- get_data_filter_exprs(mod2)
  r_filters <- translate_nm_expr(
    nm_expr = nm_exprs$exprs, type = nm_exprs$type, data_cols = names(input_data)
  )

  # Check filters and names
  # - names: simplified version of filter (no NA handling) or key symbol (#, @)
  #   stored as the name for traceability and testing purposes.
  # - value: the actual filter expression supplied to the final filter
  expect_equal(names(r_filters), c("@", "ID!=2", "SEX!=1", "WT>=50"))
  expect_equal(
    unname(r_filters),
    c("!grepl('^\\\\s*[A-Za-z@]', ID)", "(ID!=2 | is.na(ID))",
      "(SEX!=1 | is.na(SEX))", "(WT>=50 | is.na(WT))")
    )

  # Check that filter expression works correctly
  filter_expression <- paste(r_filters, collapse = " & ")
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

  # Remove filters
  data_rec$values[[5]]$value <- NULL
  data_rec$values[[7]]$value <- NULL
  nmrec::write_ctl(ctl, get_model_path(mod2))

  # Expect identical to `nm_data()` (minus the attribute)
  filtered_data <- filter_nm_data(mod2)
  expect_equal(attributes(filtered_data)$n_records_dropped, 0)
  expect_true(all.equal(nm_data(mod2), filtered_data, check.attributes = FALSE))
})


test_that("filter_nm_data() errors if expressions cant be parsed", {
  mod2 <- copy_model_from(MOD1, "2")
  on.exit(delete_models(mod2, .force = TRUE, .tags = NULL))

  # Add additional IGNORE expressions and compare to dplyr filters
  ctl <- get_model_ctl(mod2)
  data_rec <- nmrec::select_records(ctl, "data")[[1]]
  data_rec$parse()

  # Set filter that should error
  data_rec$values[[7]]$value <- "(ID.EQ.2X)"
  nmrec::write_ctl(ctl, get_model_path(mod2))

  expect_error(
    filter_nm_data(mod2),
    "ignore/accept list could not be converted to filters"
  )
})


test_that("filter_nm_data() works when NA values are present", {
  mod2 <- copy_model_from(MOD1, "2")
  on.exit(delete_models(mod2, .force = TRUE, .tags = NULL))

  # Add additional IGNORE expressions and compare to dplyr filters
  ctl <- get_model_ctl(mod2)
  data_rec <- nmrec::select_records(ctl, "data")[[1]]
  data_rec$parse()

  # Include a filter for a column with NA values
  data_rec$values[[7]]$value <- "(C='C', BLQ=1)"
  nmrec::write_ctl(ctl, get_model_path(mod2))

  # Test 1
  # - C column does *not* lead to loss of subjects due to NA values
  # - 1 subject lost due to BLQ.
  data <- nm_data(mod2)
  data_test1 <- data %>% dplyr::mutate(C = NA, BLQ = 0)
  data_test1$BLQ[nrow(data_test1)] <- 1

  data_f1 <- filter_nm_data(mod2, data = data_test1)
  expect_equal(nrow(data) - 1, nrow(data_f1))

  # Test 2
  # - filter works appropriately when found strings and NA values are present
  # - 1 subject lost due to BLQ. 1 subject lost due to 'C' filter
  data_test2 <- data_test1
  data_test2$C[1] <- "C"
  data_test2$C[2] <- "."
  data_test2$BLQ[3] <- NA

  data_f2 <- filter_nm_data(mod2, data = data_test2)
  expect_equal(nrow(data) - 2, nrow(data_f2))
  expect_true(is.na(data_f2$BLQ[2]))
  expect_equal(data_f2$C[1], ".")
})
