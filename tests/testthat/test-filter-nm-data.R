
test_that("translate_nm_expr() translates NONMEM filter expressions", {
  test_exprs <- "SEX==1, ID.EQ.2, WT/=70, AGE.NE.30, A=1, WT.GT.40"

  expect_equal(
    translate_nm_expr(test_exprs, type = 'accept'),
    "SEX==1 & ID==2 & WT!=70 & AGE!=30 & A==1 & WT>40"
  )

  expect_equal(
    translate_nm_expr(test_exprs, type = 'ignore'),
    "SEX!=1 & ID!=2 & WT==70 & AGE==30 & A!=1 & WT<40"
  )


  # Use of `@`, `#`, or form `IGNORE=C2` require `data_cols` to be specified
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
    paste0("!grepl('^[A-Za-z@]', ", data_cols, ")", collapse = " & ")
  )

})

