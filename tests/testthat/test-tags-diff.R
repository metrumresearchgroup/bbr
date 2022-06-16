context("Comparing tags between models")

test_that("tags_diff.bbi_model default happy path works [BBR-TDF-001]", {
  new_mod <- copy_model_from(MOD1, "new_mod",
    .description = "tags_diff.bbi_model default happy path works",
    .inherit_tags = TRUE
  ) %>%
    replace_tag(ORIG_TAGS[1], NEW_TAGS[1])
  on.exit(cleanup_model(new_mod))

  diff_list <- tags_diff(new_mod, .print = FALSE)

  expect_identical(diff_list[[TAGS_ADD]], NEW_TAGS[1])
  expect_identical(diff_list[[TAGS_REM]], ORIG_TAGS[1])
})

test_that("tags_diff.bbi_model print works [BBR-TDF-002]", {
  new_mod <- copy_model_from(MOD1, "new_mod",
   .description = "tags_diff.bbi_model print works",
   .inherit_tags = TRUE
  ) %>%
    replace_tag(ORIG_TAGS[1], NEW_TAGS[1])
  on.exit(cleanup_model(new_mod))

  diff_print_output <- capture.output(tags_diff(new_mod))

  expect_equal(
    diff_print_output,
    c(
      glue("In new_mod but not parent(s):\t{NEW_TAGS[1]}"),
      glue("In parent(s) but not new_mod:\t{ORIG_TAGS[1]}")
    )
  )
})

test_that("tags_diff.bbi_model .mod2 works [BBR-TDF-003]", {
  temp_mod_path <- create_temp_model()
  new_mod <- temp_mod_path %>%
    read_model() %>%
    replace_all_tags(NEW_TAGS)

  diff_print_output <- capture.output(
    diff_list <- tags_diff(new_mod, .mod2 = MOD1)
  )

  expect_identical(diff_list[[TAGS_ADD]], NEW_TAGS)
  expect_identical(diff_list[[TAGS_REM]], ORIG_TAGS)
  expect_equal(
    diff_print_output,
    c(
      glue("In {get_model_id(new_mod)} but not {MOD_ID}:\t{paste(NEW_TAGS, collapse = ', ')}"),
      glue("In {MOD_ID} but not {get_model_id(new_mod)}:\t{paste(ORIG_TAGS, collapse = ', ')}")
    )
  )
})

test_that("tags_diff.bbi_run_log_df works [BBR-TDF-004]", {
  # set up models for run log
  cleanup()
  create_rlg_models()
  on.exit({
    rm(mod3)
    cleanup()
  })

  mod3 <- read_model(MOD3_ABS_PATH) %>%
    replace_tag(ORIG_TAGS[2], NEW_TAGS[1]) %>%
    add_tags("the newest")

  log_df <- run_log(MODEL_DIR)

  # test list returned from tags_diff
  DIFF_REF <- list(
    `1` = list(tags_added = ORIG_TAGS,    tags_removed = ""),
    `2` = list(tags_added = NEW_TAGS,     tags_removed = ORIG_TAGS),
    `3` = list(tags_added = "the newest", tags_removed = c(ORIG_TAGS[2], NEW_TAGS[2]))
  )

  expect_equal(tags_diff(log_df), DIFF_REF)

  # test adding to run log tibble
  log_df <- add_tags_diff(log_df)
  expect_equal(log_df$tags_added,   map(DIFF_REF, ~ .x$tags_added))
  expect_equal(log_df$tags_removed, map(DIFF_REF, ~ .x$tags_removed))

})

test_that("add_tags_diff() can append a new column with html formatting [BBR-TDF-005]", {
  cleanup()
  create_rlg_models()

  log_df <- run_log(MODEL_DIR)
  log_html <- add_tags_diff(log_df, .format = "html")
  expect_true("tags_diff" %in% names(log_html))
  expect_true(all(str_detect(log_html$tags_diff, "<s>") == c(FALSE, rep(TRUE, 5))))
  expect_true(all(str_detect(log_html$tags_diff, "</s>") == c(FALSE, rep(TRUE, 5))))

})

test_that("add_tags_diff() can collapse tag columns to string [BBR-TDF-006]", {
  cleanup()
  create_rlg_models()

  log_df <- run_log(MODEL_DIR)
  expect_false(inherits(log_df[[TAGS_ADD]], "character"))
  log_df <- add_tags_diff(log_df, .collapse = TRUE)
  expect_true(inherits(log_df[[TAGS_ADD]], "character"))

})
