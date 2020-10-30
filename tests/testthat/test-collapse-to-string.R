context("Collapse columns to string representation")

setup({
  cleanup()
  create_rlg_models()
})

teardown({ cleanup() })

test_that("collapse_to_string() works correctly", {
  # add a note to collapse
  mod2 <- read_model(file.path(MODEL_DIR, 2)) %>% add_notes(NEW_NOTES)
  on.exit(replace_notes(mod2, NULL))

  log_df <- run_log(MODEL_DIR)
  expect_true(inherits(log_df[[YAML_TAGS]], "list"))
  expect_true(inherits(log_df[[YAML_NOTES]], "list"))

  log_df <- log_df %>%
    collapse_to_string({{YAML_TAGS}}, {{YAML_NOTES}})

  expect_identical(
    log_df[[YAML_TAGS]],
    c(
      paste(ORIG_TAGS, collapse = ", "),
      paste(NEW_TAGS, collapse = ", "),
      paste(ORIG_TAGS, collapse = ", ")
    )
  )

  expect_identical(
    log_df[[YAML_NOTES]],
    c(
      NA,
      paste(NEW_NOTES, collapse = ", "),
      NA
    )
  )
})

test_that("collapse_to_string() warns correctly", {
  log_df <- run_log(MODEL_DIR) %>%
    collapse_to_string({{YAML_TAGS}})

  expect_warning(
    collapse_to_string(log_df, {{YAML_TAGS}}),
    regexp = "The following columns are not lists and will be ignored: tags"
  )
})

test_that("collapse_to_string() errors correctly", {
  log_df <- run_log(MODEL_DIR) %>%
    collapse_to_string({{YAML_TAGS}})

  expect_error(
    collapse_to_string(log_df, bags),
    class = "vctrs_error_subscript_oob"
  )
})

test_that("collapse_to_string() leaves list of lists untouched", {
  log_df <- run_log(MODEL_DIR)
  orig_args <- log_df[[YAML_BBI_ARGS]]

  log_df <- log_df %>%
    collapse_to_string({{YAML_TAGS}}, {{YAML_BBI_ARGS}})

  expect_identical(orig_args, log_df[[YAML_BBI_ARGS]])
})
