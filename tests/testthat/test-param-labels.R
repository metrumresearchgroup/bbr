context("test parsing labels for parameter table")

# constants
PL_MODEL_DIR <- file.path(REF_DIR, "param-labels", "tidynm_extdata")
MODEL_PICKS <- list(
  list(mod_id = "101", omega = block(3), sigma = NULL),
  list(mod_id = "510", omega = block(2), sigma = NULL),
  list(mod_id = "510_fixed", omega = NULL, sigma = NULL)
)


### should we build up any tests cases for weird ways to make comments?
### in order to do unit tests for `parse_param_comment()`
### the functionality is tested as part of the `param_labels()` tests, but not explicitly as a unit test
# test_that("parse_param_comment() parses correctly ...", {...})

for (.tc in names(MAT_REF)) {
  # Remove "ref_" because all names start with "ref_" and mrgvalprep will keep
  # the common parts, leading to a hard-to-parse test description.
  label <- stringr::str_remove(.tc, "ref_")
  test_that(glue::glue("build_matrix_indices() parses correctly: {label} [BBR-PLB-001]"), {
    ref_df <- MAT_REF[[.tc]]

    test_ind <- build_matrix_indices(ref_df$is_diag)
    expect_equal(test_ind, ref_df$ref)
  })
}


for (i in seq_along(BLOCK_REF)) {
  test_that(glue::glue("block() parses correctly {i} [BBR-PLB-002]"), {
    expect_equal(block(i), BLOCK_REF[[i]])
  })
}


test_that("param_labels.character errors on vector [BBR-PLB-003]", {
  expect_error(param_labels(c("naw", "dawg")), regexp = "character scalar of the raw control stream")
})


# test parsing labels from different OMEGA and SIGMA blocks
for (.test_name in names(PARAM_BLOCK_REF)) {
  # Remove "PEX_" because all names start with "PEX_" and mrgvalprep will keep
  # the common parts, leading to a hard-to-parse test description.
  label <- stringr::str_remove(.tc, "PEX_")
  test_that(glue::glue("parse_param_comment() called internally: {label} [BBR-PLB-004]"), {
    .tc <- PARAM_BLOCK_REF[[.test_name]]
    res_df <- .tc$ctl %>% param_labels() %>% apply_indices(.omega = .tc$omega, .sigma = .tc$sigma)
    expect_equal(res_df, .tc$ref)
  })
}


# test param_labels against tidynm reference tibbles (testing character dispatch)
for (MODEL_PICK in MODEL_PICKS) {
  .mod_id <- MODEL_PICK$mod_id

  test_that(glue("param_labels.character() %>% apply_indices() matches tidynm reference {.mod_id} [BBR-PLB-005]"), {
    # get reference df from tidynm test data
    ref_df <- readRDS(file.path(REF_DIR, "param-labels", glue("{.mod_id}_PARAMTBL.rds")))[[1]]
    names(ref_df) <- names(ref_df) %>% tolower()

    #
    .ctl_raw <-  file.path(PL_MODEL_DIR, glue("{.mod_id}.ctl")) %>% readr::read_file()

    # make label df
    .label_df <- .ctl_raw %>%
      param_labels() %>%
      apply_indices(.omega = MODEL_PICK$omega, .sigma = MODEL_PICK$sigma) %>%
      select(-param_type)

    # join against reference to make sure they're the same
    ref_df <- ref_df %>% dplyr::mutate(
      !!SUMMARY_PARAM_NAMES := ifelse(param == "THETA",
                                      paste0(param, var1),
                                      paste0(param, "(", var1, ",", var2, ")")))

    join_df <- ref_df %>%
      dplyr::full_join(.label_df, by = names(.label_df))

    # test that the full join didn't add any rows (i.e. there are no mismatches)
    expect_equal(nrow(join_df), nrow(ref_df))
    expect_equal(nrow(join_df), nrow(.label_df))
  })
}


# test param_labels against tidynm reference tibbles (testing bbi_nonmem_model dispatch)
withr::with_options(list(bbr.bbi_exe_path = read_bbi_path()), {
  skip_if_not_ci_or_metworx("test-param-labels tidynm tests")

  for (MODEL_PICK in MODEL_PICKS) {
    .mod_id <- MODEL_PICK$mod_id

    test_that(glue("param_labels.bbi_nonmem_model() %>% apply_indices() matches tidynm reference {.mod_id} [BBR-PLB-006]"), {
      # get reference df from tidynm test data
      ref_df <- readRDS(file.path(REF_DIR, "param-labels", glue("{.mod_id}_PARAMTBL.rds")))[[1]]
      names(ref_df) <- names(ref_df) %>% tolower()

      # get param df with bbr::model_summary()
      if (fs::file_exists(file.path(PL_MODEL_DIR, glue("{.mod_id}.yaml")))) fs::file_delete(file.path(PL_MODEL_DIR, glue("{.mod_id}.yaml")))

      .mod <- new_model(file.path(PL_MODEL_DIR, .mod_id))

      on.exit({ if (fs::file_exists(file.path(PL_MODEL_DIR, glue("{.mod_id}.yaml")))) fs::file_delete(file.path(PL_MODEL_DIR, glue("{.mod_id}.yaml"))) })

      .param_df <- .mod %>% model_summary() %>% param_estimates()
      names(.param_df) <- names(.param_df) %>% tolower()

      # make label df
      .label_df <- .mod %>% param_labels()

      # join to constuct full parameter table
      suppressSpecificWarning({
        .new_df <- dplyr::inner_join(
          .param_df,
          .label_df %>%
            apply_indices(.omega = MODEL_PICK$omega, .sigma = MODEL_PICK$sigma) %>%
            dplyr::select(-param_type),
          by = SUMMARY_PARAM_NAMES)
      }, .regexpr = "Column .+ has different attributes on LHS and RHS of join")

      # join against reference to make sure they're the same
      ref_df <- ref_df %>% dplyr::mutate(
        !!SUMMARY_PARAM_NAMES := ifelse(param == "THETA",
                                        paste0(param, var1),
                                        paste0(param, "(", var1, ",", var2, ")")))

      join_df <- ref_df %>%
        dplyr::full_join(.new_df, by = names(.label_df))

      # tests
      expect_equal(nrow(join_df), nrow(ref_df))
      expect_equal(nrow(join_df), nrow(.new_df))
      expect_equal(join_df$value, join_df$estimate, tolerance = 0.01)
      expect_equal(join_df$se, join_df$stderr, tolerance = 0.01)
      expect_equal(join_df$c, join_df$random_effect_sd, tolerance = 0.01)
      expect_equal(join_df$cse, join_df$random_effect_sdse, tolerance = 0.01)
    })
  }
})
