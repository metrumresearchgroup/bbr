context("test parsing labels for parameter table")

library(glue)

# constants
source("data/test-matrix-indices-construction-ref.R")
MODEL_DIR <- "../../inst/extdata"
MODEL_PICKS <- c("101", "510", "510_fixed")

# should we build up any tests cases for weird ways to make comments?
# in order to do unit tests for `parse_param_comment()`
# the functionality is tested as part of the `param_labels()` tests, but not explicitly as a unit test


for (.tc in names(MAT_REF)) {
  test_that(glue("build_matrix_indices() parses correctly for {.tc}"), {
    ref_df <- MAT_REF[[.tc]]

    test_ind <- build_matrix_indices(ref_df$is_diag)
    expect_equal(test_ind, ref_df$ref)
  })
}


# test param_labels against tidynm reference tibbles
if (Sys.getenv("METWORX_VERSION") == "" && Sys.getenv("DRONE") != "true") {
  skip("param_labels only runs on Metworx or Drone")
} else {
  withr::with_options(list(rbabylon.bbi_exe_path = '/data/apps/bbi',
                           rbabylon.model_directory = MODEL_DIR)) {

    bbi_init(MODEL_DIR, "/opt/NONMEM", "nm74gf")

    for (MODEL_PICK in MODEL_PICKS) {
      test_that(glue("param_labels matches tidynm reference for {MODEL_PICK}"), {
        # get reference df from tidynm test data
        ref_df <- readRDS(glue("data/{MODEL_PICK}_PARAMTBL.rds"))[[1]]

        # get param df with rbabylon::model_summary()
        if (fs::file_exists(file.path(MODEL_DIR, glue("{MODEL_PICK}.yaml")))) fs::file_delete(file.path(MODEL_DIR, glue("{MODEL_PICK}.yaml")))
        .mod <-  rbabylon::new_model(glue("{MODEL_PICK}.yaml"), glue("the {MODEL_PICK} model"))
        .param_df <- .mod %>% rbabylon::model_summary() %>% param_estimates()

        # make label df
        .label_df <- .mod %>% param_labels()

        # join to constuct full parameter table
        .new_df <- inner_join(.param_df,
                              .label_df %>% apply_indices() %>% select(-param_type))

        # join against reference to make sure they're the same
        names(.new_df) <- toupper(names(.new_df))
        join_df <- ref_df %>%
          full_join(.new_df, by = c("LABEL", "UNIT", "TYPE")) %>%
          select(PARAM, NAMES, LABEL, UNIT, TYPE, value, ESTIMATE, se, STDERR)

        # tests
        expect_equal(nrow(.new_df), nrow(ref_df))
        expect_equal(nrow(.new_df), nrow(.label_df))

        # cleanup
        if (fs::file_exists(file.path(MODEL_DIR, glue("{MODEL_PICK}.yaml")))) fs::file_delete(file.path(MODEL_DIR, glue("{MODEL_PICK}.yaml")))
      })
    }
    if (fs::file_exists(file.path(MODEL_DIR, "babylon.yaml"))) fs::file_delete(file.path(MODEL_DIR, "babylon.yaml"))
  }
}
