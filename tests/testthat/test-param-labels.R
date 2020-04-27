context("test parsing labels for parameter table")

suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(dplyr))

# constants
source("data/test-param-labels-ref.R")
MODEL_DIR <- "data/tidynm_extdata"
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
  test_that(glue("build_matrix_indices() parses correctly for {.tc}"), {
    ref_df <- MAT_REF[[.tc]]

    test_ind <- build_matrix_indices(ref_df$is_diag)
    expect_equal(test_ind, ref_df$ref)
  })
}


for (i in length(BLOCK_REF)) {
  test_that(glue("block() parses correctly {i}"), {
    expect_equal(block(i), BLOCK_REF[[i]])
  })
}


# test param_labels against tidynm reference tibbles
if (Sys.getenv("METWORX_VERSION") == "" && Sys.getenv("DRONE") != "true") {
  skip("param_labels only runs on Metworx or Drone")
} else {
  withr::with_options(list(rbabylon.bbi_exe_path = '/data/apps/bbi',
                           rbabylon.model_directory = MODEL_DIR), {

    for (MODEL_PICK in MODEL_PICKS) {
      .mod_id <- MODEL_PICK$mod_id

      test_that(glue("param_labels() %>% apply_indices() matches tidynm reference for {.mod_id}"), {
        # get reference df from tidynm test data
        ref_df <- readRDS(glue("data/{.mod_id}_PARAMTBL.rds"))[[1]]
        names(ref_df) <- names(ref_df) %>% tolower()

        # get param df with rbabylon::model_summary()
        if (fs::file_exists(file.path(MODEL_DIR, glue("{.mod_id}.yaml")))) fs::file_delete(file.path(MODEL_DIR, glue("{.mod_id}.yaml")))
        .mod <-  rbabylon::new_model(glue("{.mod_id}.yaml"), glue("the {.mod_id} model"))
        .param_df <- .mod %>% rbabylon::model_summary() %>% param_estimates()
        names(.param_df) <- names(.param_df) %>% tolower()

        # make label df
        .label_df <- .mod %>% param_labels()

        # join to constuct full parameter table
        .new_df <- inner_join(.param_df,
                              .label_df %>% apply_indices(.omega = MODEL_PICK$omega, .sigma = MODEL_PICK$sigma) %>% select(-param_type),
                              by = "names")

        # join against reference to make sure they're the same
        ref_df <- ref_df %>% mutate(names = ifelse(param == "THETA",
                                                   paste0(param, var1),
                                                   paste0(param, "(", var1, ",", var2, ")")))

        join_df <- ref_df %>%
          full_join(.new_df, by = names(.label_df))

        # tests
        expect_equal(nrow(join_df), nrow(ref_df))
        expect_equal(nrow(join_df), nrow(.new_df))
        expect_equal(join_df$value, join_df$estimate, tolerance = 0.01)
        expect_equal(join_df$se, join_df$stderr, tolerance = 0.01)
        expect_equal(join_df$c, join_df$random_effect_sd, tolerance = 0.01)
        expect_equal(join_df$cse, join_df$random_effect_sdse, tolerance = 0.01)

        # cleanup
        if (fs::file_exists(file.path(MODEL_DIR, glue("{.mod_id}.yaml")))) fs::file_delete(file.path(MODEL_DIR, glue("{.mod_id}.yaml")))
      })
    }
  })
}
