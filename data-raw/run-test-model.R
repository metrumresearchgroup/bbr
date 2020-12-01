library(dplyr)
devtools::load_all() # can't call library(rbabylon) from within rbabylon when using pkgr

###################
# FUNCTION DEF
###################

#' Helper to re-run models used for vignettes and testing
#'
#' Note: will overwrite any output directory for this model.
#' @param .model_dir Path to directory model will be run in, relative to project/package root
#' @param .model_name Name of model file, without extension
#' @param .summary If `TRUE`, the default, will also save the summary object (with `dput()`) to `inst/test-refs/{.model_name}_summary_obj.R`.
#' @param .param_table If `TRUE`, the default, will also save the summary object (with `dput()`) to `inst/test-refs/{.model_name}_param_table.R`.
#'   Only checked if `.summary` is also `TRUE`.
run_test_model <- function(
  .model_dir,
  .model_name,
  .summary = TRUE,
  .param_table = TRUE
) {

  .model_dir <- file.path(rprojroot::find_rstudio_root_file(), .model_dir)
  .ref_out_dir <- system.file('test-refs', package = 'rbabylon')

  withr::with_options(list(rbabylon.bbi_exe_path = read_bbi_path()), {
    bbi_init(.dir = .model_dir,            # the directory to create the babylon.yaml in
             .nonmem_dir = "/opt/NONMEM", # location of NONMEM installation
             .nonmem_version = "nm74gf")  # default NONMEM version to use
    withr::defer(fs::file_delete(file.path(.model_dir, "babylon.yaml")))

    mod_path <- file.path(.model_dir, .model_name)

    message(paste("Running", mod_path))
    .mod <- read_model(file.path(.model_dir, .model_name))
    .proc <- .mod %>% submit_model(.mode = "local", .bbi_args = list(overwrite = TRUE))

    if (isTRUE(.summary)) {
      message("  writing out summary object...")
      .sum <- model_summary(.mod)
      sum_out_path <- file.path(
        .ref_out_dir,
        as.character(glue::glue("{.model_name}_summary_obj.R"))
      )
      dput(.sum, file = sum_out_path)

      if (isTRUE(.param_table)) {
        message("  writing out parameter table...")
        param_df <- param_estimates(.sum)
        param_out_path <- file.path(
          .ref_out_dir,
          as.character(glue::glue("{.model_name}_param_table.R"))
        )
        dput(param_df, file = param_out_path)
      }
    }

    message("  Done.")
  })
}


###################
# CALLS
###################

# To re-run models, source this script.
run_test_model("inst/model/nonmem/basic", 1)
