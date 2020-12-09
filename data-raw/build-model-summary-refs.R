library(dplyr)
devtools::load_all() # can't call library(rbabylon) from within rbabylon when using pkgr

###################
# FUNCTION DEF
###################

#' Helper to re-build summary object and parameter table references
#'
#' Saves to `inst/test-refs/`.
#' @param .mod bbi_nonmem_model object
#' @param .bbi_path absolute path to bbi installation
build_model_summary_refs <- function(.mod, .bbi_path = read_bbi_path()) {

  model_name <- get_model_id(.mod)
  ref_out_dir <- system.file('test-refs', package = 'rbabylon')
  sum_out_path <- file.path(
    ref_out_dir,
    as.character(glue::glue("{model_name}_summary_obj.R"))
  )
  param_out_path <- file.path(
    ref_out_dir,
    as.character(glue::glue("{model_name}_param_table.R"))
  )

  withr::with_options(list(rbabylon.bbi_exe_path = .bbi_path), {
    message(paste("  writing", sum_out_path))
    .sum <- model_summary(.mod)
    dput(.sum, file = sum_out_path)
    styler::style_file(sum_out_path)

    message(paste("  writing", param_out_path))
    param_df <- param_estimates(.sum)
    dput(param_df, file = param_out_path)
    styler::style_file(param_out_path)
  })
}

###################
# CALLS
###################

# To render refs, source this script.
.proj_root <- rprojroot::find_rstudio_root_file()

file.path(.proj_root, "inst", "model", "nonmem", "basic", 1) %>%
  read_model() %>%
  build_model_summary_refs()
