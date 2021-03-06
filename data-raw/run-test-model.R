library(dplyr)
devtools::load_all() # can't call library(bbr) from within bbr when using pkgr

###################
# FUNCTION DEF
###################

#' Helper to re-run models used for vignettes and testing
#'
#' Note: will overwrite any output directory for this model.
#' @param .model_dir Path to directory model will be run in, relative to project/package root
#' @param .model_name Name of model file, without extension
#' @param .bbi_path absolute path to bbi installation
run_test_model <- function(
  .model_dir,
  .model_name,
  .bbi_path = read_bbi_path()
) {

  .model_dir <- file.path(rprojroot::find_rstudio_root_file(), .model_dir)
  .ref_out_dir <- system.file('test-refs', package = 'bbr')

  withr::with_options(list(bbr.bbi_exe_path = .bbi_path), {
    bbi_init(.dir = .model_dir,            # the directory to create the bbi.yaml in
             .nonmem_dir = "/opt/NONMEM", # location of NONMEM installation
             .nonmem_version = "nm74gf")  # default NONMEM version to use
    withr::defer(fs::file_delete(file.path(.model_dir, "bbi.yaml")))

    mod_path <- file.path(.model_dir, .model_name)

    message(paste("Running", mod_path, "..."))
    .proc <- read_model(mod_path) %>%
      submit_model(.mode = "local", .bbi_args = list(overwrite = TRUE))
    message("  finished model run.")
  })
}


###################
# CALLS
###################

# To re-run models, source this script.
run_test_model("inst/model/nonmem/basic", 1)
