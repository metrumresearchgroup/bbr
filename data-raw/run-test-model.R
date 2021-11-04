library(dplyr)
devtools::load_all() # can't call library(bbr) from within bbr when using pkgr

###################
# FUNCTION DEF
###################

#' Helper to re-run models used for vignettes and testing
#'
#' The point of this is that, when updates are made to the test model or its
#' data, you should be able to run this and it should update relevant files used
#' as references in the test suite. Note: will overwrite any output directory
#' for this model.
#' @param .model_dir Path to directory model will be run in, relative to project/package root
#' @param .model_name Name of model file, without extension
#' @param .bbi_path absolute path to bbi installation
run_test_model <- function(
  .model_dir,
  .model_name,
  .bbi_path = read_bbi_path()
) {

  .model_dir <- file.path(rprojroot::find_rstudio_root_file(), .model_dir)
  ref_out_dir <- system.file('test-refs', package = 'bbr')

  withr::with_options(list(bbr.bbi_exe_path = .bbi_path), {
    bbi_init(.dir = .model_dir,            # the directory to create the bbi.yaml in
             .nonmem_dir = "/opt/NONMEM", # location of NONMEM installation
             .nonmem_version = "nm74gf")  # default NONMEM version to use
    withr::defer(fs::file_delete(file.path(.model_dir, "bbi.yaml")))

    mod_path <- file.path(.model_dir, .model_name)

    message(paste("Running", mod_path, "..."))
    .m <- read_model(mod_path)
    submit_model(.m, .mode = "local", .bbi_args = list(overwrite = TRUE))
    message("  finished model run.")

    # update reference values
    ref_json_path <- file.path(ref_out_dir, "ref_values.json")
    ref_json <- jsonlite::fromJSON(ref_json_path)

    .s <- model_summary(.m)
    .fp <- unlist(.s$parameters_data[[1]]$fixed)
    ref_json$PARAM_COUNT_REF <- length(.fp) - sum(.fp) # count of non-fixed parameters
    ref_json$OFV_REF <- .s$ofv[[1]]$ofv_no_constant

    .c <- jsonlite::fromJSON(get_config_path(.m))
    ref_json$CONFIG_DATA_PATH <- .c$data_path
    ref_json$CONFIG_DATA_MD5 <- .c$data_md5
    ref_json$CONFIG_MODEL_MD5 <- .c$model_md5
    ref_json$MOD_BBI_VERSION <- .c$bbi_version
    ref_json$MOD_NM_VERSION <- resolve_nonmem_version(.c)

    readr::write_lines(jsonlite::toJSON(ref_json, pretty=TRUE, auto_unbox = TRUE), ref_json_path)
  })
}


###################
# CALLS
###################

# To re-run models, source this script.
run_test_model("inst/model/nonmem/basic", 1)
