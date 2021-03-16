devtools::load_all() # can't call library(bbr) from within bbr when using pkgr

###################
# FUNCTION DEF
###################

#' Helper to re-run Stan model used for vignettes and testing
#'
#' Note: will overwrite any output directory for this model.
#' Currently does _not_ save any test refs.
#' @param .model_dir Path to directory model will be run in, relative to project/package root
#' @param .model_name Name of model file, without extension
run_stan_test_model <- function(
  .model_dir,
  .model_name
) {

  .model_dir <- file.path(rprojroot::find_rstudio_root_file(), .model_dir)
  mod_path <- file.path(.model_dir, .model_name)

  message(paste("Running", mod_path, "..."))
  mod <- read_model(mod_path)
  res <- mod %>%
    submit_model(
      .mode = "local",
      .overwrite = TRUE,
      iter_sampling = 100,
      iter_warmup = 100,
      seed = 123456
    )

  # delete output files that are not included in package
  model_binary <- build_path_from_model(mod, "")
  posterior_csvs <- fs::dir_ls(get_output_dir(mod), glob = "*.csv")
  purrr::walk(c(model_binary, posterior_csvs), ~fs::file_delete(.x))

  message("  finished model run.")
}


###################
# CALLS
###################

# To re-run models, source this script.
run_stan_test_model("inst/model/stan", "fxa")
