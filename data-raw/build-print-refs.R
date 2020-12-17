library(dplyr)
devtools::load_all() # can't call library(rbabylon) from within rbabylon when using pkgr

###################
# FUNCTION DEF
###################

#' Helper to re-build references for test-print.R
#'
#' Saves to `inst/test-refs/print-refs/`.
#' @param .s bbi_nonmem_summary object
build_print_bbi_nonmem_summary_refs <- function(.s) {
  root_dir <- file.path(get_output_dir(.s), get_model_id(.s))
  out_dir <- system.file("test-refs", "print-refs", package = "rbabylon")
  message(glue::glue("Writing test-print.R refs for {root_dir} into {out_dir}"))

  #
  out_stem <- file.path(out_dir, paste0("print_bbi_nonmem_summary_", get_model_id(.s)))

  # NEED TO ADD ITERATING OVER ARGS (pass args in as a list of lists? then use do.call?)
  print_str <- capture.output(print(.s))
  writeLines(print_str, out_stem)

}

###################
# CALLS
###################

# To render refs, source this script.
.proj_root <- rprojroot::find_rstudio_root_file()

file.path(.proj_root, "inst", "model", "nonmem", "basic", 1) %>%
  read_model() %>%
  model_summary() %>%
  build_print_bbi_nonmem_summary_refs()

