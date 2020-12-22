library(dplyr)
devtools::load_all() # can't call library(rbabylon) from within rbabylon when using pkgr

###################
# FUNCTION DEF
###################

#' Helper to re-build references for test-print.R
#'
#' Saves to `inst/test-refs/print-refs/`.
#' @param .s bbi_nonmem_summary object
#' @param .args list of arguments to pass through to print method
#' @param .suffix optional suffix to put on output file name
build_print_bbi_nonmem_summary_refs <- function(.s, .args = list(), .suffix = "") {
  out_dir <- system.file("test-refs", "print-refs", package = "rbabylon")

  # build output path
  .suffix <- ifelse(.suffix == "", .suffix, paste0("_", .suffix))
  out_path <- file.path(out_dir, paste0("print_nmsum_", get_model_id(.s), .suffix, ".txt"))
  message(glue::glue("Writing test-print.R refs for {get_model_id(.s)} into {out_path}"))

  # capture print output and write to file
  .args[["x"]] <- .s
  print_str <- capture.output(do.call("print", .args))
  writeLines(print_str, out_path)
}

###################
# CALLS
###################

# To render refs, source this script.
withr::with_options(list(rbabylon.bbi_exe_path = read_bbi_path()), {
  .proj_root <- rprojroot::find_rstudio_root_file()

  file.path(.proj_root, "inst", "model", "nonmem", "basic", 1) %>%
    read_model() %>%
    model_summary() %>%
    build_print_bbi_nonmem_summary_refs()

  file.path(.proj_root, "inst", "model", "nonmem", "complex", "iovmm") %>%
    read_model() %>%
    model_summary() %>%
    build_print_bbi_nonmem_summary_refs()

  file.path(.proj_root, "inst", "model", "nonmem", "complex", "1001") %>%
    read_model() %>%
    model_summary(.bbi_args = list(ext_file = "1001.1.TXT")) %>%
    build_print_bbi_nonmem_summary_refs()

  iov_sum <- file.path(.proj_root, "inst", "model", "nonmem", "complex", "acop-iov") %>%
    read_model() %>%
    model_summary()

  build_print_bbi_nonmem_summary_refs(iov_sum, .suffix = "noargs")
  build_print_bbi_nonmem_summary_refs(iov_sum, .args = list(.fixed = TRUE), .suffix = "fixedTRUE")
  build_print_bbi_nonmem_summary_refs(iov_sum, .args = list(.fixed = TRUE, .nrow = 15), .suffix = "fixedTRUE_nrow15")

  # write out args scenarios
  saem_imp_sum <- file.path(.proj_root, "inst", "model", "nonmem", "complex", "example2_saemimp") %>%
    read_model() %>%
    model_summary()

  build_print_bbi_nonmem_summary_refs(saem_imp_sum, .suffix = "noargs")
  build_print_bbi_nonmem_summary_refs(saem_imp_sum, .args = list(.fixed = TRUE), .suffix = "fixedTRUE")
  build_print_bbi_nonmem_summary_refs(saem_imp_sum, .args = list(.off_diag = TRUE), .suffix = "offdiagTRUE")
  build_print_bbi_nonmem_summary_refs(saem_imp_sum, .args = list(.fixed = TRUE, .off_diag = TRUE), .suffix = "fixedTRUE_offdiagTRUE")

})
