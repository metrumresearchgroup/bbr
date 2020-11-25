library(rbabylon)

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
#' @param .read_output_refs If `TRUE`, will also save the ref strings for `test-read-output.R` to `inst/test-refs/{.model_name}_output_*.txt`.
run_test_model <- function(
  .model_dir,
  .model_name,
  .summary = TRUE,
  .param_table = TRUE,
  .read_output_refs = TRUE
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

    if (isTRUE(.read_output_refs)) {
      message("  writing out test-read-output.R refs")
      build_read_output_refs(.mod)
    }

    message("  Done.")
  })
}


build_read_output_refs <- function(.mod) {
  .root <- file.path(get_output_dir(.mod), get_model_id(.mod))
  out_dir <- system.file("test-refs/read-output-refs", package = "rbabylon")

  # write out .lst file test cases
  lst_file <- paste0(.root, ".lst")
  lst_out_stem <- file.path(out_dir, paste0(get_model_id(.mod), "_lst_ref_"))

  lst_lines <- readLines(lst_file)
  lst_len <- length(lst_lines)

  lst_default <- c(lst_lines[1:3], "...", lst_lines[(lst_len-4):lst_len])
  writeLines(lst_default, paste0(lst_out_stem, "default.txt"))

  lst_0_5 <- c("...", lst_lines[(lst_len-4):lst_len])
  writeLines(lst_default, paste0(lst_out_stem, "0_5.txt"))

  lst_5_0 <- c(lst_lines[1:5], "...")
  writeLines(lst_default, paste0(lst_out_stem, "5_0.txt"))

  lst_1_5 <- c(lst_lines[1], "...", lst_lines[(lst_len-4):lst_len])
  writeLines(lst_default, paste0(lst_out_stem, "1_5.txt"))

  lst_5_1 <- c(lst_lines[1:3], "...", lst_lines[lst_len])
  writeLines(lst_default, paste0(lst_out_stem, "5_1.txt"))
}

###################
# SAMPLE CALLS
###################

# To re-run models, uncomment whichever model calls you want below
#   and then source this script.

run_test_model("inst/model/nonmem/basic", 1)
