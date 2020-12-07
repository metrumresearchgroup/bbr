library(dplyr)
devtools::load_all() # can't call library(rbabylon) from within rbabylon when using pkgr

###################
# FUNCTION DEF
###################

#' Helper to re-build references for test-read-output.R
#'
#' Saves to `inst/test-refs/read-output-refs/`.
#' @param .mod bbi_nonmem_model object
build_read_output_refs <- function(.mod) {
  .root <- file.path(get_output_dir(.mod), get_model_id(.mod))
  out_dir <- system.file(file.path("test-refs", "read-output-refs"), package = "rbabylon")
  message(glue::glue("Writing test-read-output.R refs for {.root} into {out_dir}"))

  # write out .lst file test cases
  lst_file <- paste0(.root, ".lst")
  lst_out_stem <- file.path(out_dir, paste0(get_model_id(.mod), "_lst_ref_"))

  lst_lines <- readLines(lst_file)
  lst_len <- length(lst_lines)

  c(lst_lines[1:3], "...", lst_lines[(lst_len-4):lst_len]) %>%
    writeLines(paste0(lst_out_stem, "default.txt"))

  c("...", lst_lines[(lst_len-4):lst_len]) %>%
    writeLines(paste0(lst_out_stem, "0_5.txt"))

  c(lst_lines[1:5], "...") %>%
    writeLines(paste0(lst_out_stem, "5_0.txt"))

  c(lst_lines[1], "...", lst_lines[(lst_len-4):lst_len]) %>%
    writeLines(paste0(lst_out_stem, "1_5.txt"))

  c(lst_lines[1:5], "...", lst_lines[lst_len]) %>%
    writeLines(paste0(lst_out_stem, "5_1.txt"))

  # write out .ext file tibbles
  ext_file <- paste0(.root, ".ext")
  ext_out_stem <- file.path(out_dir, paste0(get_model_id(.mod), "_ext_ref_"))
  ext_df <- readr::read_table2(ext_file, skip=1, col_types = readr::cols())

  ext_df %>%
    dput(file = paste0(ext_out_stem, "floorNULL.R"))

  ext_df %>%
    filter(ITERATION > 0) %>%
    dput(file = paste0(ext_out_stem, "floor0.R"))

  # write out .grd file tibbles
  grd_file <- paste0(.root, ".grd")
  grd_out_stem <- file.path(out_dir, paste0(get_model_id(.mod), "_grd_ref_"))
  grd_df <- readr::read_table2(grd_file, skip=1, col_types = readr::cols())

  grd_df %>%
    dput(file = paste0(grd_out_stem, "floorNULL.R"))

  grd_df %>%
    filter(ITERATION > 0) %>%
    dput(file = paste0(grd_out_stem, "floor0.R"))

  grd_df %>%
    filter(ITERATION > 10) %>%
    dput(file = paste0(grd_out_stem, "floor10.R"))

}

###################
# CALLS
###################

# To render refs, source this script.
.proj_root <- rprojroot::find_rstudio_root_file()

file.path(.proj_root, "inst", "model", "nonmem", "basic", 1) %>%
  read_model() %>%
  build_read_output_refs()
