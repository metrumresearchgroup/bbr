library(dplyr)
devtools::load_all() # can't call library(bbr) from within bbr when using pkgr

###################
# FUNCTION DEF
###################

#' Helper to re-build references for test-read-output.R
#'
#' Saves to `inst/test-refs/read-output-refs/`.
#' @param .mod bbi_nonmem_model object
build_read_output_refs <- function(.mod) {
  root_dir <- file.path(get_output_dir(.mod), get_model_id(.mod))
  out_dir <- system.file("test-refs", "read-output-refs", package = "bbr")
  message(glue::glue("Writing test-read-output.R refs for {root_dir} into {out_dir}"))

  # write out .lst file test cases
  lst_file <- fs::path_ext_set(root_dir, ".lst")
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
  ext_file <- fs::path_ext_set(root_dir, ".ext")
  ext_out_stem <- file.path(out_dir, paste0(get_model_id(.mod), "_ext_ref_"))
  ext_df <- readr::read_table(ext_file, skip=1, col_types = readr::cols())

  out_path <- paste0(ext_out_stem, "floorNULL.R")
  dput(ext_df, file = out_path)
  styler::style_file(out_path)

  out_path <- paste0(ext_out_stem, "floor0.R")
  dput(filter(ext_df, ITERATION > 0), file = out_path)
  styler::style_file(out_path)

  # write out .grd file tibbles
  grd_file <- fs::path_ext_set(root_dir, ".grd")
  grd_out_stem <- file.path(out_dir, paste0(get_model_id(.mod), "_grd_ref_"))
  grd_df <- readr::read_table(grd_file, skip=1, col_types = readr::cols())

  out_path <- paste0(grd_out_stem, "floorNULL.R")
  dput(grd_df, file = out_path)
  styler::style_file(out_path)

  out_path <- paste0(grd_out_stem, "floor0.R")
  dput(filter(grd_df, ITERATION > 0), file = out_path)
  styler::style_file(out_path)

  out_path <- paste0(grd_out_stem, "floor10.R")
  dput(filter(grd_df, ITERATION > 10), file = out_path)
  styler::style_file(out_path)

}

###################
# CALLS
###################

# To render refs, source this script.
.proj_root <- rprojroot::find_rstudio_root_file()

file.path(.proj_root, "inst", "model", "nonmem", "basic", 1) %>%
  read_model() %>%
  build_read_output_refs()
