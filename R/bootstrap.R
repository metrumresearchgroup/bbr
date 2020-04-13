
#' Bootstrap a model
#'
#' Resamples data set a specified number of times and creates new models that are copies of `.parent_mod` but point to a resampled data set
#' @param .parent_mod A `bbi_{.model_type}_model` object to bootstrap
#' @param .bootstrap_start Integer to use for the name of the first bootstrapped model
#' @param .bootstrap_count Number of bootstraps to perform. For example if `.bootstrap_start = 100` and `.bootstrap_count = 50` then 50 models, named 100-149 will be created.
#' @param .key_col Character scaler specifying the key column to sample on.
#' @param .sample_rate Number between 0 and 1 specifying the size of each resample. For example `.sample_rate = 0.75` will sample 75% of the data for each bootstrap.
#' @importFrom readr read_csv write_csv
#' @importFrom PKPDmisc resample_df
#' @export
bootstrap <- function(.parent_mod, .bootstrap_start, .bootstrap_count, .key_col, .sample_rate) {
  .orig_key <- paste0("ORIG_", .key_col)

  # load data to sample from
  .data_path <- .parent_mod[[YAML_MOD_TEMPLATE]][[YAML_MOD_DATA_PATH]] %||% extract_data_path(.parent_mod)
  .df <- read_csv(.data_path, col_types = cols())
  names(.df) <- names(.df) %>% toupper()

  # get number of observations to sample
  .orig_num <- .df[.key_col] %>% unique() %>% nrow()
  .sample_num <- round(.orig_num * .sample_rate, 0)

  # do the bootstrap
  .bootstrap_end <- .bootstrap_start + (.bootstrap_count - 1)
  .boot_mods <- map(seq(.bootstrap_start, .bootstrap_end), function(.x) {
    # copy model
    .bootstrap_id <- paste0("bootstrap_", .x)
    .new_mod <- copy_model_from(.parent_mod,
                                .x,
                                .description = paste(.parent_mod[[YAML_DESCRIPTION]], .bootstrap_id, sep = " -- "))

    # re-sample the data
    .sample_df <- PKPDmisc::resample_df(.df, key_cols = .key_col, n = .sample_num) %>%
      rename(
        {{ .orig_key }} := .data[[.key_col]],
        {{ .key_col }} := .data[["KEY"]]
      ) # this is so ugly. I hate tidyverse sometimes.


    # set new data path
    .data_ext <- .data_path %>% tools::file_ext()
    .new_data_path <- as.character(glue("{tools::file_path_sans_ext(.data_path)}_{.bootstrap_id}.{.data_ext}"))

    # write sampled data to new data path
    write_csv(.sample_df, .new_data_path)

    # modify new data path to be relative again
    .new_data_path <- .new_data_path %>% str_replace(paste0(.new_mod[[WORKING_DIR]], "/"), "")
    if (.new_mod %>% get_model_path() %>% tools::file_ext() == "ctl") {
      .new_data_path <- .new_data_path %>% str_replace("\\.\\.\\/", "../../")
    }

    # change data and input lines
    if (is.null(.new_mod[[YAML_MOD_TEMPLATE]])) {

      # modify control stream with new data path
      modify_data_str(.new_mod, .new_data_path, .new_input = names(.sample_df))
    } else {
      stop(glue("{YAML_MOD_TEMPLATE} should be null. This isn't implemented yet."))
    }

    # return new model
    return(.new_mod)
  })

  return(.boot_mods)
}


###############################################################
# Helper utilities for string manipulation of control streams
###############################################################

#' Helper function to modify the raw $DATA and $INPUT lines in a control stream
#' @importFrom stringr str_replace str_detect str_extract
modify_data_str <- function(.mod, .new_data_path, .new_input=NULL) {
  # read parent control stream
  .model_path <- .mod %>% get_model_path()
  mod_str <- .model_path %>% read_file()

  # replace the $DATA line
  .data_regex <- "\\$DATA(.|\n)*?(\\$|IGNORE)"
  .data_line <- str_extract(mod_str, .data_regex)
  if (str_detect(.data_line, "IGNORE$")) {
    .data_end <- "IGNORE"
  } else {
    .data_end <- "\n$"
  }

  mod_str <- str_replace(mod_str,
                         .data_regex,
                         as.character(glue("$DATA {.new_data_path} {.data_end}")))

  # replace $INPUT line
  if (!is.null(.new_input)) {
    .input_str <- paste(.new_input, collapse = " ")
    mod_str <- str_replace(mod_str,
                           "\\$INPUT(.|\n)*?\\$",
                           as.character(glue("$INPUT {.input_str}\n$")))

  }

  # read parent control stream
  write_file(mod_str, .model_path)
}

#' Helper function to extract the data path from the $DATA line of a raw control stream
#' @importFrom stringr str_replace str_detect str_extract
extract_data_path <- function(.mod) {
  # read parent control stream
  .model_path <- .mod %>% get_model_path()
  mod_str <- .model_path %>% read_file()

  # replace the $DATA line
  .data_regex <- "\\$DATA(.|\n)*?(\\$|IGNORE)"
  .data_line <- str_extract(mod_str, .data_regex)
  if (str_detect(.data_line, "IGNORE$")) {
    .data_end <- "IGNORE"
  } else {
    .data_end <- "\n$"
  }

  .data_path <- .data_line %>% str_replace("^\\$DATA ", "") %>% str_replace(" .*$", "") %>% unlist()

  # if ctl file, strip off the first ..
  if (tools::file_ext(.model_path) == "ctl") {
    .data_path <- .data_path %>% str_replace("\\.\\.\\/", "")
  }

  # create absolute path
  .data_path <- file.path(.mod[[WORKING_DIR]], .data_path)
  return(.data_path)
}
