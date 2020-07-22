#######################
# Generating run logs
#######################

#' Create tibble summarizing all model runs
#'
#' Parses all model YAML files and outputs into a tibble that serves as a run log for the project.
#' Future releases will incorporate more diagnostics and parameter estimates, etc. from the runs into this log.
#' Users can also use `add_config()` to append additional output about the model run.
#' @seealso `add_config()`
#' @param .base_dir Directory to search for model YAML files. Only models with a corresponding YAML will be included. Defaults to `get_model_directory()`, and falls back to `getwd()` if `get_model_directory()` returns `NULL`.
#' @param .recurse If `TRUE`, the default, search recursively in all subdirectories. Passed through to `fs::dir_ls()` -- If a positive number, the number of levels to recurse.
#' @importFrom purrr map_df
#' @return A tibble of class `bbi_run_log_df` with information on each model.
#' @export
run_log <- function(
  .base_dir = get_model_directory(),
  .recurse = TRUE
) {

  # if no directory defined, set to working directory
  if (is.null(.base_dir)) {
    .base_dir <- getwd()
  }

  mod_list <- find_models(.base_dir, .recurse)
  if(is.null(mod_list)) {
    return(NULL)
  }

  df <- mod_list %>% map_df(run_log_entry)

  df <- create_run_log_object(df)

  return(df)
}


#' Search for model YAML files and read them
#'
#' Private helper function that searches from a base directory for any YAML files (excluding `babylon.yaml`)
#' and attempts to read them to a model object with `safe_read_model()`.
#' @param .base_dir Directory to search for model YAML files.
#' @param .recurse If `TRUE` search recursively in subdirectories as well.
#' @importFrom stringr str_subset
#' @importFrom purrr map_lgl map
#' @importFrom fs dir_ls
#' @keywords internal
find_models <- function(.base_dir, .recurse) {

  # get yaml files
  yaml_files <- dir_ls(.base_dir, recurse = .recurse)
  yaml_files <- str_subset(yaml_files, "\\.ya?ml$")
  yaml_files <- str_subset(yaml_files, "babylon\\.ya?ml$", negate = TRUE)

  # read in all candidate yaml's
  all_yaml <- map(yaml_files, safe_read_model, .directory = NULL)

  # filter to only model yaml's
  not_mod_bool <- map_lgl(all_yaml, is.null)
  not_mod <- yaml_files[which(not_mod_bool)]
  if (length(not_mod) > 0) {
    warning(glue("Found {length(not_mod)} YAML files that do not contain required keys for a model YAML. Ignoring the following files: `{paste(not_mod, collapse='`, `')}`"))
  }
  mod_list <- all_yaml[!not_mod_bool]

  # stop if no valid model YAML found
  if (length(mod_list) == 0) {
    warning(glue("Found no valid model YAML files in {.base_dir}"))
    return(NULL)
  }

  return(mod_list)
}


#' Read in model YAML with error handling
#'
#' Private helper function that tries to call `read_model()` on a yaml path and returns NULL, with no error, if the YAML is not a valid model file.
#' @param .yaml_path Path to read model from
#' @param .directory Model directory which `.yaml_path` is relative to. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`.
#' @importFrom stringr str_detect
#' @keywords internal
safe_read_model <- function(.yaml_path, .directory = get_model_directory()) {
  # check for .directory and combine with .yaml_path
  .yaml_path <- combine_directory_path(.directory, .yaml_path)

  # try to read in model
  .mod <- tryCatch(read_model(.yaml_path),
                   error = function(e) {
                     if (stringr::str_detect(e$message, "Model yaml must have keys")) {
                       return(NULL)
                     } else {
                       stop(glue("Unexpected error trying to read yaml `{.yaml_path}`: {e$message}"))
                     }
                   })
  return(.mod)
}



#' Create a run log row from a `bbi_{.model_type}_model` object
#' @param .mod S3 object of class `bbi_{.model_type}_model`
#' @importFrom tibble tibble
#' @keywords internal
run_log_entry <- function(.mod) {
  # build row
  entry_df <- tibble::tibble(
    !!ABS_MOD_PATH := file.path(
      enforce_length(.mod, WORKING_DIR),
      get_model_id(enforce_length(.mod, YAML_YAML_NAME))
    ),
    !!YAML_YAML_MD5     := enforce_length(.mod, YAML_YAML_MD5),
    !!YAML_MOD_TYPE     := enforce_length(.mod, YAML_MOD_TYPE),
    !!YAML_DESCRIPTION  := enforce_length(.mod, YAML_DESCRIPTION),
    !!YAML_BBI_ARGS     := .mod[[YAML_BBI_ARGS]] %>% list(),
    !!YAML_BASED_ON     := .mod[[YAML_BASED_ON]] %>% list(),
    !!YAML_TAGS         := .mod[[YAML_TAGS]] %>% list(),
    !!YAML_DECISIONS    := .mod[[YAML_DECISIONS]] %>% list()
  )

  # check that it is only one row
  if (nrow(entry_df) != 1) {
    stop(glue("There is a problem with {.mod[[YAML_YAML_NAME]]} file. `run_log()` should be able to load it to 1 row but got {nrow(entry_df)} rows instead. User should not see this error; report to developers if encountered."))
  }

  return(entry_df)
}

#' Private helper to enforce length of a list element
#' @param .l list to check
#' @param .k key to check
#' @param .len length to enforce. Defaults to 1. Throws an error if `length(.l[[.k]]) != .len`
#' @keywords internal
enforce_length <- function(.l, .k, .len = 1) {
  len_k <- length(.l[[.k]])
  if (len_k != .len) {
    stop(glue("The `{.k}` key in file `{.l[[YAML_YAML_NAME]]}` is expected to have length of {.len} but it has length {len_k}. Please fix YAML."))
  }
  return(.l[[.k]])
}

