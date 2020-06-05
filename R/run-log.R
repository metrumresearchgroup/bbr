#######################
# Generating run logs
#######################


#' Parses model yaml and outputs into a tibble that serves as a run log. Future releases will incorporate more diagnostics and parameter estimates, etc. from the runs into this log.
#' @param .base_dir Directory to search for model yaml files. Only runs with a corresponding yaml will be included.
#' @param .recurse Boolean for whether to search subdirectories recursively for additional yaml files. Defaults to TRUE.
#' @importFrom stringr str_subset
#' @importFrom fs dir_ls
#' @importFrom purrr map map_lgl transpose
#' @importFrom dplyr as_tibble mutate_at mutate select everything
#' @importFrom yaml read_yaml
#' @return tibble with information on each run
#' @export
run_log <- function(
  .base_dir = get_model_directory(),
  .recurse = TRUE
) {

  # if no directory defined, set to working directory
  if (is.null(.base_dir)) {
    .base_dir <- getwd()
  }

  # get yaml files
  yaml_files <- .base_dir %>% dir_ls(recurse = .recurse) %>% str_subset("\\.ya?ml$") %>% str_subset("babylon\\.yaml$", negate = TRUE)

  # read in all candidate yaml's
  all_yaml <- map(yaml_files, safe_read_model, .directory = NULL)

  # filter to only model yaml's
  not_mod_bool <- map_lgl(all_yaml, is.null)
  not_mod <- yaml_files[which(not_mod_bool)]
  if (length(not_mod) > 0) {
    warning(glue("Found {length(not_mod)} YAML files that do not contain required keys for a model YAML. Ignoring the following files: `{paste(not_mod, collapse='`, `')}`"))
  }
  mod_yaml <- all_yaml[!not_mod_bool]

  # stop if no valid model YAML found
  if (length(mod_yaml) == 0) {
    warning(glue("Found no valid model YAML files in {.base_dir}"))
    return(NULL)
  }

  # create run log tibble
  df <- mod_yaml %>% map_df(run_log_entry)

  df <- create_run_log_object(df)

  return(df)
}

#' Read in model YAML with error handling
#'
#' Helper function that tries to call `read_model()` on a yaml path and returns NULL, with no error, if the YAML is not a valid model file.
#' @param .yaml_path Path to read model from
#' @param .directory Model directory which `.yaml_path` is relative to. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`.
#' @importFrom stringr str_detect
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
#' @export
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

#' Helper to enforce length of a list element
#' @param .l list to check
#' @param .k key to check
#' @param .len length to enforce. Defaults to 1. Throws an error if `length(.l[[.k]]) != .len`
enforce_length <- function(.l, .k, .len = 1) {
  len_k <- length(.l[[.k]])
  if (len_k != .len) {
    stop(glue("The `{.k}` key in file `{.l[[YAML_YAML_NAME]]}` is expected to have length of {.len} but it has length {len_k}. Please fix YAML."))
  }
  return(.l[[.k]])
}


###############################
# Fields from bbi_config.json
###############################

#' Parses model config.json outputs into a log tibble
#' @param .base_dir Base directory to look from model YAML files in
#' @param .recurse Boolean for whether to search recursively in subdirectories
#' @importFrom stringr str_subset
#' @importFrom fs dir_ls
#' @importFrom purrr map_df
#' @importFrom dplyr mutate select everything
#' @importFrom jsonlite fromJSON
#' @return tibble with information on each run
#' @export
config_log <- function(
  .base_dir = get_model_directory(),
  .recurse = TRUE
) {

  # if no directory defined, set to working directory
  if (is.null(.base_dir)) {
    .base_dir <- getwd()
  }

  # define json keys to keep as constant
  KEEPERS = c(
    "model_md5",
    "data_path",
    "data_md5"
  )

  BBI_CONFIG <- "/bbi_config.json$"

  # get json files and parse to df
  json_files <- dir_ls(.base_dir, recurse = .recurse)
  json_files <- str_subset(json_files, BBI_CONFIG)
  json_files <- normalizePath(json_files)

  if (length(json_files) == 0) {
    warning(glue("Found no bbi_config.json files in {.base_dir}"))
    return(NULL)
  }

  # map over json files and parse relevant fields
  df <- map_df(json_files, function(.path) {
    .conf_list <- fromJSON(.path)
    if (!all(KEEPERS %in% names(.conf_list))) {
      warning(paste(
        glue("{.path} is missing the required keys: `{paste(KEEPERS[!(KEEPERS %in% names(.conf_list))], collapse=', ')}` and will be skipped."),
        glue("This is likely because it was run with an old version of babylon. Model was run on version {.conf_list$bbi_version}"),
        "User can call `bbi_current_release()` to see the most recent release version, and call `use_bbi(options('rbabylon.bbi_exe_path'))` to upgrade to the version.",
        sep = "\n"
      ))
      return(NULL)
    }
    return(.conf_list[KEEPERS])
  })
  df <- mutate(df, !!ABS_MOD_PATH := str_replace(json_files, BBI_CONFIG, ""))
  df <- select(df, .data[[ABS_MOD_PATH]], everything())

  return(df)
}

#' Joins config log tibble onto a matching run log tibble
#' @param .log_df the output tibble from `run_log()`
#' @param ... arguments passed through to `config_log()`
#' @importFrom dplyr left_join
#' @export
add_config <- function(.log_df, ...) {
  # check input df
  if (!inherits(.log_df, "bbi_run_log_df")) {
    stop(glue("Can only pass an object of class `bbi_run_log_df` to `add_config()`. Passed object has classes {paste(class(.log_df), collapse = ', ')}"))
  }

  # get config log
  .conf_df <- config_log(...)

  # check for missing rows
  if (is.null(.conf_df)) {
    warning(paste(glue("add_config() found {nrow(.log_df)} runs but found {nrow(.conf_df)} configs."),
                  "Check if your runs are still in progress.",
                  sep = "\n"))
    return(.log_df)
  } else if (nrow(.log_df) != nrow(.conf_df)) {
    warning(paste(glue("add_config() found {nrow(.log_df)} runs but found {nrow(.conf_df)} configs."),
                  "Check for rows with `is.null('model_md5')` in tibble. Some runs may have failed or still be in progress.",
                  sep = "\n"))
  }

  # join to log df
  df <- left_join(
    .log_df,
    .conf_df,
    by = ABS_MOD_PATH,
    suffix = c(".log", ".config")
  )

  return(df)
}


