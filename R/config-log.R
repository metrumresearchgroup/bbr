###############################
# Fields from bbi_config.json
###############################

#' Parse babylon configs to log
#'
#' Parses `bbi_config.json` files into a log tibble.
#' This file is created by babylon, in the model output folder.
#' It stores metadata about the execution of a model run,
#' including md5 hashes of the model file (control stream) and data file at the time the model was run.
#'
#' @details
#' `config_log()` will return a tibble with one row per `bbi_config.json` found.
#'
#' `add_config()` takes a `bbi_run_log_df` tibble (the output of `run_log()`) as its input,
#' searches for a `bbi_config.json` for each row in the input tibble,
#' and joins on the data from the relevant config, if found.
#' The returned tibble will have all of its input columns, plus all of the columns returned from `config_log()`
#'
#' @seealso `run_log()`
#' @param .base_dir Base directory to look in for models. Defaults to `get_model_directory()`, and falls back to `getwd()` if `get_model_directory()` returns `NULL`.
#' @param .recurse If `TRUE`, the default, search recursively in subdirectories.
#' @export
config_log <- function(
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

  df <- config_log_impl(mod_list)

  df <- create_config_log_object(df)

  return(df)
}


#' @param .log_df a `bbi_run_log_df` tibble (the output of `run_log()`)
#' @importFrom dplyr left_join
#' @importFrom purrr map
#' @rdname config_log
#' @export
add_config <- function(.log_df) {
  # check input df
  check_bbi_run_log_df_object(.log_df)

  # get config log
  mod_list <- map(.log_df[[ABS_MOD_PATH]], ~ read_model(.x))
  .conf_df <- config_log_impl(mod_list)

  # join to log df
  df <- left_join(
    .log_df,
    .conf_df,
    by = ABS_MOD_PATH
  )

  df <- create_config_log_object(df)

  return(df)
}

#' Build config log
#'
#' Private implementation function for building the config log from a list of model objects.
#' This is called by both `config_log()` and `add_config()`.
#' @importFrom stringr str_subset
#' @importFrom fs dir_ls file_exists
#' @importFrom purrr map_df map_chr
#' @importFrom dplyr select everything
#' @importFrom jsonlite fromJSON
#' @param .mods List of model objects. The function will attempt to find a `bbi_config.json` file for each model.
#' @keywords internal
config_log_impl <- function(.mods) {

  check_model_object_list(.mods)

  out_dirs <- map_chr(.mods, get_output_dir)
  json_files <- file.path(out_dirs, "bbi_config.json")

  # check for files that don't exist
  missing <- !fs::file_exists(json_files)

  if (all(missing)) {
    warning(glue("Found no bbi_config.json files for {length(.mods)} models."), call. = FALSE)
    return(NULL)
  }

  if (any(missing)) {
    warning(paste(
      glue("Found only {sum(!missing)} bbi_config.json files for {length(.mods)} models."),
      "The following models may have failed or still be in progress:",
      paste(out_dirs[missing], collapse = "\n"),
      sep = "\n"
    ), call. = FALSE)

    # throw out missing files
    json_files <- json_files[!missing]
  }

  # map over json files and parse relevant fields
  df <- map_df(json_files, function(.path) {

    .conf_list <- fromJSON(.path)

    if (!all(CONFIG_KEEPERS %in% names(.conf_list))) {
      warning(paste(
        glue("{.path} is missing the required keys: `{paste(CONFIG_KEEPERS[!(CONFIG_KEEPERS %in% names(.conf_list))], collapse=', ')}` and will be skipped."),
        glue("This is likely because it was run with an old version of babylon. Model was run on version {.conf_list$bbi_version}"),
        "User can call `bbi_current_release()` to see the most recent release version, and call `use_bbi(options('rbabylon.bbi_exe_path'))` to upgrade to the version.",
        sep = "\n"
      ))
      return(NULL)
    }

    .conf_list <- .conf_list[CONFIG_KEEPERS]
    .conf_list[[ABS_MOD_PATH]] = dirname(.path)

    return(.conf_list)
  })

  df <- select(df, .data[[ABS_MOD_PATH]], everything())

  return(df)
}
