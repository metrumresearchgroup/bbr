###############################
# Fields from bbi_config.json
###############################

#' Parse babylon configs to log
#'
#' Extracts selected fields from `bbi_config.json`, which is created by babylon
#' in the model output folder to store metadata about the execution of a model
#' run.
#'
#' @return An object of class `bbi_config_log_df`, which includes the fields
#'
#'   * `absolute_model_path`: the path to the model file, excluding the file
#'   extension
#'
#'   * `model_md5`: the MD5 sum of the model file
#'
#'   * `model_has_changed`: a logical indicating whether the model file has
#'   changed since it was last run
#'
#'   * `data_path`: the path to the data file, relative to `absolute_model_path`
#'
#'   * `data_md5`: the MD5 sum of the data file
#'
#'   * `data_has_changed`: a logical indicating whether the data file has
#'   changed since the model was last run
#'
#'   `config_log()` creates a new tibble with one row per `bbi_config.json`
#'   found in `.base_dir` (and subdirectories, if `.recurse = TRUE`).
#'   `add_config()` adds these fields to `.log_df`.
#'
#' @seealso [run_log()]
#' @inheritParams run_log
#' @export
config_log <- function(
  .base_dir = get_model_directory(),
  .recurse = TRUE
) {

  # if no directory defined, set to working directory
  if (is.null(.base_dir)) {
    stop("`.base_dir` cannot be `NULL`. Either pass a valid directory path or use `set_model_directory()` to set `options('rbabylon.model_directory')` which will be used by default.", call. = FALSE)
  }

  mod_list <- find_models(.base_dir, .recurse)

  df <- config_log_impl(mod_list)
  return(df)
}


#' @rdname config_log
#' @param .log_df A `bbi_run_log_df` tibble (the output of [run_log()]).
#' @export
add_config <- function(.log_df) {
  df <- add_log_impl(.log_df, config_log_impl)
  return(df)
}

#' Build config log
#'
#' Private implementation function for building the config log from a list of model objects.
#' This is called by both [config_log()] and [add_config()].
#' @importFrom stringr str_subset
#' @importFrom fs dir_ls file_exists
#' @importFrom purrr map_df map_chr
#' @importFrom dplyr select everything
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @param .mods List of model objects. The function will attempt to find a `bbi_config.json` file for each model.
#' @keywords internal
config_log_impl <- function(.mods) {

  if(length(.mods) == 0) {
    return(tibble())
  }

  check_model_object_list(.mods)

  out_dirs <- map_chr(.mods, get_output_dir)
  json_files <- file.path(out_dirs, "bbi_config.json")

  # check for files that don't exist
  missing <- !fs::file_exists(json_files)

  if (all(missing)) {
    warning(glue("Found no bbi_config.json files for {length(.mods)} models."), call. = FALSE)
    return(tibble())
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

  json_files %>%
    purrr::map_dfr(parse_bbi_config) %>%
    dplyr::select(.data[[ABS_MOD_PATH]], everything()) %>%
    create_config_log_object()
}

#' Parse a bbi config file
#'
#' @param path A string giving the path to `bbi_config.json`.
#' @param fields A character vector of fields to include.
#' @param model_path_field A string giving the field name for the model path.
#'
#' @return A list whose elements include
#'
#'   * the path to the model file (minus extension)
#'
#'   * `fields`
#'
#'   * whether the model file has changed
#'
#'   * whether the data file has changed
#'
#'   The return value is `NULL` if any element of `fields` is not found in
#'   `path`.
#'
#' @keywords internal
parse_bbi_config <- function(path,
                             fields = CONFIG_KEEPERS,
                             model_path_field = ABS_MOD_PATH) {
  # checkmate::assert_string(path)
  checkmate::assert_file_exists(path)
  checkmate::assert_character(fields)
  checkmate::assert_string(model_path_field)

  config <- jsonlite::fromJSON(path)

  if (!all(fields %in% names(config))) {
    msg <- glue::glue(
      glue::glue(
        "{path} is missing the required keys:",
        "`{paste(fields[!(fields %in% names(config))], collapse = ', ')}`",
        "and will be skipped.",
        .sep = " "
      ),
      glue::glue(
        "This is likely because it was run with an old version of babylon.",
        "Model was run on version {config[['bbi_version']]}",
        .sep = " "
      ),
      glue::glue(
        "User can call `bbi_current_release()` to see the most recent release",
        "version, and call `use_bbi(options('rbabylon.bbi_exe_path'))` to",
        "upgrade to the version.",
        .sep = " "
      ),
      .sep = "\n"
    )

    warning(msg)
    return(NULL)
  }

  # the model_path field may not exist, e.g., it could be the home directory of
  # the user who ran the model, so we cannot use it directly
  output_dir <- dirname(path)
  model_path <- fs::path_ext_set(output_dir, config[["model_extension"]])

  data_path <- fs::path_norm(
    file.path(
      output_dir,
      config[["data_path"]]
    )
  )

  matches <- purrr::map2_lgl(
    c(model_path, data_path),
    c(config[["model_md5"]], config[["data_md5"]]),
    file_matches
  )

  config[["model_has_changed"]] <- !matches[1]
  config[["data_has_changed"]] <- !matches[2]

  config[[model_path_field]] <- output_dir
  config[c(fields, model_path_field, "model_has_changed", "data_has_changed")]
}

#' Compare a file to an MD5 sum
#'
#' @param path String giving the path to the file.
#' @param md5 String giving expected MD5 sum.
#'
#' @return `TRUE` if `path` matches `md5`, `FALSE` if they don't match, and `NA`
#'   if `path` doesn't exist.
#' @keywords internal
file_matches <- function(path, md5) {
  # TODO: consider if this assertion should be that `path` exists
  checkmate::assert_string(path)
  checkmate::assert_string(md5)

  tools::md5sum(path) == md5
}
