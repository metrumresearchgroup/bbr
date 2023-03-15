###############################
# Fields from bbi_config.json
###############################

#' Parse bbi configs to log
#'
#' Extracts selected fields from `bbi_config.json`, which is created by bbi
#' in the model output folder to store metadata about the execution of a model
#' run.
#'
#' @return An object of class `bbi_config_log_df`, which includes the fields described below.
#' If _no_ `bbi_config.json` files are found, the returned tibble will only contain the
#' `absolute_model_path` and `run` columns, and will have 0 rows.
#'
#' `config_log()` creates a new tibble with one row per `bbi_config.json`
#' found in `.base_dir` (and subdirectories, if `.recurse = TRUE`).
#'
#' `add_config()` adds these fields to the tibble passed to `.log_df`.
#'
#' @details
#' The following fields from are extracted from `bbi_config.json`.
#'
#'   * `absolute_model_path`: the path to the model file, excluding the file
#'   extension
#'
#'   * `model_md5`: the MD5 sum of the model file
#'
#'   * `data_path`: the path to the data file, relative to the model's output directory
#'     (which can be extracted via [get_output_dir()])
#'
#'   * `data_md5`: the MD5 sum of the data file
#'
#'   * `bbi_version`: the version of bbi last used to run the model
#'
#'   * `nm_version`: the version of NONMEM last used to run the model
#'
#'   * `model_has_changed`: a logical indicating whether the model file has
#'   changed since it was last run
#'
#'   * `data_has_changed`: a logical indicating whether the data file has
#'   changed since the model was last run
#'
#' @seealso [run_log()], [summary_log()]
#' @inheritParams run_log
#' @export
config_log <- function(.base_dir, .recurse = FALSE, .include = NULL) {
  checkmate::assert_string(.base_dir)

  mod_list <- find_models(.base_dir, .recurse, .include)

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
#' @importFrom purrr map_df map_chr map_dfc
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

  out_dirs <- map_chr(.mods, get_output_dir, .check_exists = FALSE)
  json_files <- file.path(out_dirs, "bbi_config.json")

  # check for files that don't exist
  missing <- !fs::file_exists(json_files)

  if (all(missing)) {
    warning(glue("Found no bbi_config.json files for {length(.mods)} models."), call. = FALSE)
    return(
      c(ABS_MOD_PATH, RUN_ID_COL) %>% map_dfc(~ tibble(!!.x := character()))
    )
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

  res_df <- purrr::map_dfr(json_files, config_log_entry)

  res_df <- add_run_id_col(res_df)

  res_df <- create_config_log_object(res_df)

  return(res_df)
}

#' Prepare a model-specific config log entry
#'
#' [config_log()] relies on [config_log_entry()] to create a entry.
#' [config_log_entry()] reads in configuration and relies on this method to
#' prepare and tailor the config object for a given model type.
#'
#' @param .mod A model object.
#' @param config The raw configuration read from `bbi_config.json`.
#' @param fields Requested fields to include in the config. If `NULL`, a default
#'   set of fields for the particular model type should be used.
#' @return A two element list. The first element is named "config" and contains
#'   the prepared config object. The second is named "fields" and is a character
#'   vector of fields that includes those specified via the `fields` parameter
#'   as well as any additional fields that were automatically tacked on.
#' @export
config_log_make_entry <- function(.mod, config, fields = NULL) {
  UseMethod("config_log_make_entry")
}

#' @rdname config_log_make_entry
#' @export
config_log_make_entry.default <- function(.mod, config, fields = NULL) {
  stop("No method for type ", .mod)
}

#' @rdname config_log_make_entry
#' @export
config_log_make_entry.bbi_nonmem_model <- function(.mod, config, fields = NULL) {
  fields <- fields %||% CONFIG_KEEPERS
  if (!all(fields %in% names(config))) {
    path <- get_config_path(.mod, .check_exists = FALSE)
    msg <- paste(
      glue(
        "{path} is missing the required keys:",
        "`{paste(fields[!(fields %in% names(config))], collapse = ', ')}`",
        "and will be skipped.",
        .sep = " "
      ),
      glue(
        "This is likely because it was run with an old version of bbi.",
        "Model was run on version {config[['bbi_version']]}",
        .sep = " "
      ),
      glue(
        "User can call `bbi_current_release()` to see the most recent release",
        "version, and call `use_bbi(options('bbr.bbi_exe_path'))` to",
        "upgrade to the version.",
        .sep = " "
      ),
      .sep = "\n"
    )

    warning(msg)
    return(NULL)
  }
  config[["nm_version"]] <- resolve_nonmem_version(config) %||% NA_character_

  return(list(config = config, fields = c(fields, "nm_version")))
}

#' Parse a bbi config file
#'
#' @param path A string giving the path to `bbi_config.json`.
#' @param fields A character vector of fields to include.
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
#'   * the version of NONMEM
#'
#'   The return value is `NULL` if any element of `fields` is not found in
#'   `path`.
#'
#' @keywords internal
config_log_entry <- function(path, fields = NULL) {
  checkmate::assert_file_exists(path)
  checkmate::assert_character(fields, null.ok = TRUE)

  cfg_mod <- read_model_from_config(path)
  config <- jsonlite::fromJSON(path)

  res <- config_log_make_entry(cfg_mod, config, fields)
  if (is.null(res$config)) {
    return(NULL)
  }

  config <- res$config
  fields <- res$fields

  # bbr.bayes kludge: .build_data is passed for
  # check_up_to_date.bbi_stan_{model,summary}.
  matches <- suppressMessages(check_up_to_date(cfg_mod, .build_data = TRUE))

  config[["model_has_changed"]] <- as.logical(!matches["model"]) # use as.logical to strip off names
  config[["data_has_changed"]]  <- as.logical(!matches["data"])  # use as.logical to strip off names
  config[[ABS_MOD_PATH]] <- cfg_mod[[ABS_MOD_PATH]]

  out_fields <- c(
    ABS_MOD_PATH,
    fields,
    "model_has_changed",
    "data_has_changed"
  )

  config[out_fields]
}


#' Determine the NONMEM version used
#'
#' The version of NONMEM used to run a model is represented in `bbi_config.json`
#' as either the field `nm_version`, or the version in the `nonmem` object with
#' `default: true`.
#'
#' @param x A list representing `bbi_config.json`.
#'
#' @return A string giving the version of NONMEM used.
#' @keywords internal
resolve_nonmem_version <- function(x) {
  checkmate::assert_list(x)

  ver <- x[["nm_version"]]
  if (is.null(ver)) {
    idx <- purrr::map(x[["configuration"]][["nonmem"]], "default")
    ver <- names(purrr::compact(idx))
  }
  ver
}

#' Create a model object from a bbi_config.json path
#'
#' This is non-trivial because, while configs
#' always sit in the ouput directory, for NONMEM models
#' this is one dir deeper than the YAML and for Stan
#' models this is two dirs deeper than the YAML.
#' Hence, this is abstracted into an obnoxious private
#' helper function.
#' @importFrom fs path_norm path_ext_set file_exists
#' @importFrom stringr str_detect
#' @importFrom checkmate assert_file_exists assert_true
#' @param .config_path The absolute path a `bbi_config.json` file
#' @return a `bbi_{.model_type}_model` object
#' @keywords internal
read_model_from_config <- function(.config_path) {
  checkmate::assert_true(stringr::str_detect(.config_path, "bbi_config\\.json$"))
  checkmate::assert_file_exists(.config_path)

  potential_nm_path   <- file.path(.config_path, "..") %>%
    fs::path_norm() %>%
    fs::path_ext_set("yaml")

  potential_stan_path <- file.path(.config_path, "..", "..") %>%
    fs::path_norm() %>%
    fs::path_ext_set("yaml")

  winner <- names(which(fs::file_exists(c(potential_nm_path, potential_stan_path))))

  if (length(winner) != 1) {
    dev_error(glue("read_model_from_config() checked {potential_nm_path} and {potential_stan_path} and the following exist: {paste(winner, collapse = ', ')}"))
  }

  mod <- suppressMessages(
    read_model(tools::file_path_sans_ext(winner))
  )
  return(mod)
}
