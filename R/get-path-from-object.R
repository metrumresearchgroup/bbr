
#' Get path from bbi object
#'
#' `bbi_{.model_type}_model` objects correspond to multiple files on disk. These functions
#' provide an easy way to retrieve the absolute path to these files.
#'
#' @details
#' **`get_model_path()`** returns the path to the model definition file.
#'   For NONMEM models, this is the control stream.
#'
#' **`get_output_dir()`** returns the path to the directory containing
#'   output files created when the model is run.
#'
#' **`get_yaml_path()`** returns the path to the YAML file created by bbr.
#'   This file contains metadata like tags, etc. and should, generally speaking,
#'   _not_ be interacted with directly. Use the helper functions mentioned in the
#'   [modify_model_field()] help page to modify this file for you.
#'
#' Note that [build_path_from_model()] and [get_data_path()] provide similar functionality, but have a slightly
#' different interface and implementation and so are documented separately.
#'
#' @param .bbi_object The object to query. Could be
#' a `bbi_{.model_type}_model` object,
#' `bbi_{.model_type}_summary` object,
#' or a tibble of class `bbi_log_df`.
#' @param .check_exists If `TRUE`, the default, will throw an error if the file does not exist
#' @name get_path_from_object
#' @seealso [get_model_id()] [get_data_path()] [build_path_from_model()]
NULL

#' @rdname get_path_from_object
#' @export
get_model_path <- function(.bbi_object, .check_exists = TRUE) {
  UseMethod("get_model_path")
}

#' @rdname get_path_from_object
#' @export
get_model_path.bbi_nonmem_model <- function(.bbi_object, .check_exists = TRUE) {
  get_model_path_nonmem(.bbi_object, .check_exists)
}

#' @rdname get_path_from_object
#' @export
get_model_path.bbi_nonmem_summary <- function(.bbi_object, .check_exists = TRUE) {
  get_model_path_nonmem(.bbi_object, .check_exists)
}


#' @rdname get_path_from_object
#' @export
get_model_path.bbi_log_df <- function(.bbi_object, .check_exists = TRUE) {
  get_path_from_log_df(.bbi_object, get_model_path, .check_exists = .check_exists)
}

#' @rdname get_path_from_object
#' @export
get_output_dir <- function(.bbi_object, .check_exists = TRUE) {
  UseMethod("get_output_dir")
}

#' @rdname get_path_from_object
#' @export
get_output_dir.bbi_nonmem_model <- function(.bbi_object, .check_exists = TRUE) {
  get_output_dir_nonmem(.bbi_object, .check_exists)
}

#' @rdname get_path_from_object
#' @export
get_output_dir.bbi_nonmem_summary <- function(.bbi_object, .check_exists = TRUE) {
  get_output_dir_nonmem(.bbi_object, .check_exists)
}

#' @rdname get_path_from_object
#' @export
get_output_dir.bbi_log_df <- function(.bbi_object, .check_exists = TRUE) {
  get_path_from_log_df(.bbi_object, get_output_dir, .check_exists = .check_exists)
}


#' @rdname get_path_from_object
#' @export
get_yaml_path <- function(.bbi_object, .check_exists = TRUE) {
  UseMethod("get_yaml_path")
}

#' @rdname get_path_from_object
#' @export
get_yaml_path.bbi_model <- function(.bbi_object, .check_exists = TRUE) {
  .path <- paste0(.bbi_object[[ABS_MOD_PATH]], ".yaml")

  if (isTRUE(.check_exists)) {
    checkmate::assert_file_exists(.path)
  }

  return(.path)
}

#' @rdname get_path_from_object
#' @export
get_yaml_path.bbi_log_df <- function(.bbi_object, .check_exists = TRUE) {
  get_path_from_log_df(.bbi_object, get_yaml_path, .check_exists = .check_exists)
}

#' @rdname get_path_from_object
#' @export
get_config_path <- function(.bbi_object, .check_exists = TRUE) {
  UseMethod("get_config_path")
}

#' @rdname get_path_from_object
#' @export
get_config_path.bbi_model <- function(.bbi_object, .check_exists = TRUE) {
  .path <- file.path(get_output_dir(.bbi_object), "bbi_config.json")

  if (isTRUE(.check_exists)) {
    checkmate::assert_file_exists(.path)
  }

  return(.path)
}

#' @rdname get_path_from_object
#' @export
get_config_path.bbi_log_df <- function(.bbi_object, .check_exists = TRUE) {
  get_path_from_log_df(.bbi_object, get_config_path, .check_exists = .check_exists)
}


#' Get model identifier
#'
#' Helper to strip path and extension from model file to get only model identifier
#' (i.e. the model file name without extension)
#' @param .mod Model to use, either a `bbi_{.model_type}_model` or
#' `bbi_{.model_type}_summary`, or a file path to a model.
#' @importFrom tools file_path_sans_ext
#' @return Character scalar with only model identifier
#' @export
get_model_id <- function(.mod) {
  UseMethod("get_model_id")
}

#' @describeIn get_model_id Takes file path to model.
#' @export
get_model_id.character <- function(.mod) {
  return(basename(tools::file_path_sans_ext(.mod)))
}

#' @describeIn get_model_id Takes `bbi_{.model_type}_model` or `bbi_{.model_type}_summary` object
#' @export
get_model_id.bbi_model <- function(.mod) {
  return(get_model_id(get_model_path(.mod)))
}


#' Get path to data file
#'
#' Helper to extract the path to the input data from a
#' `bbi_{.model_type}_summary` or `bbi_{.model_type}_model` object. Note that
#' **this function only works on models that have been successfully run**
#' because the path to the data is constructed during model submission.
#' @param .mod Model to use, either a `bbi_{.model_type}_model` or
#' `bbi_{.model_type}_summary` object.
#' @param ... Arguments passed through to methods. (Currently none, but may have
#'   some modifier arguments other model types in the future.)
#' @importFrom fs path_rel
#' @return Absolute path to input data file
#' @export
get_data_path <- function(.mod, ...) {
  UseMethod("get_data_path")
}

#' @describeIn get_data_path Takes `bbi_nonmem_model` object
#' @export
get_data_path.bbi_model <- function(.mod, ...) {
  cfg_path <- get_config_path(.mod, .check_exists = FALSE)

  if (!fs::file_exists(cfg_path)) {
    stop(paste(
      glue("Cannot extract data path because no config file exists as {cfg_path}"),
      "This likely means the model has not successfully been run yet.",
      sep = "\n"
    ))
  }

  cfg <- jsonlite::fromJSON(cfg_path)

  fs::path_norm(
    file.path(
      get_output_dir(.mod),
      cfg[[CONFIG_DATA_PATH]]
    )
  ) %>%
    as.character()
}

#' Build path to output file
#'
#' Builds the absolute path to a file in the output directory from components of the `bbi_{.model_type}_model` object
#'
#' @return Returns an absolute path to `{output_dir}/{model_id}{.suffix}`.
#'   Does _not_ check whether the file exists.
#'
#' @param .mod Model to use, either a `bbi_{.model_type}_model` or `bbi_{.model_type}_summary` object.
#' @param .suffix Character vector to append the end of the absolute model path.
#'   Will be appended _as is_ so, if passing a file extension, be sure to included the leading `"."`.
#'   See examples.
#' @param ... arguments passed through to methods. (Currently none.)
#'
#' @examples
#' .mod <- read_model(
#'   system.file("model", "nonmem", "basic", "1", package = "bbr")
#' )
#' build_path_from_model(.mod, ".lst")
#' build_path_from_model(.mod, "-standata.R")
#'
#' @export
build_path_from_model <- function(.mod, .suffix, ...) {
  UseMethod("build_path_from_model")
}

#' @rdname build_path_from_model
#' @export
build_path_from_model.bbi_model <- function(.mod, .suffix, ...) {
  file.path(
    get_output_dir(.mod),
    paste0(get_model_id(.mod), .suffix)
  )
}


###################################################################
# Model-specific get path from bbi object implementation functions
###################################################################

#' @keywords internal
get_model_path_nonmem <- function(.bbi_object, .check_exists = TRUE) {
  find_nonmem_model_file_path(.bbi_object[[ABS_MOD_PATH]], .check_exists)
}


#' @keywords internal
get_output_dir_nonmem <- function(.bbi_object, .check_exists = TRUE) {
  .path <- .bbi_object[[ABS_MOD_PATH]]

  if (isTRUE(.check_exists)) {
    checkmate::assert_directory_exists(.path)
  }

  return(.path)
}


###########################
# Get path private helpers
###########################

#' Private helper function to extract paths from bbi_log_df
#' @param .log_df The `bbi_log_df` object
#' @param .get_func The getter function to call
#' @inheritParams get_path_from_object
#' @importFrom purrr map_chr
#' @keywords internal
get_path_from_log_df <- function(.log_df, .get_func, .check_exists) {
  .out_paths <- map_chr(.log_df[[ABS_MOD_PATH]], function(.path) {
    .mod <- read_model(.path)
    .get_func(.mod, .check_exists = .check_exists)
  })
  return(.out_paths)
}

#' Get the working directory of a model object
#'
#' The working directory of a model object is the parent directory of the
#' absolute model path.
#'
#' @param .mod A bbi model or summary object.
#'
#' @return The path to the working directory.
#' @keywords internal
get_model_working_directory <- function(.mod) {
  dirname(.mod[[ABS_MOD_PATH]])
}


###############################
# Manipulating file extensions
###############################

#' Private helper to check if file has a valid NONMEM control stream extension
#' @param .path File path to check
#' @keywords internal
is_valid_nonmem_extension <- function(.path) {
  tools::file_ext(.path) %in% c("ctl", "mod")
}

#' Private helper to check if file has a valid YAML extension
#' @param .path File path to check
#' @keywords internal
is_valid_yaml_extension <- function(.path) {
  tools::file_ext(.path) %in% c("yaml", "yml")
}

#' Change file extension
#'
#' Helpers that strip file extension then add a different one
#' @param .x file path to modify
#' @param .ext new file extension to apply
#' @export
new_ext <- function(.x, .ext) {
  sprintf("%s.%s", tools::file_path_sans_ext(.x), .ext)
}

#' @rdname new_ext
#' @export
ctl_ext <- function(.x) {
  sprintf("%s.ctl", tools::file_path_sans_ext(.x))
}

#' @rdname new_ext
#' @export
mod_ext <- function(.x) {
  sprintf("%s.mod", tools::file_path_sans_ext(.x))
}

#' @rdname new_ext
#' @export
yaml_ext <- function(.x) {
  sprintf("%s.yaml", tools::file_path_sans_ext(.x))
}

###################################
# Other Assorted file path helpers
###################################


#' Find the path to a NONMEM model file
#'
#' NONMEM recognizes two model file extensions, `.ctl` and (less common) `.mod`.
#' Look for these at `.path` and if exactly one exists, return it. If both
#' exist, throw an error, because there is no way to know which should be used.
#' If neither exists and `.check_exists = TRUE`, throw an error.
#'
#' @importFrom fs path_norm
#'
#' @param .path File path to a NONMEM model file (control stream) with either
#'   `.ctl` or `.mod` extension.
#' @inheritParams get_model_path
#'
#' @return The path to the model file, if it exists. If the model file does not
#'   exist and `.check_exists = FALSE`, a string that is `.path` with the `.ctl`
#'   extension.
#' @keywords internal
find_nonmem_model_file_path <- function(.path, .check_exists = TRUE) {
  checkmate::assert_string(.path)
  checkmate::assert_logical(.check_exists, any.missing = FALSE, len = 1L)

  maybe_paths <- paste0(.path, c(".ctl", ".mod"))
  exists_idx <- purrr::map_lgl(maybe_paths, fs::file_exists)

  if (all(exists_idx)) {
    stop(
      glue::glue(
        "Both `.ctl` and `.mod` files found at {.path}. Remove one to continue."
      )
    )
  }

  if (!any(exists_idx)) {
    if (isTRUE(.check_exists)) {
      stop(paste(
        glue(
          "No model file found at {maybe_paths[1L]} or {maybe_paths[2L]}.",
          "Please put relevant model file in that location.",
          .sep = " "
        ),
        "IF THIS IS NOT A NONMEM MODEL please pass the appropriate type to `.model_type`",
        sep = "\n"
      ))
    } else {
      res <- maybe_paths[1L]
    }
  } else {
    res <- maybe_paths[which(exists_idx)]
  }

  res %>%
    fs::path_norm() %>%
    as.character()
}
