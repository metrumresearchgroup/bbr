
#' Get path from bbi object
#'
#' `bbi_{.model_type}_model` objects correspond to multiple files on disk. These functions
#' provide an easy way to retrieve the absolute path to these files.
#'
#' @param .bbi_object The object to query. Could be
#' a `bbi_{.model_type}_model` object,
#' or a tibble of class `bbi_log_df`.
#' @param .check_exists If `TRUE`, the default, will throw an error if the file does not exist
#' @name get_path_from_object
NULL

#' @rdname get_path_from_object
#' @export
get_model_path <- function(.bbi_object, .check_exists = TRUE) {
  UseMethod("get_model_path")
}

#' @rdname get_path_from_object
#' @export
get_model_path.bbi_nonmem_model <- function(.bbi_object, .check_exists = TRUE) {
  .path <- find_model_file_path(.bbi_object[[ABS_MOD_PATH]])

  if(isTRUE(.check_exists)) {
    checkmate::assert_file_exists(.path)
  }

  return(.path)
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
  .path <- .bbi_object[[ABS_MOD_PATH]]

  if(isTRUE(.check_exists)) {
    checkmate::assert_directory_exists(.path)
  }

  return(.path)
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
get_yaml_path.bbi_nonmem_model <- function(.bbi_object, .check_exists = TRUE) {
  .path <- yaml_ext(.bbi_object[[ABS_MOD_PATH]])

  if(isTRUE(.check_exists)) {
    checkmate::assert_file_exists(.path)
  }

  return(.path)
}

#' @rdname get_path_from_object
#' @export
get_yaml_path.bbi_log_df <- function(.bbi_object, .check_exists = TRUE) {
  get_path_from_log_df(.bbi_object, get_yaml_path, .check_exists = .check_exists)
}

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
#' @param .mod A model object.
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

#' Get model identifier
#'
#' Helper to strip path and extension from model file to get only model identifier
#' (i.e. the model file name without extension)
#' @param .mod Model to use, either a `bbi_{.model_type}_model` or file path to a model.
#' @importFrom tools file_path_sans_ext
#' @returns Character scalar with only model identifier
#' @export
get_model_id <- function(.mod) {
  UseMethod("get_model_id")
}

#' @describeIn get_model_id Takes file path to model.
#' @export
get_model_id.character <- function(.mod) {
  return(basename(tools::file_path_sans_ext(.mod)))
}

#' @describeIn get_model_id Takes `bbi_nonmem_model` object
#' @export
get_model_id.bbi_nonmem_model <- function(.mod) {
  return(basename(tools::file_path_sans_ext(.mod[[YAML_MOD_PATH]])))
}

#' Build absolute path from a directory and path
#'
#' Private helper that concatenates a directory and path, and uses normalizePath() to create an absolute path, with some specific rules:
#' If .directory is NULL, set to working directory.
#' If .path is an absolute path, ignore .directory and just return .path,
#' otherwise return `file.path(.directory, .path)`.
#' Also NOTE that `.directory` must point an already existing directory so that the absolute path can be reliably built.
#' @param .directory Character scalar for the directory
#' @param .path Character scalar for the path to the file (could be just the file name, or could be in a subdirectory)
#' @keywords internal
combine_directory_path <- function(.directory, .path) {
  # If .directory is NULL, set to working directory
  if (is.null(.directory)) {
    .directory <- getwd()
  }

  # If .path is an absolute path, ignore .directory
  if (!fs::is_absolute_path(.path)) {
    # normalizePath and optionally check if it exists
    .directory <- normalizePath(.directory, mustWork = TRUE)

    # build file path
    .path <- file.path(.directory, .path)
  }

  return(.path)
}


#' Private helper to find valid model file and return ctl_ext(.path) by default if not found
#' @param .path File path to a NONMEM model file (control stream) with either `.ctl` or `.mod` extension
#' @keywords internal
find_model_file_path <- function(.path) {
  .ctl_path <- ctl_ext(.path)
  .mod_path <- mod_ext(.path)
  if(fs::file_exists(.ctl_path)) {
    return(.ctl_path)
  } else if(fs::file_exists(.mod_path)) {
    return(.mod_path)
  } else {
    stop(glue("No model file found at {.ctl_path} or {.mod_path}. Please put relevant model file in that location."))
    return(.ctl_path)
  }
}
