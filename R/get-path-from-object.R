
#' Get path from bbi object
#'
#' Builds the full path to a file that is stored as part of a `bbi_...` S3 object
#' NOTE: All paths saved in the object or accompanying YAML will be relative **to the location of that YAML**
#' When the object is loaded into memory, the absolute path to the YAML is stored in the object.
#' These functions simply stitch together that path with the requested relative path.
#' As long as the YAML has not moved since it was loaded, this will work.
#' @param .bbi_object The model object (or path, etc.) to query
#' @param .key The key that contains the relative path
#' @param .check_exists Logical scaler for whether it will check if the file exists and error if it does not. True by default.
#' @export
#' @rdname get_path_from_object
get_path_from_object <- function(.bbi_object, .key, .check_exists = TRUE) {
  UseMethod("get_path_from_object")
}


#' The default method attempts to extract the path from any object passed to it
#' @param .bbi_object The object to attempt to query. Could be a partially built bbi_{.model_type}_object or some other custom object containing model data.
#' @rdname get_path_from_object
#' @export
get_path_from_object.default <- function(.bbi_object, .key, .check_exists = TRUE) {

  # do some QA on the required fields
  if (any(map_lgl(c(WORKING_DIR, .key), ~ is.null(.bbi_object[[.x]])))) {
    stop_get_fail_msg(
      .bbi_object,
      .key,
      glue(".bbi_object must contain keys for both `{WORKING_DIR}` and `{.key}` but has only the following keys: {paste(names(.bbi_object), collapse = ', ')}")
    )
  }

  if (length(.bbi_object[[.key]]) > 1) {
    stop_get_fail_msg(
      .bbi_object,
      .key,
      glue("Expected a scaler value for `.bbi_object[['{.key}']]` but instead got {class(.bbi_object[[.key]])[1]} of length {length(.bbi_object[[.key]])}")
    )
  }

  # extract the requested path and optionally check if it exists
  .path <- file.path(.bbi_object[[WORKING_DIR]], .bbi_object[[.key]])

  if (isTRUE(.check_exists) && !fs::file_exists(.path)) {
    stop_get_fail_msg(
      .bbi_object,
      .key,
      glue("Extracted path `{.path}` but nothing exists at that location. (If this is expected, pass `.check_exists = FALSE` to bypass this error.)")
    )
  }

  return(.path)

}


#' @rdname get_path_from_object
#' @param .bbi_object Character scaler of a path to a model that can be loaded with `read_model(.bbi_object)`
#' @export
get_path_from_object.character <- function(.bbi_object, .key, .check_exists = TRUE) {

  if (length(.bbi_object) > 1) {
    stop_get_scaler_msg(length(.bbi_object))
  }

  .bbi_object <- tryCatch(
    {
      read_model(.bbi_object)
    },
    error = function(e) {
      stop(glue("Cannot load model object from path `{.bbi_object}` :\n{paste(e, collapse = '\n')}"))
    }
  )

  return(get_path_from_object(.bbi_object, .key, .check_exists = .check_exists))
}


#' @rdname get_path_from_object
#' @param .bbi_object Tibble of class `bbi_run_log_df`
#' @importFrom purrr map_chr
#' @export
get_path_from_object.bbi_run_log_df <- function(.bbi_object, .key, .check_exists = TRUE) {

  .out_paths <- map_chr(.bbi_object[[ABS_MOD_PATH]], function(.path) {
    get_path_from_object(.path, .key, .check_exists = .check_exists)
  })

  return(.out_paths)
}


#' Returns the path to the model file from a `bbi_{.model_type}_model` object
#' @export
#' @rdname get_path_from_object
get_model_path <- function(.bbi_object, .check_exists = TRUE) {
  return(get_path_from_object(.bbi_object, YAML_MOD_PATH, .check_exists = .check_exists))
}

#' Returns the path to the model output directory from a `bbi_{.model_type}_model` object
#' @export
#' @rdname get_path_from_object
get_output_dir <- function(.bbi_object, .check_exists = TRUE) {
  return(get_path_from_object(.bbi_object, YAML_OUT_DIR, .check_exists = .check_exists))
}

#' Returns the path to the model output directory from a `bbi_{.model_type}_model` object
#' @export
#' @rdname get_path_from_object
get_yaml_path <- function(.bbi_object, .check_exists = TRUE) {
  return(get_path_from_object(.bbi_object, YAML_YAML_NAME, .check_exists = .check_exists))
}


###############################
# Manipulating file extensions
###############################

is_valid_nonmem_extension <- function(.path) {
  tools::file_ext(.path) %in% c("ctl", "mod")
}

is_valid_yaml_extension <- function(.path) {
  tools::file_ext(.path) %in% c("yaml", "yml")
}

#' helpers that strip extension then add a diffent type
#' @param .x file path to modify
#' @param .ext new file extension to apply
#' @rdname new_ext
#' @export
new_ext <- function(.x, .ext) {
  sprintf("%s.%s", tools::file_path_sans_ext(.x), .ext)
}

#' @param .x file path to modify
#' @rdname new_ext
#' @export
ctl_ext <- function(.x) {
  sprintf("%s.ctl", tools::file_path_sans_ext(.x))
}

#' @param .x file path to modify
#' @rdname new_ext
#' @export
mod_ext <- function(.x) {
  sprintf("%s.mod", tools::file_path_sans_ext(.x))
}

#' @param .x file path to modify
#' @rdname new_ext
#' @export
yaml_ext <- function(.x) {
  sprintf("%s.yaml", tools::file_path_sans_ext(.x))
}

#' @param .x file path to modify
#' @rdname new_ext
#' @export
yml_ext <- function(.x) {
  sprintf("%s.yml", tools::file_path_sans_ext(.x))
}


###################################
# Other Assorted file path helpers
###################################

#' Helper to strip path and extension from model file to get only model identifier
#' @param .mod generic model
#' @importFrom tools file_path_sans_ext
#' @returns Character scaler with only model identifier
#' @rdname get_model_id
#' @export
get_model_id <- function(.mod) {
  UseMethod("get_model_id")
}

# S3 dispatch to get model identifier from file path to model
#' @param .mod Character scaler model path to strip
#' @rdname get_model_id
#' @export
get_model_id.character <- function(.mod) {
  return(basename(tools::file_path_sans_ext(.mod)))
}

#' S3 dispatch to get model identifier from a `bbi_{.model_type}_model` object
#' @param .mod The `bbi_{.model_type}_model` object
#' @rdname get_model_id
#' @export
get_model_id.bbi_nonmem_model <- function(.mod) {
  return(basename(tools::file_path_sans_ext(.mod[[YAML_MOD_PATH]])))
}

#' Build absolute path from a directory and path
#'
#' Concatenates a directory and path, and uses normalizePath() to create an absolute path, with some specific rules:
#' If .directory is NULL, set to working directory.
#' If .path is an absolute path, ignore .directory and just return .path,
#' otherwise return `file.path(.directory, .path)`.
#' Also NOTE that `.directory` must point an already existing directory so that the absolute path can be reliably built.
#' @param .directory Character scaler for the directory
#' @param .path Character scaler for the path to the file (could be just the file name, or could be in a subdirectory)
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


#' helper to find valid model file and return ctl_ext(.path) by default if not found
#' @param .path File path to a NONMEM model file (control stream) with either `.ctl` or `.mod` extension
find_model_file_path <- function(.path) {
  .ctl_path <- ctl_ext(.path)
  .mod_path <- mod_ext(.path)
  if(fs::file_exists(.ctl_path)) {
    return(.ctl_path)
  } else if(fs::file_exists(.mod_path)) {
    return(.mod_path)
  } else {
    warning(glue("No model file found at {.ctl_path} but setting that path as default model path for {.path}. Please put relevant model file in that location."))
    return(.ctl_path)
  }
}


#' helper to find valid yaml file
#' @param .path File path with any extension or no extension.
#' Function will search for that path with either a .yaml or .yml extension and error if neither is found.
#' Will also error if _both_ are found, because there cannot be two YAML files referring to the same model.
find_yaml_file_path <- function(.path) {
  .yml_path <- yml_ext(.path)
  .yaml_path <- yaml_ext(.path)

  .yaml_bool <- fs::file_exists(unique(c(.yml_path, .yaml_path)))

  if (length(.yaml_bool) == 2 && all(.yaml_bool)) {
    stop(glue("Files found at BOTH {.yml_path} AND {.yaml_path}. Cannot have two YAML files for a single model. Please confirm which is correct and delete the other."))
  } else if(!any(.yaml_bool)) {
    stop(glue("No file found at {.yml_path} OR {.yaml_path}"))
  }

  return(names(which(.yaml_bool)))

}

