
#' Get path from bbi object
#'
#' Builds the full path to a file that is stored as part of a `bbi_...` S3 object
#' All paths saved in the object or accompanying YAML will be relative **to the location of that YAML**
#' When the object is loaded into memory, the absolute path to the YAML is stored in the object.
#' These functions simply stitch together that path with the requested relative path.
#' As long as the YAML has not moved since it was loaded, this will work.
#' @param .bbi_object The object to query
#' @param .key The key that contains the relative path
#' @export
#' @rdname get_path_from_object
get_path_from_object <- function(.bbi_object, .key) {
  return(file.path(.bbi_object[[WORKING_DIR]], .bbi_object[[.key]]))
}


#' Builds the absolute path to file in the output directory from components of the `bbi_{.model_type}_model` object
#' @param .mod `bbi_{.model_type}_model` object
#' @param .extension file extension to append (for example `lst`, `ext`, `grd`, etc.)
build_path_from_mod_obj <- function(.mod, .extension) {
  ext_path <- file.path(.mod[[WORKING_DIR]],
                        .mod[[YAML_OUT_DIR]],
                        paste0(get_model_id(.mod[[YAML_MOD_PATH]]), ".", .extension))
  return(ext_path)
}


#' Returns the path to the model file from a `bbi_{.model_type}_model` object
#' @param .mod The `bbi_...` S3 object
#' @export
#' @rdname get_path_from_object
get_model_path <- function(.mod) {
  return(get_path_from_object(.mod, YAML_MOD_PATH))
}


#' Returns the path to the model output directory from a `bbi_{.model_type}_model` object
#' @param .mod The `bbi_...` S3 object
#' @export
#' @rdname get_path_from_object
get_output_dir <- function(.mod) {
  return(get_path_from_object(.mod, YAML_OUT_DIR))
}


#' S3 generic to return the path to the YAML file
#' @param .mod generic file path to check
#' @param .check_exists Boolean for whether it will check if the file exists and error if it does not. True by default.
#' @export
#' @rdname get_yaml_path
get_yaml_path <- function(.mod, .check_exists = TRUE) {
  UseMethod("get_yaml_path")
}


#' S3 dispatch to return the path to the YAML file from a `bbi_nonmem_model` object
#' @param .mod The `bbi_nonmem_model` S3 object
#' @export
#' @rdname get_yaml_path
get_yaml_path.bbi_nonmem_model <- function(.mod, .check_exists = TRUE) {
  yaml_file <- .mod[[YAML_YAML_NAME]]
  yaml_path <- file.path(.mod[[WORKING_DIR]], yaml_file) %>% get_yaml_path(.check_exists = .check_exists)
  return(yaml_path)
}


#' S3 dispatch to return the path to the YAML file from a model path or output directory
#' @param .mod The file path to convert to a YAML
#' @export
#' @rdname get_yaml_path
get_yaml_path.character <- function(.mod, .check_exists = TRUE) {
  # convert to .yaml extension
  yaml_path <- .mod %>% yaml_ext()

  # check that the YAML exists
  if (isTRUE(.check_exists)) {
    if (!fs::file_exists(yaml_path)) {
      stop(glue("Inferred YAML path {yaml_path} but no file exists at that location. Use `read_model('path/to/yaml')` to explicitly create model object from YAML."))
    }
  }

  return(yaml_path)
}


#####################
# File path helpers
#####################

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
  if (tools::file_ext(.x) == "yml") {
    return(.x)
  }
  sprintf("%s.yaml", tools::file_path_sans_ext(.x))
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
    return(basename(.ctl_path))
  } else if(fs::file_exists(.mod_path)) {
    return(basename(.mod_path))
  } else {
    warning(glue("No model file found at {.ctl_path} but setting that path as default model path for {.path}. Please put relevant model file in that location."))
    return(basename(.ctl_path))
  }
}


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

