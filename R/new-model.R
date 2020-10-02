##############################
# Build or load model object
#############################

#' Create new model object
#'
#' Creates new model object by specifying relevant information as arguments.
#' Also creates necessary YAML file for using functions like `add_tags()` and
#' `run_log()` later. Will look for an associated model file (control stream) on
#' disk and throw an error if it doesn't find one.
#'
#' @param .yaml_path Path to save resulting model YAML file to. MUST be either
#'   an absolute path, or a path relative to the `.directory` argument.
#' @param .description Description of new model run. This will be stored in the
#'   yaml (and can be viewed later in `run_log()`). By convention, it should
#'   match the $PROBLEM statement in the control stream, but this is not
#'   enforced.
#' @param .based_on Character scalar or vector of paths to other models that
#'   this model was "based on." These are used to reconstuct model developement
#'   and ancestry. \strong{Paths must be relative to `.yaml_path`.}
#' @param .tags A character scalar or vector with any user tags to be added to
#'   the YAML file
#' @param .bbi_args A named list specifying arguments to pass to babylon
#'   formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" =
#'   4)`. Run [print_bbi_args()] to see valid arguments. These will be written
#'   into YAML file.
#' @param .overwrite If `FALSE`, the default, error if a file already exists at
#'   `.yaml_path`. If `TRUE` overwrite existing file, if one exists.
#' @param .model_type Character scaler to specify type of model being created
#'   (used for S3 class). Currently only `'nonmem'` is supported.
#' @param .directory Model directory which `.yaml_path` is relative to. Defaults
#'   to `options('rbabylon.model_directory')`, which can be set globally with
#'   `set_model_directory()`.
#' @importFrom yaml write_yaml
#' @importFrom fs file_exists
#' @importFrom digest digest
#' @return S3 object of class `bbi_{.model_type}_model` that can be passed to
#'   `submit_model()`, `model_summary()`, etc.
#' @export
new_model <- function(
  .yaml_path,
  .description,
  .based_on = NULL,
  .tags = NULL,
  .bbi_args = NULL,
  .overwrite = FALSE,
  .model_type = c("nonmem"),
  .directory = get_model_directory()
) {

  # TODO: update once numeric "dispatch" is formally deprecated
  if (fs::path_ext(as.character(.yaml_path)) != "yaml") {
      warning(glue("Did not pass a YAML extension to .yaml_path. Inferred path `{yaml_ext(.yaml_path)}` from `{.yaml_path}`"))
      .yaml_path <- yaml_ext(.yaml_path)
  }

  # check for .directory and combine with .yaml_path
  .yaml_path <- combine_directory_path(.directory, .yaml_path)

  # check if file already exists
  if (fs::file_exists(.yaml_path) && !isTRUE(.overwrite)) {
    stop(paste(glue("Passed {.yaml_path} to `new_model(.yaml_path)` but that file already exists."),
               "Either call `read_model()` to load model from YAML or use `new_model(.overwrite = TRUE)` to overwrite the existing YAML."))
  }

  model_working_directory <- normalizePath(dirname(.yaml_path))
  model_id <- get_model_id(.yaml_path)

  # fill list from passed args
  .mod <- list()
  .mod[[ABS_MOD_PATH]] <- file.path(model_working_directory, model_id)
  .mod[[YAML_DESCRIPTION]] <- .description
  .mod[[YAML_MOD_TYPE]] <- .model_type
  if (!is.null(.based_on)) {
    .mod[[YAML_BASED_ON]] <- safe_based_on(model_working_directory, .based_on)
  }
  if (!is.null(.tags)) .mod[[YAML_TAGS]] <- .tags
  if (!is.null(.bbi_args)) .mod[[YAML_BBI_ARGS]] <- .bbi_args

  # make list into S3 object
  .mod <- create_model_object(.mod, save_yaml = TRUE)

  return(.mod)
}


#' Creates a model object from a YAML model file
#'
#' Parses a model YAML file into a list object that contains correctly formatted information from the YAML
#' and is an S3 object of class `bbi_{.model_type}_model` that can be passed to `submit_model()`, `model_summary()`, etc.
#' @param .path Path to the YAML file to parse. MUST be either an absolute path, or a path relative to the `.directory` argument.
#' @param .directory Model directory which `.path` is relative to. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`.
#' @importFrom yaml read_yaml
#' @importFrom digest digest
#' @importFrom fs file_exists
#' @return S3 object of class `bbi_{.model_type}_model`
#' @export
read_model <- function(
  .path,
  .directory = get_model_directory()
) {

  # check for .directory and combine with .path
  .path <- combine_directory_path(.directory, .path)

  # If not YAML extension, convert to YAML and look for file
  .path <- yaml_ext(.path)
  checkmate::assert_file_exists(.path)

  model_working_directory <- normalizePath(dirname(.path))
  model_id <- get_model_id(.path)

  # load from file
  yaml_list <- read_yaml(.path)
  yaml_list[[ABS_MOD_PATH]] <- file.path(model_working_directory, model_id)
  yaml_list[[YAML_YAML_MD5]] <- digest(file = .path, algo = "md5")

  .mod <- create_model_object(yaml_list, save_yaml = FALSE)
  return(.mod)
}


#' Saves a model object to a yaml file
#'
#' Saves the passed model object to its YAML file and updates
#' the md5 hash after saving.
#' @param .mod S3 object of class `bbi_{.model_type}_model`
#' @importFrom yaml write_yaml
#' @importFrom fs file_exists
#' @importFrom purrr compact
#' @return The input `bbi_{.model_type}_model` object, with its YAML md5 hash updated.
#' @keywords internal
save_model_yaml <- function(.mod) {

  .out_path <- get_yaml_path(.mod, .check_exists = FALSE)

  # create copy to save out
  .out_mod <- .mod

  # erase keys that don't need to be saved out
  for (key in YAML_ERASE_OUT_KEYS) {
    .out_mod[[key]] <- NULL
  }

  # convert keys that need to be coerced to arrays
  for (.key in YAML_SCALAR_TO_LIST_KEYS) {
    if (length(.out_mod[[.key]]) == 1) {
      .out_mod[[.key]] <- (list(.out_mod[[.key]]))
    }
  }

  # throw out empty and null keys
  .out_mod <- purrr::compact(.out_mod)

  # write to disk
  yaml::write_yaml(.out_mod, .out_path)

  # update md5 after writing new yaml
  .mod[[YAML_YAML_MD5]] <- digest(file = .out_path, algo = "md5")

  return(.mod)
}

