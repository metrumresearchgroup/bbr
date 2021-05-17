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
#' @param .path Path to save the new model. Will be the path to the model file
#'   and YAML file (both without extension), and the path to the output
#'   directory.
#' @param .description Character scalar description of new model run. This will
#'   be stored in the yaml (and can be viewed later in `run_log()`).
#' @param .based_on Character scalar or vector of paths to other models that
#'   this model was "based on." These are used to reconstuct model developement
#'   and ancestry. \strong{Paths must be relative to `.yaml_path`.}
#' @param .tags A character scalar or vector with any user tags to be added to
#'   the YAML file
#' @param .bbi_args A named list specifying arguments to pass to bbi
#'   formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" =
#'   4)`. Run [print_bbi_args()] to see valid arguments. These will be written
#'   into YAML file.
#' @param .overwrite If `FALSE`, the default, error if a file already exists at
#'   `.yaml_path`. If `TRUE` overwrite existing file and output directory, if
#'   they exist.
#' @param .model_type Character scaler to specify type of model being created
#'   (used for S3 class). Currently only `'nonmem'` and `'stan'` are supported.
#'   Defaults to `'nonmem'` to preserve legacy API.
#'
#' @return S3 object of class `bbi_{.model_type}_model` that can be passed to
#'   `submit_model()`, `model_summary()`, etc.
#' @seealso [copy_model_from()], [read_model()]
#' @importFrom checkmate assert_scalar
#' @importFrom fs file_exists file_delete dir_exists dir_delete
#' @export
new_model <- function(
  .path,
  .description = NULL,
  .based_on = NULL,
  .tags = NULL,
  .bbi_args = NULL,
  .overwrite = FALSE,
  .model_type = c("nonmem", "stan"),
  ...
) {

  .model_type <- match.arg(.model_type)

  # check if file already exists and decide whether to overwrite if it does
  check_for_existing_model(.path, .overwrite)

  # construct the absolute model path in a way that avoids a warning from
  # normalizePath() if `.path` does not exist (we only require that the model
  # file exist at `.path`)
  abs_mod_path <- file.path(
    normalizePath(dirname(.path)),
    basename(.path)
  )

  parse_new_model_dots(.model_type, abs_mod_path, ...)

  # create model object
  .mod <- list()
  .mod[[ABS_MOD_PATH]] <- abs_mod_path
  .mod[[YAML_MOD_TYPE]] <- .model_type
  .mod <- create_model_object(.mod, save_yaml = TRUE)

  # update model from passed args
  if (!is.null(.description)) .mod <- replace_description(.mod, .description)
  if (!is.null(.tags))        .mod <- replace_all_tags(.mod, .tags)
  if (!is.null(.bbi_args))    .mod <- replace_all_bbi_args(.mod, .bbi_args)
  if (!is.null(.based_on))    .mod <- replace_all_based_on(.mod, .based_on)

  return(.mod)
}


#' Creates a model object from a YAML model file
#'
#' Parses a model YAML file into a list object that contains correctly formatted
#' information from the YAML and is an S3 object of class
#' `bbi_{.model_type}_model` that can be passed to [submit_model()],
#' [model_summary()], etc.
#'
#' @param .path Path to the model to read, in the sense of absolute model path.
#'   The absolute model path is the path to the YAML file and model file, both
#'   without extension, and (possibly) the output directory.
#'
#' @return S3 object of class `bbi_{.model_type}_model`
#' @seealso [copy_model_from()], [new_model()]
#' @export
read_model <- function(.path) {
  yaml_path <- paste0(.path, ".yaml")
  checkmate::assert_file_exists(yaml_path)

  yaml_list <- yaml::read_yaml(yaml_path)
  yaml_list[[ABS_MOD_PATH]] <- fs::path_ext_remove(normalizePath(yaml_path))
  yaml_list[[YAML_YAML_MD5]] <- digest::digest(file = yaml_path, algo = "md5")

  create_model_object(yaml_list, save_yaml = FALSE)
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


#' Private helper to look for existing model and overwrite if necessary
#' @inheritParams new_model
#' @importFrom fs file_exists file_delete dir_exists dir_delete
#' @keywords internal
check_for_existing_model <- function(.path, .overwrite) {
  maybe_yaml_path <- paste0(.path, ".yaml")
  if (fs::file_exists(maybe_yaml_path)) {
    if (isTRUE(.overwrite)) {
      fs::file_delete(maybe_yaml_path)
      if (fs::dir_exists(.path)) fs::dir_delete(.path)
    } else {
      stop(paste(
        glue("File already exists at {maybe_yaml_path}."),
        "Either call `read_model()` to load model from YAML or use,",
        "`.overwrite = TRUE` to overwrite the existing YAML."
      ))
    }
  }
}

#' Parse ... passed to new_model()
#'
#' @importFrom ellipsis check_dots_empty
#' @keywords internal
parse_new_model_dots <- function(.model_type, .path, ...) {
  args <- list(...)

  if (.model_type == "nonmem") {
    if (length(args) > 0) {
      stop(paste(
        "You have passed extra arguments to `new_model()` via `...`, which is NOT valid for NONMEM models.",
        NONMEM_MODEL_TYPE_ERR_MSG,
        glue("The extra passed arguments are {paste(names(args), collapse = ', ')}"),
        sep = "\n"), call. = FALSE)
    }
  } else if (.model_type == "stan") {
    if (all(c("formula", "data") %in% names(args))) {
      stan_files_from_brms(.path, args)
    } else {
      stop(paste(
        "You have passed extra arguments to `new_model(.model_type = 'stan')` via `...`",
        "This is used for constructing a `bbi_stan_model` with `brms` and REQUIRES both `formula` and `data` to be passed.",
        glue("The extra passed arguments are {paste(names(args), collapse = ', ')}"),
        sep = "\n"), call. = FALSE)
    }
  }
}


#' Write necessary stan files to disk from brms args
#'
#' @importFrom readr write_lines
#'
#' @param .path absolute model path (files will be created in this dir)
#' @param args named list of arguments to pass to [brms::make_stancode] and [brms::make_standata]
#' @keywords internal
stan_files_from_brms <- function(.path, args) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("Must have cmdstanr installed to use create a `bbi_stan_model` with `brms`")
  }
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("Must have brms installed to use create a `bbi_stan_model` with `brms`")
  }
  if (!fs::dir_exists(.path)) fs::dir_create(.path)

  # write .stan file
  stan_code <- do.call(brms::make_stancode, args)
  write_lines(stan_code, build_path_from_model(.path, STANMOD_SUFFIX))

  # write data file
  stan_data <- do.call(brms::make_standata, args)
  cmdstanr::write_stan_json(unclass(stan_data), build_path_from_model(.path, STANDATA_JSON_SUFFIX))

  write_lines(
    make_standata_r_from_brms_json(.path),
    build_path_from_model(.path, STANDATA_R_SUFFIX)
  )

  # write inits?
}

#' Code gen make_standata() for data created by brms
#' @param .path absolute model path
#' @keywords internal
make_standata_r_from_brms_json <- function(.path) {
  paste0(
    "# standata created by brms\n",
    "make_standata <- function(.dir) {\n",
    "  jsonlite::fromJSON(file.path(.dir, '", get_model_id(.path), STANDATA_JSON_SUFFIX, "'))\n",
    "}")
}
