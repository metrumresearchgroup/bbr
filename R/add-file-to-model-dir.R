#' Attaches a file to a model
#'
#' These functions take a model object and create the appropriate file in the
#' required location. If `.source_file` argument is used, this file will be
#' copied to the location. Otherwise, a template "scaffold" of the required file
#' is created in that location. **Note, this primarily intended for Stan
#' models** which require several necessary files to run. (See `?`[bbi_stan_model]
#' for details about these files.) Users can call `check_stan_model(.mod)` to
#' see if any of these files are missing.
#' @param .mod a `bbi_{.model_type}_model` object
#' @param .source_file If `NULL`, the default, create an empty scaffold file
#'   at the destination path. If not `NULL`, pass a path to a file that
#'   will be copied to the destination path. Use this if you have a
#'   file elsewhere on disk that you would like to use for this model.
#' @name add_file_to_model_dir
NULL

#' @describeIn add_file_to_model_dir Adds a `<run>.stan` model file.
#' @export
add_stan_file <- function(.mod, .source_file = NULL) {
  add_file_to_model_dir_impl(
    .mod,
    STAN_MOD_CLASS,
    STANMOD_SUFFIX,
    STANMOD_SCAFFOLD_STRING,
    .source_file
  )
}

#' @describeIn add_file_to_model_dir Adds a `<run>-standata.R` file for building the data. See `?`[bbi_stan_model] for details.
#' @export
add_standata_file <- function(.mod, .source_file = NULL) {
  add_file_to_model_dir_impl(
    .mod,
    STAN_MOD_CLASS,
    STANDATA_R_SUFFIX,
    STANDATA_SCAFFOLD_STRING,
    .source_file
  )
}

#' @describeIn add_file_to_model_dir Adds a `<run>-init.R` file for building the initial values. See `?`[bbi_stan_model] for details.
#' @export
add_stan_init <- function(.mod, .source_file = NULL) {
  add_file_to_model_dir_impl(
    .mod,
    STAN_MOD_CLASS,
    STANINIT_SUFFIX,
    STANINIT_SCAFFOLD_STRING,
    .source_file
  )
}

###################################
# PRIVATE IMPLEMENTATION FUNCTIONS
###################################

#' Attaches a file to a model
#'
#' Implementation function for [add_file_to_model_dir()].
#'
#' @importFrom checkmate assert_class assert_string
#' @importFrom digest digest
#'
#' @inheritParams add_file_to_model_dir
#' @param .model_class Function will assert that `.mod` inherits from this class.
#' @param .file_suffix Destination path is created with `build_path_from_model(.mod, .file_suffix)`
#' @param .scaffold_string If `.source_file` is `NULL`, the default, this string will be written
#'   into a new file at the destination path.
#' @param .overwrite Logical scalar for whether to overwrite and existing
#'   file at the destination path. If `is.null(.source_file)` this defaults
#'   to `FALSE`, forcing the user to explicitly confirm overwriting an
#'   existing file with a scaffold. However, **if `.source_file` is passed
#'   then `.overwrite` defaults to `TRUE`**, assuming that the user intends
#'   to use the `.source_file` instead of the existing file.
#' @keywords internal
add_file_to_model_dir_impl <- function(
  .mod,
  .model_class,
  .file_suffix,
  .scaffold_string,
  .source_file = NULL,
  .overwrite = ifelse(is.null(.source_file), FALSE, TRUE)
) {
  checkmate::assert_class(.mod, .model_class)

  dest_path <- build_path_from_model(.mod, .file_suffix)

  # copy over .source_file if one was passed in
  if(!is.null(.source_file)) {
    checkmate::assert_string(.source_file)

    if (fs::file_exists(dest_path)) {
      if (isTRUE(.overwrite)) {
        fs::file_delete(dest_path)
      } else {
        stop(glue("File already exists at {dest_path}. To overwrite existing file with a {.source_file} pass `.overwrite = TRUE`"), call. = FALSE)
      }
    }

    fs::file_copy(.source_file, dest_path)
    message(glue("Copied {.source_file} to {dest_path}"))
    return(.mod)
  }

  # write scaffold to file, first checking if a non-scaffold file would be overwritten
  if (fs::file_exists(dest_path)) {
    if (file_matches_string(dest_path, .scaffold_string)) {
      return(invisible(.mod))
    } else if (!isTRUE(.overwrite)) {
      stop(glue("File already exists at {dest_path}. To overwrite existing file with a scaffold pass `.overwrite = TRUE`"), call. = FALSE)
    }
  }
  writeLines(.scaffold_string, dest_path)

  # return invisibly so this will work in pipes
  return(invisible(.mod))
}

#' Private helper to add scaffolds of any missing stan files
#'
#' This is used when setting up a new model.
#' @importFrom rlang list2
#' @importFrom purrr walk
#' @importFrom fs file_exists
#'
#' @param .mod a `bbi_stan_model`
#' @keywords internal
scaffold_missing_stan_files <- function(.mod) {
  checkmate::assert_class(.mod, STAN_MOD_CLASS)
  files_to_check <- build_path_from_model(.mod, STAN_MODEL_REQ_FILES)
  missing_files <- STAN_MODEL_REQ_FILES[!fs::file_exists(files_to_check)]

  SCAFFOLD_LOOKUP <- rlang::list2(
    !!STANMOD_SUFFIX    := add_stan_file,
    !!STANDATA_R_SUFFIX := add_standata_file,
    !!STANINIT_SUFFIX   := add_stan_init
  )

  purrr::walk(missing_files, function(.f) {
    message(glue("Automatically adding scaffolded {.f} file"))
    SCAFFOLD_LOOKUP[[.f]](.mod)
  })

  return(invisible(NULL))
}
