########################
# HOLDING PLACE for
# functions having to do
# with stan models
########################

#' Checks a `bbi_stan_model` for necessary files
#'
#' Checks a `bbi_stan_model` object to make sure it can find
#' all of the files necesssary to submit the model.
#'
#' @details
#' Will look for the following:
#' * `build_path_from_model(.mod, "-standata.R")`
#' * `build_path_from_model(.mod, "-init.R")`
#' * `build_path_from_model(.mod, ".stan")`
#'
#' @param .mod A `bbi_stan_model` object
#' @param .error If `FALSE`, the default, will warn if any necessary files are missing.
#'   If `TRUE` will error instead.
#' @importFrom stringr str_replace
#' @importFrom purrr map_lgl
#' @export
check_stan_model <- function(.mod, .error = FALSE) {
  # check if output dir exists and if not create an empty one
  out_dir <- get_output_dir(.mod, .check_exists = FALSE)
  if (!fs::dir_exists(out_dir)) fs::dir_create(out_dir)

  # check for files in output dir
  files_to_check <- build_path_from_model(.mod, STAN_MODEL_REQ_FILES)
  files_present <- fs::file_exists(files_to_check)
  files_missing <- !files_present

  problems <- NULL

  if (any(files_missing)) {
    problems <- paste(
      problems,
      glue("The following files, which are necessary to run a `bbi_stan_model` are missing from {get_model_id(.mod)}:"),
      paste(paste0(" * ", names(which(files_missing))), collapse = "\n"),
      paste("See `?add_file_to_model_dir` for helper functions to add them.\n"),
      sep = "\n"
    )
  }

  # checking if any of the files found are only scaffolds
  if (any(files_present)) {
    candidates <- names(which(files_present))

    scaffold_bool <- map_lgl(candidates, function(.f) {
      tools::md5sum(.f) %in% STAN_SCAFFOLD_MD5_VEC
    })

    if (any(scaffold_bool)) {
      problems <- paste(
        problems,
        glue("The following files, which are necessary to run a `bbi_stan_model` are only scaffolds:"),
        paste(paste0(" * ", candidates[scaffold_bool]), collapse = "\n"),
        "Please add necessary code to the scaffolded files.\n",
        sep = "\n"
      )
    }
  }

  if (!is.null(problems)) {
    if (isTRUE(.error)) {
      stop(problems, call. = FALSE)
    } else {
      message(problems)
    }
  }

  return(invisible(is.null(problems)))
}


#' Attaches a file to a model
#'
#' These functions take a model object and create the appropriate
#' file in the required location. If `.source_file` argument is used,
#' this file will be copied to the location. Otherwise, a scaffold of
#' the required file is created in that location. **Note, this primarily
#' intended for Stan models** which require a number of required files.
#' Users can call `check_stan_model(.mod)` to see if any of these
#' files are missing.
#' @param .mod a `bbi_{.model_type}_model` object
#' @param .source_file If `NULL`, the default, create an empty scaffold file
#'   at the destination path. If not `NULL`, pass a path to a file that
#'   will be copied to the destination path. Use this if you have a
#'   file elsewhere on disk that you would like to use for this model.
#' @param .overwrite Logical scalar for whether to overwrite and existing
#'   file at the destination path. If `is.null(.source_file)` this defaults
#'   to `FALSE`, forcing the user to explicitly confirm overwriting an
#'   existing file with a scaffold. However, **if `.source_file` is passed
#'   then `.overwrite` defaults to `TRUE`**, assuming that the user intends
#'   to use the `.source_file` instead of the existing file.
#' @name add_file_to_model_dir
NULL

#' @describeIn add_file_to_model_dir Adds a `.stan` model file
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

#' @describeIn add_file_to_model_dir Adds a `.stan` model file
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

#' @describeIn add_file_to_model_dir Adds a `.stan` model file
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

#' Adds scaffolds of any missing stan files
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


##############
# modeling
##############

#' Write Stan input data to json
#'
#' Runs the `-standata.R` file associated with a model
#' and writes the returned data list to json with
#' [cmdstanr::write_stan_json].
#'
#' @importFrom cmdstanr write_stan_json
#'
#' @param .mod a `bbi_stan_model` object
#' @param .out_path The path to write the json to on disk.
#'   If `NULL`, the default, it will be written to
#'   `build_path_from_model(.mod, STANDATA_JSON_SUFFIX)`.
#'   This is primarily used to write to a temp path
#'   when testing whether the json in an output folder
#'   is the same as what is produced by calling this.
#'
#' @return Invisibly returns the list object returned
#'   from make_standata()
#'
#' @keywords internal
standata_to_json <- function(.mod, .out_path = NULL) {
  # make sure the make_standata function doesn't exist in a parent environment
  suppressSpecificWarning(rm(make_standata), .regexpr = "object 'make_standata' not found")

  # source and call function
  source(build_path_from_model(.mod, STANDATA_R_SUFFIX))
  standata_list <- make_standata(.dir = get_output_dir(.mod))

  # write to json and return path
  if (is.null(.out_path)) {
    .out_path <- build_path_from_model(.mod, STANDATA_JSON_SUFFIX)
  }
  cmdstanr::write_stan_json(standata_list, .out_path)
  return(invisible(standata_list))
}


#' Pull in Stan init file
#'
#' Sources the `-init.R` file associated with a model
#' and returns whatever is returned by the the `make_init()`
#' function it contains.
#'
#' @param .mod a `bbi_stan_model` object
#' @param .standata the data list that is returned from [standata_to_json(.mod)]
#'
#' @keywords internal
import_stan_init <- function(.mod, .standata) {
  # make sure the standata function doesn't exist in a parent environment
  suppressSpecificWarning(rm(make_init), .regexpr = "object 'make_init' not found")

  # source and call function
  source(build_path_from_model(.mod, STANINIT_SUFFIX))

  # MAYBE FIRST DO SOME CHECKING?
  # that the returned value is actually either a function or a list of lists that all have the same keys?
  return(make_init(.standata))
}


#' Parse args to be passed to cmdstanr
#'
#' Any arguments destined for [cmdstanr::sample()] will be passed in
#' via `...`. This function checks that those are valid and then writes
#' the resulting list of args to a file for reproducibility checking.
#'
#' @importFrom rlang list2
#' @importFrom digest digest
#'
#' @param .mod the `bbi_stan_model` object
#' @param .valid_stanargs A character vector of valid arguments to pass
#'   through to [cmdstanr::sample()]
#' @param ... The arguments to capture and check
#'
#' @return the named list of parsed and checked args
#' @keywords internal
parse_stanargs <- function(.mod, valid_stanargs, ...) {
  stanargs <- rlang::list2(...)

  if (any(names(stanargs) %in% STAN_RESERVED_ARGS)) {
    stop(paste(
      "Cannot pass any of the following through submit_model() to cmdstanr",
      glue("because they are parsed internally from the model object: {paste(STAN_RESERVED_ARGS, collapse = ', ')}")
    ))
  }

  invalid_stanargs <- setdiff(names(stanargs), valid_stanargs)
  if (length(invalid_stanargs) > 0) {
    stop(paste(
      "Attempting to pass invalid arguments through to Stan via submit_model(...)",
      "  The following are not accepted by cmdstanr::sample():",
      paste(invalid_stanargs, collapse = ", "),
      sep = "\n"
    ), call. = FALSE)
  }

  # reorder list and write to disk
  stanargs <- stanargs[order(names(stanargs))]
  dput(stanargs, build_path_from_model(.mod, STANARGS_SUFFIX))

  return(stanargs)
}
