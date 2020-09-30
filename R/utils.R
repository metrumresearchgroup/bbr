#' Checks that all passed NONMEM command line args are valid and formats
#' @param .args A named list of .args to check
#' @importFrom checkmate assert_list
#' @importFrom rlang is_bare_character is_bare_numeric is_bare_logical
#' @importFrom purrr imap set_names
#' @return character string, output from format_cmd_args()
#' @keywords internal
check_bbi_args <- function(.args) {
  # if NULL, return NULL back
  if (length(.args) == 0) {
    return(NULL)
  }

  # check that unique named list was passed
  tryCatch(
    checkmate::assert_list(.args, names="unique"),
    error = function(e) {
      err_msg <- paste("`.args` must be a unique, named list:", e)
      stop(err_msg)
    }
  )

  # check against BBI_ARGS
  err_vec <- imap(.args, function(.v, .n) {
    # check that arg is valid
    ref <- BBI_ARGS[[.n]]
    if (is.null(ref)) {
      err_msg <- paste(.n, "is not a valid argument for the `.args` list in submit_nonmem_model(). Run `print_bbi_args()` to see valid arguments.")
      return(err_msg)
    }

    # check type
    if (ref$type == "character") {
      type_pass_bool <- is_bare_character(.v)
    } else if (ref$type == "logical") {
      type_pass_bool <- is_bare_logical(.v)
    } else if (ref$type == "numeric") {
      type_pass_bool <- is_bare_numeric(.v)
    } else {
      # This feels weird to put an intentional bug catch in here, but I don't like having to rely on raw string matching
      # from our aaa.R file to trigger functionality, so this feels like the safest option.
      stop(paste0("BUG!!! BBI_ARGS[['", .n, "']][['type']] is `", ref$type, "` which is invalid. Must be either 'character', 'logical', or 'numeric'"))
    }
    if (!type_pass_bool) {
      err_msg <- paste0("`", .v, "` passed for arg `", .n, "`. Expected '", ref$type, "' type.")
      return(err_msg)
    }
    # if no errors return NULL
    return(NULL)
  }) %>% unlist()

  # check for errors, if no errors format arguments
  if (length(err_vec) > 0) {
    err_msg <- paste0(
      "There are ", length(err_vec), " errors in check_bbi_args(): ", paste(err_vec, collapse = " :: ")
    )
    stop(err_msg)
  }

  # format to key value pairs with command line flags
  key_value_flags <- unlist(imap(.args, function(.v, .n) {BBI_ARGS[[.n]]$flag}))
  key_value_list <- imap(.args, function(.v, .n) {.v}) %>% set_names(key_value_flags)

  # format list to character vector
  return(format_cmd_args(key_value_list))
}

#' Formats command line args from a named list to a string as it would be passed on the command line
#' @param .args A named list of .args to check
#' @param .collapse If `FALSE`, the default, returns a character vector. If `TRUE`, collapses return vector to a single string.
#' @importFrom checkmate assert_list
#' @importFrom rlang is_bare_logical
#' @importFrom purrr imap set_names
#' @return character vector of command args
#' @export
format_cmd_args <- function(.args, .collapse = FALSE) {
  # check that unique named list was passed
  tryCatch(
    checkmate::assert_list(.args, names="unique"),
    error = function(e) {
      err_msg <- paste("`.args` must be a unique, named list:", e)
      stop(err_msg)
    }
  )

  # build string from list
  arg_vec <- imap(.args, function(.v, .n) {
    # if logical and TRUE add flag
    if (rlang::is_bare_logical(.v)) {
      if (.v) {
        return(sprintf("%s", .n))
      } else {
        # should we return a warning here that passing FALSE does nothing?
        return(NULL)
      }
    }
    # otherwise add flag with value
    return(sprintf("%s=%s", .n, .v))
  }) %>% unlist()

  # return parsed args
  if (.collapse) {
    return(paste0(arg_vec, collapse = " "))
  } else {
    return(arg_vec %>% set_names(NULL))
  }
}

#' Group models for bbi submission
#'
#' Multiple models can be submitted in a single bbi call if those models share a
#' working directory and a set of CLI arguments.
#'
#' @inheritParams submit_nonmem_models
#'
#' @return A list whose elements correspond to groups of models that can be
#'   submitted in a single bbi call. Each element is itself a list with elements
#'
#'   * `bbi_args`: a set of CLI arguments
#'
#'   * `models`: a list of the elements of `.mods` in the group
#'
#' @keywords internal
build_bbi_param_list <- function(.mods, .bbi_args = NULL) {

  # check that everything in list is a model object
  check_model_object_list(.mods)

  # character vector whose elements correspond to `.mods` and are strings
  # representing the CLI args to bbi for the corresponding model
  all_mod_args <-
    .mods %>%
    # TODO: consider whether this should be abstracted
    purrr::map(YAML_BBI_ARGS) %>%
    # `parse_args_list()` will return an empty list if both are `NULL`
    purrr::map(~ parse_args_list(.bbi_args, .)) %>%
    purrr::map(check_bbi_args) %>%
    purrr::map(sort)

  mod_working_dirs <-
    .mods %>%
    # TODO: change this once we have the helper
    purrr::map(WORKING_DIR) %>%
    purrr::map_chr(dirname)

  mod_keys <- purrr::map2(
    all_mod_args,
    mod_working_dirs,
    c
  )

  purrr::map(
    unique(mod_keys),
    ~ {
      key_idx <- purrr::map_lgl(mod_keys, identical, .)
      key <- mod_keys[key_idx][[1L]]
      # `key` can be of variable length, but the directory is always last
      dir_idx <- length(key)
      args <- key[-dir_idx]
      keep_mod_idx <- purrr::map2_lgl(
        all_mod_args,
        mod_working_dirs,
        ~ identical(.x, args) && identical(.y, key[dir_idx])
      )
      list(
        bbi_args = args,
        models = .mods[keep_mod_idx]
      )
    }
  )
}

#' Combines NONMEM args that were passed into the function call with args that were parsed from a model yaml
#' @param .func_args A named list of arguments for bbi, passed into submit_model function call
#' @param .yaml_args A named list of arguments for bbi, parsed from user input yaml
#' @importFrom checkmate assert_list
#' @return The combination of the two lists, with .func_args overwriting any keys that are shared
#' @keywords internal
parse_args_list <- function(.func_args, .yaml_args) {
  # start with .yaml_args
  if (is.null(.yaml_args)) {
    .yaml_args <- list()
  }
  .args <- .yaml_args

  if (!is.null(.func_args)) {
    # check that unique named list was passed
    tryCatch(
      checkmate::assert_list(.func_args, names="unique"),
      error = function(e) { stop(glue("`.args` must be a unique, named list: {e}")) }
    )
    # add .func_args and overwrite anything from .yaml_args list that's in .func_args
    for (.n in names(.func_args)) {
      .args[[.n]] <- .func_args[[.n]]
    }
  }
  return(.args)
}


#' Combines two named lists. By default, shared keys are overwritten by that key in .new_list
#' Any S3 classes will be inherited with those from .new_list given precedence.
#' @param .new_list A named list
#' @param .old_list A named list
#' @param .append Shared keys will have their contents concatenated instead of overwriting from .new_list
#' @importFrom checkmate assert_list
#' @return The combined list
#' @keywords internal
combine_list_objects <- function(.new_list, .old_list, .append = FALSE) {
  # check that unique named lists were passed
  tryCatch(
    checkmate::assert_list(.new_list, names="unique"),
    error = function(e) { stop(glue("`.new_list` must be a unique, named list: {e}")) }
  )
  tryCatch(
    checkmate::assert_list(.old_list, names="unique"),
    error = function(e) { stop(glue("`.old_list` must be a unique, named list: {e}")) }
  )

  # add .new_list and overwrite anything from .old_list list that's in .new_list
  .out_list <- list()
  for (.n in names(.new_list)) {
    .out_list[[.n]] <- .new_list[[.n]]
  }
  for (.n in names(.old_list)) {
    if (.n %in% names(.out_list)) {
      if (.append) {
        .out_list[[.n]] <- c(.out_list[[.n]], .old_list[[.n]])
      }
    } else {
      .out_list[[.n]] <- .old_list[[.n]]
    }
  }

  # add classes and return
  class(.out_list) <- unique(c(class(.new_list), class(.old_list)), fromLast = T)
  return(.out_list)
}

#' Print valid .bbi_args
#'
#' Prints all valid arguments to pass in to `.bbi_args=list()` argument of `submit_model()` or `model_summary()`
#' @importFrom purrr imap
#' @export
print_bbi_args <- function() {
  doc_list <- imap(BBI_ARGS, function(.v, .n) {
    paste0(.n, " (", .v$type, ") -- ", .v$description, " (sets CLI flag `", .v$flag, "`)")
  })
  cat(paste(doc_list, collapse = "\n"))
}


check_required_keys <- function(.list, .req) {
  all(.req %in% names(.list))
}


#' Get or set global model directory option
#'
#' Gets or sets `options('rbabylon.model_directory')`,
#' which is used by default in functions like `read_model()`, `submit_model()` and `model_summary()` so that,
#' once this is set, those functions can take a path relative to this directory instead of the working/script directory.
#'
#' @details
#' `set_model_directory()` sets `options('rbabylon.model_directory')` to the absolute path of the directory passed to `.path`.
#' Note that the directory must exist or this will error.
#'
#' `get_model_directory()` gets the path set to `options('rbabylon.model_directory')` and checks that it is both absolute and exists,
#' erroring if it is not.
#' @importFrom fs is_absolute_path dir_exists
#' @export
get_model_directory <- function() {
  .mod_dir <- getOption("rbabylon.model_directory")
  if (is.null(.mod_dir)) {
    return(NULL)
  }

  if (!fs::is_absolute_path(.mod_dir)) {
    strict_mode_error(paste(
      glue("`options('rbabylon.model_directory')` must be set to an absolute path but is currently set to {.mod_dir}"),
      "It is recommended to use `set_model_directory('...')` or put `options('rbabylon.model_directory' = normalizePath('...'))` in your .Rprofile for this project.",
      sep = "\n"))
  }

  if (!fs::dir_exists(.mod_dir)) {
    strict_mode_error(paste(
      glue("`options('rbabylon.model_directory')` must be set to an existing directory but {.mod_dir} does not exist."),
      "It is recommended to use `set_model_directory('...')` or put `options('rbabylon.model_directory' = normalizePath('...'))` in your .Rprofile for this project.",
      sep = "\n"))
  }

  return(.mod_dir)
}


#' @rdname get_model_directory
#' @param .path Path, either from working directory or absolute, that will be set as `options('rbabylon.model_directory')`
#' @export
set_model_directory <- function(.path) {
  if (is.null(.path)) {
    options('rbabylon.model_directory' = NULL)
  } else {
    options('rbabylon.model_directory' = normalizePath(.path, mustWork = TRUE))
  }

  cat(glue("options('rbabylon.model_directory') set to {options('rbabylon.model_directory')}"))
}


#' Private helper to search for babylon.yaml config files
#' @param .config_path Path to config, possibly relative
#' @param .model_dir absolute path to directory where model is be run from
#' @importFrom fs is_file file_exists is_absolute_path
#' @importFrom stringr str_detect
#' @return path to babylon.yaml in `.config_path`, relative to `.model_dir`
#' @keywords internal
find_config_file_path <- function(.config_path, .model_dir) {
  if (!fs::is_absolute_path(.model_dir)) {
    stop(glue("USER SHOULDN'T SEE THIS ERROR: find_config_file_path(.model_dir) is not absolute: {.model_dir}"))
  }

  # if passed a directory, add babylon.yaml
  if (!is_valid_yaml_extension(.config_path)) {
    .config_path <- file.path(.config_path, "babylon.yaml")
  }

  .config_path <- tryCatch({
    normalizePath(.config_path, mustWork = TRUE)
  },
  error = function(e) {
    if (str_detect(e$message, "No such file or directory")) {
      stop(glue("No babylon.yaml file exists at {.config_path} -- Either use `bbi_init('model/dir/')` to create one, or pass a valid path to the `.config_path` argument of `submit_model()`"))
    }
    stop(e$message)
  })

  .config_path <- fs::path_rel(.config_path, .model_dir)
  return(.config_path)
}

#' Private helper to check if an object inherits a model class and error if not
#' @param .mod The object to check
#' @param .mod_types Character vector of acceptable classes, defaulting to `VALID_MOD_CLASSES`
#' @keywords internal
check_model_object <- function(.mod, .mod_types = VALID_MOD_CLASSES) {
  if (!inherits(.mod, .mod_types)) {
    stop(glue("Must pass a model object, but got object of class: `{paste(class(.mod), collapse = ', ')}`"))
  }
  return(invisible(TRUE))
}

#' Private helper to check if a list of objects all inherit a model class and error if not
#' @param .mods The list of objects to check
#' @param .mod_types Character vector of acceptable classes, defaulting to `VALID_MOD_CLASSES`
#' @keywords internal
check_model_object_list <- function(.mods, .mod_types = VALID_MOD_CLASSES) {
  all_models_bool <- map_lgl(.mods, function(.x) { inherits(.x, .mod_types) })
  if (isFALSE(all(all_models_bool))) {
    losers <- which(!all_models_bool)
    stop(paste(
      glue("Passed list must contain only model objects, but found {length(losers)} invalid objects at indices:"),
      paste(losers, collapse = ", ")
    ))
  }
  return(invisible(TRUE))
}

#' Private helper to check if an object inherits a run log class and error if not
#' @param .df The object to check
#' @keywords internal
check_bbi_run_log_df_object <- function(.df) {
  if (!inherits(.df, RUN_LOG_CLASS)) {
    stop(glue("Must pass a tibble of class `{RUN_LOG_CLASS}`, but got object of class: `{paste(class(.df), collapse = ', ')}`"))
  }
  return(invisible(TRUE))
}


############################
# Error handlers
############################

#' Raise error in strict mode
#'
#' Raises an error if `options("rbabylon.strict" = TRUE)` (recommended). Otherwise raises a warning.
#' These errors are used for things like type-checking which guarantees safe, predictable behavior of functions,
#' but theoretically advanced users or developers could want to turn them off.
#' @keywords internal
strict_mode_error <- function(err_msg) {
  if (isTRUE(getOption("rbabylon.strict"))) {
    stop(err_msg, call. = FALSE)
  } else {
    warning(paste(
      "The following error is being ignored because `options('rbabylon.strict')` is not set to TRUE.",
      "Consider setting `options('rbabylon.strict' = TRUE)` if you experience issues.",
      err_msg
    ), call. = FALSE)
  }
}

#' Build error message and throw error for passing character vector to get_... functions
#' @param .len The length of the vector that was passed.
#' @keywords internal
stop_get_scalar_msg <- function(.len) {
  stop(paste(
    glue("When passing character input to `rbabylon::get_...` functions, only scalar values are permitted. A vector of length {.len} was passed."),
    "Consider instead passing the tibble output from `run_log()`, or iterating with something like `purrr::map(your_vector, ~ get_...(.x)`"
  ))
}

#' Build error message and throw error for generic failure fo get_... functions
#' @param .bbi_object The object that something is attempting to be extracted from
#' @param .key The name of the field that is attempting to be extracted
#' @param .msg Character scalar or vector of more specific error messages to include at the end
#' @keywords internal
stop_get_fail_msg <- function(.bbi_object, .key, .msg = "") {
  stop(glue("Cannot extract `{.key}` from object of class `{paste(class(.bbi_object), collapse = ', ')}` :\n{paste(.msg, collapse = ', ')}"), call. = FALSE)
}


#' Suppress a warning that matches `.regexpr`
#' @importFrom stringr str_detect
#' @param .expr Expression to run
#' @param .regexpr Regex to match against any generated warning. Warning will be suppressed if this matches the warning message.
#' @export
suppressSpecificWarning <- function(.expr, .regexpr) {
  withCallingHandlers({
    .expr
  }, warning=function(w) {
    if (stringr::str_detect(w$message, .regexpr))
      invokeRestart("muffleWarning")
  })
}

