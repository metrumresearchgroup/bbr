`%||%` <- function(x, y) {
  if (is.null(x)) return(y)
  return(x)
}
#' Checks that all passed NONMEM command line args are valid and formats
#' @param .args A named list of .args to check
#' @importFrom checkmate assert_list
#' @importFrom rlang is_bare_character is_bare_numeric is_bare_logical
#' @importFrom purrr imap set_names
#' @return character string, output from format_cmd_args()
#' @export
check_nonmem_args <- function(.args) {
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

  # check against NONMEM_ARGS
  err_vec <- imap(.args, function(.v, .n) {
    # check that arg is valid
    ref <- NONMEM_ARGS[[.n]]
    if (is.null(ref)) {
      err_msg <- paste(.n, "is not a valid argument for the `.args` list in submit_nonmem_model(). Run `print_nonmem_args()` to see valid arguments.")
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
      stop(paste0("BUG!!! NONMEM_ARGS[['", .n, "']][['type']] is `", ref$type, "` which is invalid. Must be either 'character', 'logical', or 'numeric'"))
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
      "There are ", length(err_vec), " errors in check_nonmem_args(): ", paste(err_vec, collapse = " :: ")
    )
    stop(err_msg)
  }

  # format to key value pairs with command line flags
  key_value_flags <- unlist(imap(.args, function(.v, .n) {NONMEM_ARGS[[.n]]$flag}))
  key_value_list <- imap(.args, function(.v, .n) {.v}) %>% set_names(key_value_flags)

  # format list to character vector
  return(format_cmd_args(key_value_list))
}

#' Formats command line args from a named list to a string as it would be passed on the command line
#' @param .args A named list of .args to check
#' @param .collapse Boolean for whether to collapse return vector to a single string (FALSE by default)
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


#' Combines NONMEM args that were passed into the function call with args that were parsed from a model yaml
#' @param .func_args A named list of arguments for bbi, passed into submit_model function call
#' @param .yaml_args A named list of arguments for bbi, parsed from user input yaml
#' @importFrom checkmate assert_list
#' @return The combination of the two lists, with .func_args overwriting any keys that are shared
#' @export
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
#' @export
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


#' Prints all valid arguments to pass in submit_nonmem_model(.args=list())
#' @importFrom purrr imap
#' @export
print_nonmem_args <- function() {
  doc_list <- imap(NONMEM_ARGS, function(.v, .n) {
    paste0(.n, " (", .v$type, ") -- ", .v$description, " (sets CLI flag `", .v$flag, "`)")
  })
  cat(paste(doc_list, collapse = "\n"))
}


#' Helper to strip path and extension from model file to get only model identifier
#' @param .mod_path Character scaler model path to strip
#' @importFrom tools file_path_sans_ext
#' @returns Character scaler with only model identifier
#' @rdname get_mod_id
#' @export
get_mod_id <- function(.mod_path, ...) {
  UseMethod("get_mod_id", .mod_path)
}

# S3 dispatch to get model identifier from file path to model
#' @rdname get_mod_id
#' @export
get_mod_id.character <- function(.mod_path) {
  return(basename(tools::file_path_sans_ext(.mod_path)))
}

#' S3 dispatch to get model identifier from
#' @rdname get_mod_id
#' @export
get_mod_id.bbi_nonmem_model <- function(.mod) {
  return(basename(tools::file_path_sans_ext(.mod[[YAML_MOD_PATH]])))
}


check_required_keys <- function(.list, .req) {
  all(.req %in% names(.list))
}

scaler_to_list <- function(.x) {
  if (length(.x) == 1) {
    .x <- (list(.x))
  }
  return(.x)
}

is_valid_nonmem_extension <- function(.path) {
  tools::file_ext(.path) %in% c("ctl", "mod")
}

is_valid_yaml_extension <- function(.path) {
  tools::file_ext(.path) %in% c("yaml", "yml")
}

# helpers that strip extension then add a diffent type
ctl_ext <- function(.x) {
  sprintf("%s.ctl", tools::file_path_sans_ext(.x))
}

mod_ext <- function(.x) {
  sprintf("%s.mod", tools::file_path_sans_ext(.x))
}

yaml_ext <- function(.x) {
  if (tools::file_ext(.x) == "yml") {
    return(.x)
  }
  sprintf("%s.yaml", tools::file_path_sans_ext(.x))
}

#' Builds the absolute path to file in the output directory from components of the `bbi_{.model_type}_model` object
#' @param .mod `bbi_{.model_type}_model` object
#' @param .extension file extension to append (for example `lst`, `ext`, `grd`, etc.)
build_path_from_mod_obj <- function(.mod, .extension) {
  ext_path <- file.path(.mod[[WORKING_DIR]],
                        .mod[[YAML_OUT_DIR]],
                        paste0(get_mod_id(.mod[[YAML_MOD_PATH]]), ".", .extension))
  return(ext_path)
}

# helper to find valid model file and return ctl_ext(.path) by default if not found
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

#' Returns the path to the model file from a `bbi_{.model_type}_model` object
#' @param .mod The `bbi_...` S3 object
#' @param .check_exists Boolean for whether it will check if the file exists and error if it does not. True by default.
#' @export
#' @rdname get_path_from_object
get_yaml_path <- function(.mod, .check_exists = TRUE) {
  # check if file name was stored on load, otherwise infer from model file
  if (is.null(.mod[[YAML_YAML_NAME]])) {
    yaml_file <- .mod[[YAML_MOD_PATH]] %>% get_mod_id() %>% yaml_ext()
  } else {
    yaml_file <- .mod[[YAML_YAML_NAME]]
  }

  yaml_path <- file.path(.mod[[WORKING_DIR]], yaml_file)
  if (.check_exists) {
    if (!fs::file_exists(yaml_path)) {
      stop(glue("Parsed YAML path {yaml_path} from .mod but no file exists at that location."))
    }
  }
  return(yaml_path)
}


strict_mode_error <- function(err_msg) {
  if (isTRUE(getOption("rbabylon.strict"))) {
    stop(err_msg)
  } else {
    warning(paste(
      "The following error is being ignored because `options('rbabylon.strict')` is not set to TRUE.",
      "Consider setting `options('rbabylon.strict' = TRUE)` if you experience issues.",
      err_msg
    ))
  }
}


#' Checks that object (list) has all the required elements to become an S3 object of class `bbi_{.model_type}_model` and then, if it does, assigns the class.
#' @param .mod_list The candidate list to assign the class to
#' @param .model_type The type of model
assign_model_class <- function(.mod_list, .model_type) {
  # check for required keys
  if (!check_required_keys(.mod_list, .req = MODEL_REQ_KEYS)) {
    err_msg <- paste0(
      "Model object must have the following named elements to be converted to an S3 object of class `bbi_{.model_type}_model`: `", paste(MODEL_REQ_KEYS, collapse=", "),
      "` but the following keys are missing: `", paste(MODEL_REQ_KEYS[!(MODEL_REQ_KEYS %in% names(.mod_list))], collapse=", "),
      "`\nObject has the following keys: ", paste(names(.mod_list), collapse=", ")
    )
    strict_mode_error(err_msg)
  }

  # assign class
  class(.mod_list) <- c(as.character(glue("bbi_{.model_type}_model")), class(.mod_list))

  return(.mod_list)
}

