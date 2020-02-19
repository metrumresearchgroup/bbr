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
  if (is.null(.args)) {
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


#' Parses a model yaml file into a list object that contains correctly formatted information from the yaml
#' Output list will have:
#'   $model_path -- the path the model file that should be run
#'   $args_list -- any babylon arguments that should be passed though. The keys must be the same as would be passed to check_nonmem_args()
#'   $user_data -- any other data the user included, such as metadata about the model, that is NOT a babylon argument
#' @param .path Path to the yaml file to parse.
#' @importFrom yaml read_yaml
#' @importFrom fs file_exists
#' @return Output list as specified above.
#' @export
parse_mod_yaml <- function(.path) {
  if (!fs::file_exists(.path)) {
    stop(glue("Cannot find yaml file at path {.path}"))
  }

  # load from file
  yaml_list <- read_yaml(.path)

  # parse model path
  if (!check_mod_yaml_keys(yaml_list)) {
    stop(paste0(
      "Model yaml must have keys `", paste(YAML_REQ_KEYS, collapse=", "), "` specified in it. ",
      "But `", paste(YAML_REQ_KEYS[!(YAML_REQ_KEYS %in% names(yaml_list))], collapse=", "), "` are missing. ",
      .path, " has the following keys: ", paste(names(yaml_list), collapse=", ")
      ))
  }

  return(yaml_list)
}


#' Combines NONMEM args that were passed into the function call with args that were parsed from a model yaml
#' @param .func_args A named list of arguments for bbi, passed into submit model function call
#' @param .yaml_args A named list of arguments for bbi, parsed from user input yaml
#' @importFrom checkmate assert_list
#' @return The combination of the two lists, with .yaml_args overriding any keys that are shared
#' @export
parse_args_list <- function(.func_args, .yaml_args) {
  # if no .args passed to function, pass through yaml args
  if (is.null(.func_args)) {
    .args <- .yaml_args
  } else {
    # check that unique named list was passed
    tryCatch(
      checkmate::assert_list(.func_args, names="unique"),
      error = function(e) {
        err_msg <- paste("`.args` must be a unique, named list:", e)
        stop(err_msg)
      }
    )
    # add .yaml_args and overwrite anything from .func_args list that's in .yaml_args
    .args <- .func_args
    for (.n in names(.yaml_args)) {
      .args[[.n]] <- .yaml_args[[.n]]
    }
  }
  return(.args)
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
#' @param .mod_path model path to strip
#' @importFrom tools file_path_sans_ext
#' @returns Character scaler with only model identifier
#' @export
get_mod_id <- function(.mod_path) {
  return(basename(tools::file_path_sans_ext(.mod_path)))
}


check_mod_yaml_keys <- function(.list) {
  all(YAML_REQ_KEYS %in% names(.list))
}

scaler_to_list <- function(.x) {
  if (length(.x) == 1) {
    .x <- (list(.x))
  }
  return(.x)
}

