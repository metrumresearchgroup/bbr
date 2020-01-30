#' Checks that all passed NONMEM command line args are valid and formats
#' @param .args A named list of .args to check
#' @importFrom checkmate assert_list assert_class
#' @importFrom rlang is_bare_character
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
      err_msg <- paste(.n, "is not a valid argument for the `.args` list in submit_nonmem_model(). Valid arguments are:",
                       paste(names(NONMEM_ARGS), collapse=", "))
      return(err_msg)
    }

    # check type
    type_res <- checkmate::check_class(.v, ref$type)
    if (rlang::is_bare_character(type_res)) {
      err_msg <- paste0("`", .v, "` passed for arg `", .n, "` -- ", type_res)
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
#' @return Output list as specified above.
#' @export
parse_mod_yaml <- function(.path) {
  # load from file
  raw_yaml <- read_yaml(.path)

  # parse model path
  if (YAML_MOD_PATH %in% names(raw_yaml)) {
    yaml_list <- list(model_path = raw_yaml[[YAML_MOD_PATH]])
  } else {
    stop(paste0(
      "Model yaml must have a `", YAML_MOD_PATH, "` specified in it. ",
      .path, " has the following keys: ", paste(names(raw_yaml), collapse=", ")
      ))
  }

  # parse NONMEM args
  args_keys <- names(raw_yaml)[names(raw_yaml) %in% names(NONMEM_ARGS)]
  ##### SOME LOGGING OF args_keys?
  yaml_list$args_list <- raw_yaml[args_keys]

  # parse user data
  user_keys <- names(raw_yaml)[!(names(raw_yaml) %in% c("model_path", args_keys))]
  ##### SOME LOGGING OF user_keys?
  yaml_list$user_data <- raw_yaml[user_keys]

  return(yaml_list)
}


#' DOES SOMETHING WITH USER DATA FROM YAML. NOT SURE WHAT THIS WILL BE YET.
#' @param .func_args A named list of arguments for bbi, passed into submit model function call
#' @param .yaml_args A named list of arguments for bbi, parsed from user input yaml
#' @importFrom purrr list_modify
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
    # overwrite anything from .args list that's specified in yaml
    .args <- list_modify(.func_args, .yaml_args)
  }
  return(.args)
}


#' DOES SOMETHING WITH USER DATA FROM YAML. NOT SURE WHAT THIS WILL BE YET.
#' @param .user_data A list, parsed from user input yaml, that will be persisted
#' @importFrom jsonlite toJSON
#' @importFrom checkmate assert_list
#' @export
parse_user_data <- function(.user_data) {
  if (is.null(.user_data)) {
    invisible()
  } else {
    # check that unique named list was passed
    tryCatch(
      checkmate::assert_list(.user_data, names="unique"),
      error = function(e) {
        err_msg <- paste("`.user_data` must be a unique, named list:", e)
        stop(err_msg)
      }
    )
  }

  # parse to json
  json_string <- toJSON(.user_data)

  # do something with the json
  cat(paste("parse_user_data() NOT FULLY IMPLEMENTED. Parsed json from .user_data:", json_string))
}
