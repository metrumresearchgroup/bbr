#' Checks that all passed NONMEM command line args are valid and formats
#' @param .args A named list of .args to check
#' @importFrom checkmate assert_list assert_class
#' @importFrom rlang is_bare_character
#' @importFrom purrr imap set_names
#' @return character string, output from format_cmd_args()
#' @export
check_nonmem_args <- function(.args) {
  # check that unique named list was passed
  tryCatch(
    checkmate::assert_list(.args, names="named", unique=TRUE),
    error = function(e) {
      err_msg <- paste("check_nonmem_args() takes a unique, named list:", e) ##### change check_nonmem_args()
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
#' @return character vector of
#' @export
format_cmd_args <- function(.args, .collapse = FALSE) {
  # check that unique named list was passed
  tryCatch(
    checkmate::assert_list(.args, names="named", unique=TRUE),
    error = function(e) {
      err_msg <- paste("format_cmd_args() takes a unique, named list:", e)
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

