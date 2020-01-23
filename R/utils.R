#' Checks status code from processx::run output
#' @param .path Full path to a model that has been run that user wants to summarize
#' @param .model_type Type of model to summarize. Currently only supports "nonmem"
#' @export
check_status_code <- function(.output) {
  if (.output$status != 0) {
    err_msg <- paste0(
      "`bbi ", paste(cmd_args, collapse=" "), .path, "` returned status code ", .output$status,
      " -- STDOUT: ", .output$stdout,
      " -- STDERR: ", .output$stderr
    )
    stop(err_msg)
  }
}


#' Checks that all passed NONMEM command line args are valid and formats
#' @param .args A named list of .args to check
#' @import checkmate
#' @import rlang
#' @import purrr
#' @return character string, output from format_cmd_args()
#' @export
check_nonmem_args <- function(.args) {
  # check that unique named list was passed
  tryCatch(
    checkmate::assert_list(.args, names="named", unique=TRUE),
    error = function(e) {
      err_msg <- paste("check_nonmem_args() takes a unique, named list:", e)
      stop(err_msg)
    }
  )

  # check against nonmen_args option
  nm_args <- getOption("rbabylon.nonmen_args")
  err_vec <- purrr::imap(.args, function(.v, .n) {
    # check that arg is valid
    ref <- nm_args[[.n]]
    if (is.null(ref)) {
      err_msg <- paste(.n, "is not a valid argument for the `.args` list in submit_nonmem_model(). Valid arguments are:",
                       paste(names(nm_args), collapse=", "))
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
  } else {
    return(format_cmd_args(.args))
  }
}

#' Formats command line args from a named list to a string as it would be passed on the command line
#' @param .args A named list of .args to check
#' @import rlang
#' @import checkmate
#' @import purrr
#' @return character string
#' @export
format_cmd_args <- function(.args) {
  # check that unique named list was passed
  tryCatch(
    checkmate::assert_list(.args, names="named", unique=TRUE),
    error = function(e) {
      err_msg <- paste("format_cmd_args() takes a unique, named list:", e)
      stop(err_msg)
    }
  )

  # build string from list
  purrr::imap(.args, function(.v, .n) {
    # if logical and TRUE add flag
    if (rlang::is_bare_logical(.v)) {
      if (.v) {
        return(sprintf("--%s", .n))
      } else {
        # should we return a warning here that passing FALSE does nothing?
        return(NULL)
      }
    }
    # otherwise add flag with value
    return(sprintf("--%s=%s", .n, .v))
  }) %>% unlist() %>%
    paste0(collapse = " ")
}


# test cases
##.args1 <- list("json" = T, "threads" = 4)
##.args2 <- list("json" = T, "threads" = 4, "naw" = "naw")
##.args3 <- list("json" = T, "threads" = 4, "debug" = "naw")
##.args4 <- list("json" = T, "threads" = 4, "debug" = F)
##check_nonmem_args(.args1) # "--json --threads=4"
##check_nonmem_args(.args2) # naw is not a valid argument for the `.args`
##check_nonmem_args(.args3) # `naw` passed for arg `debug` -- Must inherit from class 'logical'
##check_nonmem_args(.args4) # "--json --threads=4"

