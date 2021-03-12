#' Build model input data
#'
#' Some types of models carry around code that builds the input data for the
#' modeling, instead of relying on a single static file on disk. When necessary,
#' **this function is called internally by [submit_model()]** to build the data
#' for running the model, but they can also be called manually by users for
#' other purposes.
#'
#' @details **Currently, only `bbi_stan_model` objects have this implemented.**
#' The `<run>-standata.R` file contains the code for building the Stan input
#' data.
#'
#' @return
#' Invisibly returns the data object returned from the relevant build function.
#' Also optionally saves the object to disk if a valid file path is passed to
#' `.out_path`.
#'
#' The data object is returned invisibly because sometimes you will want to call
#' this function solely for the side effect of writing the data to disk.
#'
#' @param .mod a `bbi_{.model_type}_model` object.
#' @param .out_path If `NULL`, the default, does not write any data to disk.
#'   Otherwise, pass a file path where the resulting data object should be
#'   written.
#' @param ... Arguments passed through to methods (currently none).
#'
#' @importFrom checkmate assert_string
#' @export
build_data <- function(.mod, .out_path = NULL, ...) {
  UseMethod("build_data")
}

#' @describeIn build_data executes the function in `<run>-standata.R` and
#'   returns the list that will be passed to the `data` argument of
#'   `cmdstanr::sample()`. Also optionally writes the returned data list to json
#'   with [cmdstanr::write_stan_json()] if an `.out_path` is passed.
#'
#' @importFrom stringr str_detect
#'
#' @export
build_data.bbi_stan_model <- function(.mod, .out_path = NULL, ...) {
  # source and call function
  standata_r_path <- build_path_from_model(.mod, STANDATA_R_SUFFIX)
  make_standata <- safe_source_function(standata_r_path, "make_standata")
  standata_list <- safe_call_sourced(
    .func = make_standata,
    .args = list(.dir = dirname(get_output_dir(.mod, .check_exists = FALSE))),
    .file = standata_r_path,
    .expected_class = "list"
  )

  # optionally write to json
  if (!is.null(.out_path)) {
    if (!requireNamespace("cmdstanr", quietly = TRUE)) {
      stop("Must have cmdstanr installed to use build_data.bbi_stan_model()")
    }

    checkmate::assert_string(.out_path)
    if (!str_detect(.out_path, ".json$")) {
      stop(glue("build_data.bbi_stan_model(.out_path) must end in '.json' because a JSON file will be written. Got {.out_path}"), .call = FALSE)
    }
    cmdstanr::write_stan_json(standata_list, .out_path)
  }

  return(invisible(standata_list))
}


###########################
# PRIVATE HELPER FUNCTIONS
###########################

#' Private helper to safely source a specific function from a file
#' @param .file (String) file path to file to source.
#' @param .func_name (String) name of function to return. A function with this
#'   name must exist in `.file` or an error will be thrown.
#' @keywords internal
safe_source_function <- function(.file, .func_name) {
  checkmate::assert_string(.file)
  checkmate::assert_string(.func_name)

  .env <- new.env()
  tryCatch(
    source(.file, local = .env),
    error = function(.e) {
      err_msg <- paste(
        glue("Loading `{.func_name}()` from {.file} FAILED with the following:"),
        .e$message
      )
      stop(err_msg, call. = FALSE)
    }
  )

  .func <- .env[[.func_name]]
  if(is.null(.func)) {
    stop(glue("{.file} must contain a function called `{.func_name}` but it does not."), call. = FALSE)
  }

  return(.func)
}

#' Private helper to safely call a function from a sourced file
#' @param .func The function to call
#' @param .args named list of args to call the function with (uses `do.call()`)
#' @param .file (optional) file path to file that was sourced (for friendlier
#'   error message)
#' @param .expected_class (optional) name of class that the result of calling
#'   `.func` should return. If _not_ `NULL` will error if the return value
#'   doesn't inherit this class.
#' @keywords internal
safe_call_sourced <- function(.func, .args, .file = NULL, .expected_class = NULL) {
  .res <- tryCatch(
    do.call(.func, .args),
    error = function(.e) {
      err_msg <- paste(
        glue("Calling `{as.character(substitute(.func))}()` from {.file} FAILED with the following:"),
        .e$message
      )
      stop(err_msg, call. = FALSE)
    }
  )

  if (!is.null(.expected_class) && !inherits(.res, .expected_class)) {
    err_msg <- paste(
      glue("The result of `{as.character(substitute(.func))}()` was expected to be {.expected_class} but got the following:"),
      paste(class(.res), collapse = ", ")
    )
    stop(err_msg, call. = FALSE)
  }

  return(.res)
}
