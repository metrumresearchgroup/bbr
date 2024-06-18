#' Check model and data up to date with outputs
#'
#' Functions for checking that the model outputs on disk match the md5 hashes
#' stored in `bbi_config.json` at run time. In other words, checking that the
#' model and data files have _not_ changed since the model was last run. See
#' Details section for what specific files are checked.
#'
#' @details
#' Different files are checked depending on what type of model is being checked.
#'
#' **For NONMEM models**
#'
#' * The model file (control stream)
#'
#' * The data file (referenced in `$DATA` within the control stream)
#'
#' **For bootstrap runs**
#'
#' * The model file (control stream)
#'
#' * The original data file used in the setup of the bootstrap run (referenced
#'  in `$DATA` within the control stream)
#'
#' @param .bbi_object the object to check. Could be
#'   a `bbi_{.model_type}_model` object,
#'   a `bbi_{.model_type}_summary` object,
#'   or a `bbi_log_df` tibble.
#' @param ... Arguments passed through (currently none).
#'
#' @return
#' **The returned value is invisible because a message is printed** alerting the
#' user of the specific files that have changed, if any. This facilitates
#' calling the function for this side effect without explicitly handling the
#' returned value.
#'
#' **`bbi_model`** method invisibly returns a logical vector of length 2. The
#' first element (named `"model"`) refers to the model files mentioned in Details.
#' The second element (named `"data"`) refers to the data files mentioned in Details.
#' For both elements, they will be `TRUE` if nothing has changed, `FALSE` if
#' anything has changed. Note: _if no file exists_ at the specified path,
#' `FALSE` will be returned because that is technically a "change." The file
#' used to exist and now it does not.
#'
#' **`bbi_log_df`** method invisibly returns a named list of lists, with one
#' element for each row in the input tibble, with the name corresponding to the
#' value in the `run` column for that row. Each element of the list will contain
#' the two-element list returned from the `bbi_model` method (described above)
#' for the relevant model.
#'
#' There is no `add_up_to_date()` function because **if you would like to add
#' these columns to a `bbi_log_df` tibble** you can use [add_config()], which
#' contains `model_has_changed` and `data_has_changed` columns. Please note:
#' these contain the opposite boolean values (`check_up_to_date()` returns
#' `TRUE` if up to date, `*_has_changed` returns `TRUE` if _changed_).
#'
#' @export
check_up_to_date <- function(.bbi_object, ...) {
  UseMethod("check_up_to_date")
}

#' @rdname check_up_to_date
#' @export
check_up_to_date.bbi_base_model <- function(.bbi_object, ...) {
  check_up_to_date_nonmem(.bbi_object)
}

#' @rdname check_up_to_date
#' @export
check_up_to_date.bbi_nmboot_model <- function(.bbi_object, ...) {
  output_dir <- get_output_dir(.bbi_object, .check_exists = FALSE)
  if (!fs::dir_exists(output_dir)) {
    rlang::abort(
      c(
        paste(
          glue("Model {get_model_id(.bbi_object)}:"),
          "Cannot check if up-to-date because bootstrap run has not been set up yet"
        ),
        "See `?setup_bootstrap_run` for more details"
      )
    )
  }

  if(!model_is_finished(.bbi_object)){
    rlang::abort(paste(glue("Model {get_model_id(.bbi_object)}:"), CHECK_UP_TO_DATE_ERR_MSG))
  }

  boot_spec <- get_boot_spec(.bbi_object)

  # check necessary files for changes
  model_file <- get_model_path(.bbi_object)
  based_on_data_file <- get_data_path(.bbi_object, .check_exists = FALSE)

  changed_files <- c(
    boot_spec[[CONFIG_MODEL_MD5]] != tools::md5sum(model_file),
    boot_spec[[CONFIG_DATA_BASED_ON_MD5]] != tools::md5sum(based_on_data_file)
  )

  any_changes <- any(changed_files)
  if(isTRUE(any_changes)) {
    message(paste(
      glue("The following files have changed in {get_model_id(.bbi_object)}"),
      paste("*", names(which(changed_files)), collapse = "\n"),
      sep = "\n"
    ))
  }

  na_files <- is.na(changed_files)
  if(isTRUE(any(na_files))) {
    message(paste(
      glue("The following files in {get_model_id(.bbi_object)} ARE NO LONGER PRESENT"),
      paste("*", names(changed_files[na_files]), collapse = "\n"),
      sep = "\n"
    ))
  }


  # build return value
  res <- replace_na(!changed_files, FALSE)
  names(res) <- c("model", "data")

  return(invisible(res))
}

#' @rdname check_up_to_date
#' @export
check_up_to_date.bbi_nonmem_summary <- function(.bbi_object, ...) {
  check_up_to_date_nonmem(.bbi_object)
}

#' @export
check_up_to_date.bbi_log_df <- function(.bbi_object, ...) {

  check_list <- map(.bbi_object[[ABS_MOD_PATH]], function(.p) {
    tryCatch(
      check_up_to_date(read_model(.p), ...),
      error = function(.e) {
        .error_msg <- paste(as.character(.e$message), collapse = " -- ")
        if (grepl(CHECK_UP_TO_DATE_ERR_MSG, .error_msg, fixed = TRUE)) {
          message(.error_msg)
          return(as.logical(c(model = NA, data = NA)))
        } else {
          stop(.e)
        }
      }
    )
  })

  names(check_list) <- .bbi_object[[RUN_ID_COL]]
  return(invisible(check_list))
}

####################################
# PRIVATE implementation functions
####################################


#' Private implementation to check that NONMEM model is up-to-date
#'
#' Specifically, check that control stream and data file on disk have not
#' changed since the model was run. This is accomplished by taking their md5
#' hashes and comparing it to the hashes stored in `bbi_config.json`.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom fs file_exists
#' @importFrom tidyr replace_na
#'
#' @inheritParams check_up_to_date
#'
#' @keywords internal
check_up_to_date_nonmem <- function(.mod) {
  config_path <- file.path(get_output_dir(.mod, .check_exists = FALSE), "bbi_config.json")
  if (!fs::file_exists(config_path)) {
    rlang::abort(paste(glue("Model {get_model_id(.mod)}:"), CHECK_UP_TO_DATE_ERR_MSG))
  }
  config <- jsonlite::fromJSON(config_path)

  # check necessary files for changes
  model_file <- get_model_path(.mod)
  data_file <- get_data_path(.mod, .check_exists = FALSE)

  changed_files <- c(
    config[[CONFIG_MODEL_MD5]] != tools::md5sum(model_file),
    config[[CONFIG_DATA_MD5]] != tools::md5sum(data_file)
  )

  any_changes <- any(changed_files)
  if(isTRUE(any_changes)) {
    message(paste(
      glue("The following files have changed in {get_model_id(.mod)}"),
      paste("*", names(which(changed_files)), collapse = "\n"),
      sep = "\n"
    ))
  }

  na_files <- is.na(changed_files)
  if(isTRUE(any(na_files))) {
    message(paste(
      glue("The following files in {get_model_id(.mod)} ARE NO LONGER PRESENT"),
      paste("*", names(changed_files[na_files]), collapse = "\n"),
      sep = "\n"
    ))
  }

  # build return value
  res <- replace_na(!changed_files, FALSE)
  names(res) <- c("model", "data")

  return(invisible(res))
}
