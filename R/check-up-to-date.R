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
#' **Currently only NONMEM implemented.**
#'
#' @param .mod a `bbi_{.model_type}_model` or `bbi_{.model_type}_summary` object
#' @param ... Arguments passed through (currently none).
#'
#' @return A (named) logical vector of length 2. The first element (named
#'   `"model"`) refers to the model files mentioned above. The second element
#'   (named `"data"`) refers to the data files mentioned above. For both
#'   elements, they will be `TRUE` if nothing has changed, `FALSE` if anything
#'   has changed. Note: _if no file exists_ at the specified path, `FALSE` will be
#'   returned because that is technically a "change." The file used to exist and
#'   now it does not.
#'
#' @export
check_up_to_date <- function(.mod, ...) {
  UseMethod("check_up_to_date")
}

#' @rdname check_up_to_date
#' @export
check_up_to_date.bbi_nonmem_model <- function(.mod, ...) {
  check_up_to_date_nonmem(.mod)
}

#' @rdname check_up_to_date
#' @export
check_up_to_date.bbi_nonmem_summary <- function(.mod, ...) {
  check_up_to_date_nonmem(.mod)
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
    stop(glue("Cannot check if {get_model_id(.mod)} is up-to-date because it has not been run yet."))
  }
  config <- jsonlite::fromJSON(config_path)

  # check necessary files for changes
  model_file <- get_model_path(.mod)
  data_file <- get_data_path(.mod)

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

  return(res)
}
