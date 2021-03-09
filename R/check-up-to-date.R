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
#' **For Stan models**
#'
#' * The model files:
#'   * `<run>.stan`
#'   * `<run>-init.R`
#'   * `<run>-stanargs.R`
#'
#' * The data files (see `.build_data` argument):
#'   * `<run>-standata.R`
#'   * `<run>-standata.json`
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
check_up_to_date.bbi_nonmem_model <- function(.bbi_object, ...) {
  check_up_to_date_nonmem(.bbi_object)
}

#' @rdname check_up_to_date
#' @export
check_up_to_date.bbi_nonmem_summary <- function(.bbi_object, ...) {
  check_up_to_date_nonmem(.bbi_object)
}

#' @rdname check_up_to_date
#' @param .build_data **Only relevant to Stan models.** If `TRUE`, the default,
#'   run `-standata.R` and save the output to a temp file and check the hash of
#'   _the temp file_ against the `bbi_config.json` hash. This option actually
#'   runs the code and, importantly, verifies that the input data to
#'   `-standata.R` has not changed either. If `FALSE`, check the hashes of the
#'   `-standata.R` and `-standata.json` against the hashes in `bbi_config.json`
#'   but do _not_ run the `-standata.R` script. This option is less secure and
#'   primarily exists for quicker checking if building the data is time
#'   consuming for certain models.
#' @importFrom ellipsis check_dots_empty
#' @export
check_up_to_date.bbi_stan_model <- function(.bbi_object, .build_data = TRUE, ...) {
  ellipsis::check_dots_empty()
  check_up_to_date_stan(.bbi_object, .build_data)
}

#' @rdname check_up_to_date
#' @export
check_up_to_date.bbi_stan_summary <- function(.bbi_object, .build_data = TRUE, ...) {
  ellipsis::check_dots_empty()
  check_up_to_date_stan(.bbi_object, .build_data)
}

#' @export
check_up_to_date.bbi_log_df <- function(.bbi_object, .build_data = TRUE, ...) {

  check_list <- map(.bbi_object[[ABS_MOD_PATH]], function(.p) {
    tryCatch(
      check_up_to_date(read_model(.p), .build_data = .build_data),
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
    stop(paste(glue("Model {get_model_id(.mod)}:"), CHECK_UP_TO_DATE_ERR_MSG))
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

  return(invisible(res))
}

#' Private implementation to check that a Stan model is up-to-date
#'
#' Specifically, check that required files on disk have not changed since the
#' model was run. This is accomplished by taking their md5 hashes and comparing
#' it to the hashes stored in `bbi_config.json`. The details of which files are
#' "model files" and which are "data files" are detailed in the
#' [check_up_to_date()] docs.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom fs file_exists
#'
#' @inheritParams check_up_to_date
#'
#' @return A (named) logical vector of length 2. The first element (named
#'   `"model"`) refers to the model files mentioned above. The second element
#'   (named `"data"`) refers to the data files mentioned above. For both
#'   elements, they will be `TRUE` if nothing has changed, `FALSE` if anything
#'   has changed.
#' @keywords internal
check_up_to_date_stan <- function(.mod, .build_data = FALSE) {

  # check model and load config
  check_stan_model(.mod, .error = TRUE)

  config_path <- file.path(get_output_dir(.mod, .check_exists = FALSE), "bbi_config.json")
  if (!fs::file_exists(config_path)) {
    stop(glue("Cannot check if {get_model_id(.mod)} is up-to-date because it has not been run yet."))
  }
  config <- jsonlite::fromJSON(config_path)

  # check necessary files for changes
  changed_files <- c()

  stan_file <- get_model_path(.mod)
  changed_files <- c(
    changed_files,
    config[[CONFIG_MODEL_MD5]] != tools::md5sum(stan_file)
  )

  init_file <- build_path_from_model(.mod, STANINIT_SUFFIX)
  changed_files <- c(
    changed_files,
    config[[STANCFG_INIT_MD5]] != tools::md5sum(init_file)
  )

  args_file <- build_path_from_model(.mod, STANARGS_SUFFIX)
  changed_files <- c(
    changed_files,
    config[[STANCFG_ARGS_MD5]] != tools::md5sum(args_file)
  )

  data_r_file <- build_path_from_model(.mod, STANDATA_R_SUFFIX)
  data_json_file <- build_path_from_model(.mod, STANDATA_JSON_SUFFIX)
  temp_data_name <- as.character(glue("Running {basename(data_r_file)} produces different results"))
  if (isTRUE(.build_data)) {
    # if building data, run -standata.R and write output to temp file, then check that file
    temp_data_path <- tempfile()
    suppressMessages(
      standata_to_json(.mod, .out_path = temp_data_path)
    )

    changed_files <- c(
      changed_files,
      config[[CONFIG_DATA_MD5]] != tools::md5sum(data_json_file),
      config[[CONFIG_DATA_MD5]] != tools::md5sum(temp_data_path)
    )
    names(changed_files)[length(changed_files)] <- temp_data_name
  } else {
    changed_files <- c(
      changed_files,
      config[[STANCFG_DATA_MD5]] != tools::md5sum(data_r_file),
      config[[CONFIG_DATA_MD5]] != tools::md5sum(data_json_file)
    )
  }

  any_changes <- any(changed_files)

  if(isTRUE(any_changes)) {
    message(paste(
      glue("The following files have changed in {get_model_id(.mod)}"),
      paste("*", names(which(changed_files)), collapse = "\n"),
      sep = "\n"
    ))
  }

  # build return value
  res <- c(
    model = !any(changed_files[c(stan_file, init_file, args_file)]),
    data = !any(changed_files[c(data_r_file, data_json_file, temp_data_name)], na.rm = TRUE)
  )

  return(invisible(res))
}

