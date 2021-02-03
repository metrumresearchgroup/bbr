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
#'   * <run>.stan
#'   * <run>-init.R
#'   * <run>-stanargs.R
#'
#' * The data files (see `.build_data` argument):
#'   * <run>-standata.R
#'   * <run>-standata.json
#'
#' @param .mod a `bbi_{.model_type}_model` or `bbi_{.model_type}_summary` object
#' @param .build_data **Only relevant to Stan models.** If `FALSE`, the default,
#'   check the hashes of the `-standata.R` and `-standata.json` against the
#'   hashes in `bbi_config.json`. If `TRUE`, run `-standata.R` and save the
#'   output to a temp file and check the hash of _the temp file_ against the
#'   `bbi_config.json` hash. The second option actually runs the code and,
#'   importantly, verifies that the input data to `-standata.R` has not changed
#'   either.
#' @param ... Arguments passed through (currently none).
#'
#' @return `TRUE` if everything is up to date, `FALSE` if anything has changed.
#'   Will also message the user about files that are out of date.
#'
#' @export
check_model_up_to_date <- function(.mod, .build_data = FALSE, ...) {
  UseMethod("check_model_up_to_date")
}

#' @rdname check_model_up_to_date
#' @export
check_model_up_to_date.bbi_nonmem_model <- function(.mod, .build_data = FALSE, ...) {
  if (!isFALSE(.build_data)) {
    warning("`.build_data` arg in `check_model_up_to_date()` is invalid for NONMEM models. Ignoring passed value.")
  }
  check_model_up_to_date_nonmem(.mod)
}

#' @rdname check_model_up_to_date
#' @export
check_model_up_to_date.bbi_nonmem_summary <- function(.mod, .build_data = FALSE, ...) {
  if (!isFALSE(.build_data)) {
    warning("`.build_data` arg in `check_model_up_to_date()` is invalid for NONMEM models. Ignoring passed value.")
  }
  check_model_up_to_date_nonmem(.mod)
}

#' @rdname check_model_up_to_date
#' @export
check_model_up_to_date.bbi_stan_model <- function(.mod, .build_data = FALSE, ...) {
  any(check_model_up_to_date_stan(.mod, .build_data))
}

#' @rdname check_model_up_to_date
#' @export
check_model_up_to_date.bbi_stan_summary <- function(.mod, .build_data = FALSE, ...) {
  any(check_model_up_to_date_stan(.mod, .build_data))
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
#'
#' @inheritParams check_model_up_to_date
#'
#' @return A (named) logical vector of length 2. The first element (named
#'   `"model"`) refers to the control stream. The second element (named
#'   `"data"`) refers to the data file. For both elements, they will be `TRUE`
#'   if nothing has changed, `FALSE` if anything has changed.
#' @keywords internal
check_model_up_to_date_nonmem <- function(.mod) {
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

  # build return value
  res <- !changed_files
  names(res) <- c("model", "data")

  return(res)
}


#' Private implementation to check that a Stan model is up-to-date
#'
#' Specifically, check that required files on disk have not changed since the
#' model was run. This is accomplished by taking their md5 hashes and comparing
#' it to the hashes stored in `bbi_config.json`. The details of which files are
#' "model files" and which are "data files" are detailed in the
#' [check_model_up_to_date()] docs.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom fs file_exists
#'
#' @inheritParams check_model_up_to_date
#'
#' @return A (named) logical vector of length 2. The first element (named
#'   `"model"`) refers to the model files mentioned above. The second element
#'   (named `"data"`) refers to the data files mentioned above. For both
#'   elements, they will be `TRUE` if nothing has changed, `FALSE` if anything
#'   has changed.
#' @keywords internal
check_model_up_to_date_stan <- function(.mod, .build_data = FALSE) {

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
  if (isTRUE(.build_data)) {
    stop("Not implemented")
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
    data = !any(changed_files[c(data_r_file, data_json_file)])
  )

  return(res)
}

