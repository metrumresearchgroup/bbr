# Constructor functions for objects of fundamental rbabylon classes


#' Creates S3 object of class `bbi_{.model_type}_model` from list with `MODEL_REQ_INPUT_KEYS`
#' @param .mod_list List with the required information to create a model object
#' @importFrom fs path_rel
#' @return S3 object of class `bbi_{.model_type}_model` that can be passed to `submit_model()`, `model_summary()` etc.
#' @rdname create_object
create_model_object <- function(.mod_list) {

  if(!inherits(.mod_list, "list")) {
    stop(glue("Can only create model object from a named list. Passed object has classes {paste(class(.mod_list), collapse = ', ')}"))
  }

  # check that necessary keys are present
  if (!check_required_keys(.mod_list, .req = MODEL_REQ_INPUT_KEYS)) {
    err_msg <- paste0(
      "Model list must have keys `", paste(MODEL_REQ_INPUT_KEYS, collapse=", "), "` specified in order to create model. ",
      "But `", paste(MODEL_REQ_INPUT_KEYS[!(MODEL_REQ_INPUT_KEYS %in% names(.mod_list))], collapse=", "), "` are missing. ",
      "List has the following keys: ", paste(names(.mod_list) %||% "NO NAMES", collapse=", ")
    )
    strict_mode_error(err_msg)
  }

  # check model type
  .model_type <- .mod_list[[YAML_MOD_TYPE]]
  if (!(.model_type %in% SUPPORTED_MOD_TYPES)) {
    stop(glue("Invalid {YAML_MOD_TYPE} `{.model_type}`. Valid options include: `{paste(SUPPORTED_MOD_TYPES, collapse = ', ')}`"))
  }

  # by default, if no model defined, will use the YAML path to look for a model and set to .ctl if none found
  if (is.null(.mod_list[[YAML_MOD_PATH]])) {
    if (is.null(.mod_list[[YAML_YAML_NAME]])) {
      stop("Must specify either a YAML_MOD_PATH or YAML_YAML_NAME to create a model. User should never see this error.")
    }
    .mod_path <- find_model_file_path(file.path(.mod_list[[WORKING_DIR]], .mod_list[[YAML_YAML_NAME]]))
    .mod_list[[YAML_MOD_PATH]] <- as.character(fs::path_rel(.mod_path, .mod_list[[WORKING_DIR]]))
  }

  # check for correct NONMEM extension
  if (.model_type == "nonmem" && (!is_valid_nonmem_extension(.mod_list[[YAML_MOD_PATH]]))) {
    stop(glue::glue("model_path defined in yaml at {.mod_list[[YAML_MOD_PATH]]} must have either a .ctl or .mod extension, but found {.mod_list[[YAML_MOD_PATH]]}"))
  }

  # check babylon args and add an empty list if missing
  if (is.null(.mod_list[[YAML_BBI_ARGS]])) {
    .mod_list[[YAML_BBI_ARGS]] <- list()
  } else {
    # check that unique named list was passed
    tryCatch(
      checkmate::assert_list(.mod_list[[YAML_BBI_ARGS]], names="unique"),
      error = function(e) { stop(glue("`{YAML_BBI_ARGS}` must be a unique, named list: {e}")) }
    )
  }

  # add output_dir
  if (!is.null(.mod_list[[YAML_BBI_ARGS]][["output_dir"]])) {
    # if specified in bbi_args, overwrite anything that's in YAML or list
    .mod_list[[YAML_OUT_DIR]] <- .mod_list[[YAML_BBI_ARGS]][["output_dir"]]
  } else if (is.null(.mod_list[[YAML_OUT_DIR]])) {
    # if null, infer from model path
    .mod_list[[YAML_OUT_DIR]] <- .mod_list[[YAML_MOD_PATH]] %>% tools::file_path_sans_ext()
  }

  # check for required keys, just as an extra safety precaution
  if (!check_required_keys(.mod_list, .req = MODEL_REQ_KEYS)) {
    err_msg <- paste0(
      "Model object must have the following named elements to be converted to an S3 object of class `bbi_{.model_type}_model`: `", paste(MODEL_REQ_KEYS, collapse=", "),
      "` but the following keys are missing: `", paste(MODEL_REQ_KEYS[!(MODEL_REQ_KEYS %in% names(.mod_list))], collapse=", "),
      "`\nObject has the following keys: ", paste(names(.mod_list), collapse=", ")
    )
    strict_mode_error(err_msg)
  }

  # assign class and return
  class(.mod_list) <- c(as.character(glue("bbi_{.model_type}_model")), class(.mod_list))
  return(.mod_list)
}

#' Create object of `bbi_{.model_type}_summary` class
#'
#' First checks to be sure the required keys are all present
#' @param res List to attempt to assign the class to
#' @param .model_type Character scaler of a valid model type (currently either `nonmem` or `stan`)
#' @rdname create_object
create_summary_object <- function(res, .model_type = SUPPORTED_MOD_TYPES) {

  if(!inherits(res, "list")) {
    stop(glue("Can only summary object from a named list. Passed object has classes {paste(class(res), collapse = ', ')}"))
  }

  .model_type <- match.arg(.model_type)

  # check for required keys, just as an extra safety precaution
  if (!check_required_keys(res, .req = SUMMARY_REQ_KEYS)) {
    err_msg <- paste0(
      glue("Summary object must have the following named elements to be converted to an S3 object of class `bbi_{.model_type}_summary`: `"), paste(SUMMARY_REQ_KEYS, collapse=", "),
      "` but the following keys are missing: `", paste(SUMMARY_REQ_KEYS[!(SUMMARY_REQ_KEYS %in% names(res))], collapse=", "),
      "`\nObject has the following keys: ", paste(names(res) %||% "NO NAMES", collapse=", ")
    )
    strict_mode_error(err_msg)
  }

  # assign class and return
  class(res) <- c(as.character(glue("bbi_{.model_type}_summary")), class(res))
  return(res)
}


#' Create object of `babylon_process` class
#'
#' First checks to be sure the required keys are all present
#' @param res List to attempt to assign the class to
#' @rdname create_object
create_process_object <- function(res) {

  if(!inherits(res, "list")) {
    stop(glue("Can only process object from a named list. Passed object has classes {paste(class(res), collapse = ', ')}"))
  }

  # check for required keys, just as an extra safety precaution
  if (!check_required_keys(res, .req = PROCESS_REQ_KEYS)) {
    err_msg <- paste0(
      "Process object must have the following named elements to be converted to an S3 object of class `babylon_process`: `", paste(PROCESS_REQ_KEYS, collapse=", "),
      "` but the following keys are missing: `", paste(PROCESS_REQ_KEYS[!(PROCESS_REQ_KEYS %in% names(res))], collapse=", "),
      "`\nObject has the following keys: ", paste(names(res) %||% "NO NAMES", collapse=", ")
    )
    strict_mode_error(err_msg)
  }

  # assign class and return
  class(res) <- c("babylon_process", class(res))
  return(res)
}


#' Create object of `bbi_run_log_df` class
#'
#' First checks to be sure the required columns are all present
#' @param log_df data.frame or tibble to attempt to assign the class to
#' @rdname create_object
create_run_log_object <- function(log_df) {

  if(!inherits(log_df, "data.frame")) {
    stop(glue("Can only create `bbi_run_log_df` object from a data.frame. Passed object has classes {paste(class(log_df), collapse = ', ')}"))
  }

  # check for required keys, just as an extra safety precaution
  if (!check_required_keys(log_df, .req = RUN_LOG_REQ_COLS)) {
    err_msg <- paste0(
      "data.frame must have the following columns to be converted to an S3 object of class `bbi_run_log_df`: `", paste(RUN_LOG_REQ_COLS, collapse=", "),
      "` but the following keys are missing: `", paste(RUN_LOG_REQ_COLS[!(RUN_LOG_REQ_COLS %in% names(log_df))], collapse=", "),
      "`\ndata.frame has the following columns: ", paste(names(log_df), collapse=", ")
    )
    strict_mode_error(err_msg)
  }

  # absolute_model_path column must be character, have none missing, and be unique
  if (!inherits(log_df[[ABS_MOD_PATH]], "character")) {
    stop(glue("`{ABS_MOD_PATH}` column must be character type, but has class {paste(class(log_df[[ABS_MOD_PATH]]), collapse = ', ')}"))
  }

  if (any(is.na(log_df[[ABS_MOD_PATH]]))) {
    stop(glue("`{ABS_MOD_PATH}` column must NOT have any NA values, but the following rows are NA: {paste(which(is.na(log_df[[ABS_MOD_PATH]])), collapse = ', ')}"))
  }

  if(any(duplicated(log_df[[ABS_MOD_PATH]]))) {
    stop(paste(
      glue("`{ABS_MOD_PATH}` column must contain unique values, but the following rows are duplicates:"),
      paste(which(duplicated(log_df[[ABS_MOD_PATH]])), collapse = ', '),
      "-- USER PROBABLY SHOULDN'T SEE THIS ERROR. This is more of a failsafe."
    ))
  }

  # assign class and return
  class(log_df) <- c("bbi_run_log_df", class(log_df))
  return(log_df)
}
