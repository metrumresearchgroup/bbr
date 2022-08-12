#' Create bbi object
#'
#' Constructor functions for objects of fundamental bbr classes.
#' @name create_bbi_object
NULL

#' Perform model-type specific setup
#'
#' This method is called by `create_model_object()` after it has created an
#' object of type `bbi_{type}_model`, and, if the caller requested writing the
#' model yaml to disk, this method will be called before that.
#'
#' Every `bbi_{type}_model` should define a `create_model_hook.bbi_{type}_model`
#' method, even if no model-specific setup is required.
#'
#' @param .mod Model object.
#' @param ... All but the first argument that was passed to
#'   `create_model_object()`.
#'
#' @export
create_model_hook <- function(.mod, ...) {
  UseMethod("create_model_hook")
}

#' @rdname create_model_hook
#' @export
create_model_hook.default <- function(.mod, ...) {
  stop("Not a valid model type: ", .mod[[YAML_MOD_TYPE]])
}

#' @rdname create_model_hook
#' @export
create_model_hook.bbi_nonmem_model <- function(.mod, ...) {
  # we won't know the model file extension, so we rely on this helper to check
  # the possible extensions and throw an error if none exists
  find_nonmem_model_file_path(.mod[[ABS_MOD_PATH]], .check_exists = TRUE)
}

#' @describeIn create_bbi_object Creates list object of class `bbi_{.model_type}_model` from named list with `MODEL_REQ_INPUT_KEYS`
#' @param res List to attempt to assign the class to
#' @param save_yaml Logical scalar for whether to save the newly created model object to its corresponding YAML file and update the md5 hash.
#' @importFrom fs path_rel
#' @importFrom rlang %||%
#' @keywords internal
create_model_object <- function(res, save_yaml) {

  if(!inherits(res, "list")) {
    stop(glue("Can only create model object from a named list. Passed object has classes {paste(class(res), collapse = ', ')}"))
  }

  # check that necessary keys are present
  if (!check_required_keys(res, .req = MODEL_REQ_INPUT_KEYS)) {
    err_msg <- paste0(
      "Model list must have keys `", paste(MODEL_REQ_INPUT_KEYS, collapse=", "), "` specified in order to create model. ",
      "But `", paste(MODEL_REQ_INPUT_KEYS[!(MODEL_REQ_INPUT_KEYS %in% names(res))], collapse=", "), "` are missing. ",
      "List has the following keys: ", paste(names(res) %||% "NO NAMES", collapse=", ")
    )
    strict_mode_error(err_msg)
  }

  # assign class and write YAML to disk
  .model_type <- res[[YAML_MOD_TYPE]]

  # assign class
  class(res) <- c(as.character(glue("bbi_{.model_type}_model")),
                  BBI_BASE_MODEL_CLASS, BBI_PARENT_CLASS, class(res))

  # look for appropriate model files on disk and then write out YAML.
  # must be done AFTER assigning the class so that associated helpers can dispatch correctly
  create_model_hook(res, save_yaml)

  if(isTRUE(save_yaml)) {
    res <- save_model_yaml(res)
  }

  # check for required keys, just as an extra safety precaution before returning
  if (!check_required_keys(res, .req = MODEL_REQ_KEYS)) {
    err_msg <- paste0(
      "Model object must have the following named elements to be converted to an S3 object of class `bbi_{.model_type}_model`: `", paste(MODEL_REQ_KEYS, collapse=", "),
      "` but the following keys are missing: `", paste(MODEL_REQ_KEYS[!(MODEL_REQ_KEYS %in% names(res))], collapse=", "),
      "`\nObject has the following keys: ", paste(names(res), collapse=", ")
    )
    strict_mode_error(err_msg)
  }

  return(res)
}

#' @describeIn create_bbi_object Create list object of `bbi_{.model_type}_summary` class, first checking that all the required keys are present.
#' @param .model_type Character scalar of a valid model type
#' @importFrom purrr map map_if
#' @keywords internal
create_summary_object <- function(res, .model_type) {

  if(!inherits(res, "list")) {
    stop(glue("Can only summary object from a named list. Passed object has classes {paste(class(res), collapse = ', ')}"))
  }

  # Overwrite cases of BBI_NULL_NUM to NA_real_
  res <- map_list_recursive(res, set_bbi_null)

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
  class(res) <- c(as.character(glue("bbi_{.model_type}_summary")), BBI_PARENT_CLASS, class(res))
  return(res)
}


#' @describeIn create_bbi_object Create list output from [model_summaries()]. Each element will contain a `bbi_{.model_type}_summary` object, as well as some metadata about possible errors.
#' @keywords internal
create_summary_list <- function(res) {

  if(!inherits(res, "list")) {
    stop(glue("Can only pass a list. Passed object has classes {paste(class(res), collapse = ', ')}"))
  }

  # check for required keys, just as an extra safety precaution
  sum_bool <- map_lgl(res, check_required_keys, .req = SUMMARY_LIST_REQ_KEYS)

  if (!all(sum_bool)) {
    err_msg <- paste0(
      glue("Each element of summary list object must have the following named elements to be converted to an S3 object of class `bbi_summary_list`: `"), paste(SUMMARY_LIST_REQ_KEYS, collapse=", "),
      "`\nThe following elements have missing keys: ", paste(which(!sum_bool), collapse=", ")
    )
    strict_mode_error(err_msg)
  }

  # assign class and return
  class(res) <- c(as.character("bbi_summary_list"), class(res))
  return(res)
}


#' @describeIn create_bbi_object Create list object of `bbi_process` class, first checking that all the required keys are present.
#' @keywords internal
create_process_object <- function(res) {

  if(!inherits(res, "list")) {
    stop(glue("Can only process object from a named list. Passed object has classes {paste(class(res), collapse = ', ')}"))
  }

  # check for required keys, just as an extra safety precaution
  if (!check_required_keys(res, .req = PROCESS_REQ_KEYS)) {
    err_msg <- paste0(
      "Process object must have the following named elements to be converted to an S3 object of class `bbi_process`: `", paste(PROCESS_REQ_KEYS, collapse=", "),
      "` but the following keys are missing: `", paste(PROCESS_REQ_KEYS[!(PROCESS_REQ_KEYS %in% names(res))], collapse=", "),
      "`\nObject has the following keys: ", paste(names(res) %||% "NO NAMES", collapse=", ")
    )
    strict_mode_error(err_msg)
  }

  # assign class and return
  class(res) <- c(PROC_CLASS, class(res))
  return(res)
}

#' @describeIn create_bbi_object Create tibble object of `bbi_run_log_df` class, first checking that all the required columns are present.
#' @keywords internal
create_run_log_object <- function(log_df) {
  log_df <- create_log_df_impl(log_df, RUN_LOG_CLASS, RUN_LOG_REQ_COLS, ABS_MOD_PATH)
  return(log_df)
}

#' @describeIn create_bbi_object Create tibble object of `bbi_config_log_df` class, first checking that all the required columns are present.
#' @keywords internal
create_config_log_object <- function(log_df) {
  log_df <- create_log_df_impl(log_df, CONF_LOG_CLASS, CONFIG_LOG_REQ_COLS, ABS_MOD_PATH)
  return(log_df)
}

#' @describeIn create_bbi_object Create tibble object of `bbi_summary_log_df` class, first checking that all the required columns are present.
#' @keywords internal
create_summary_log_object <- function(log_df) {
  log_df <- create_log_df_impl(log_df, SUM_LOG_CLASS, SUMMARY_LOG_REQ_COLS, ABS_MOD_PATH)
  return(log_df)
}


#' @describeIn create_bbi_object Implementation function used to create objects of class `bbi_summary_log_df`, `bbi_config_log_df`, and `bbi_summary_log_df`
#' @param log_df data.frame or tibble to attempt to assign the class to
#' @param .class Character scalar of the class to assign
#' @param .req_cols Character vector of required columns
#' @param .key Character scalar of column name that will be treated as a primary key. Must be a type character, unique, and have no NA's or NULL's.
#' @keywords internal
create_log_df_impl <- function(log_df, .class, .req_cols, .key) {

  if(!inherits(log_df, "data.frame")) {
    stop(glue("Can only create `{.class}` object from a data.frame. Passed object has classes {paste(class(log_df), collapse = ', ')}"))
  }

  # check for required keys, just as an extra safety precaution
  if (!check_required_keys(log_df, .req = .req_cols)) {
    err_msg <- paste0(
      glue("data.frame must have the following columns to be converted to an S3 object of class `{.class}`: `"), paste(.req_cols, collapse=", "),
      "`\n\nThe following keys are missing: `", paste(.req_cols[!(.req_cols %in% names(log_df))], collapse=", "),
      "`\n\ndata.frame has the following columns: ", paste(names(log_df), collapse=", "), "\n"
    )
    strict_mode_error(err_msg)
  }

  # absolute_model_path column must be character, have none missing, and be unique
  if (!inherits(log_df[[.key]], "character")) {
    stop(glue("`{.key}` column must be character type, but has class {paste(class(log_df[[.key]]), collapse = ', ')}"))
  }

  if (any(is.na(log_df[[.key]])) || any(is.null(log_df[[.key]]))) {
    stop(glue("`{.key}` column must NOT have any NA or NULL values, but the following rows are either NA or NULL: {paste(c(which(is.na(log_df[[.key]])), which(is.null(log_df[[.key]]))), collapse = ', ')}"))
  }

  if(any(duplicated(log_df[[.key]]))) {
    err_msg <- paste(
      glue("`{.key}` column must contain unique values, but the following rows are duplicates:"),
      paste(which(duplicated(log_df[[.key]])), collapse = ', ')
    )
    dev_error(err_msg)
  }

  # assign class and return
  class(log_df) <- c(.class, LOG_DF_CLASS, class(log_df))
  return(log_df)
}
