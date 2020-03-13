# Constructor functions for objects of fundamental rbabylon classes


#' Creates S3 object of class `bbi_{.model_type}_model` from list with `MODEL_REQ_INPUT_KEYS`
#' @param .mod_list List with the required information to create a model object
#' @return S3 object of class `bbi_{.model_type}_model` that can be passed to `submit_model()`, `model_summary()` etc.
#' @rdname create_object
create_model_object <- function(.mod_list) {
  # check that necessary keys are present
  if (!check_required_keys(.mod_list, .req = MODEL_REQ_INPUT_KEYS)) {
    err_msg <- paste0(
      "Model list must have keys `", paste(MODEL_REQ_INPUT_KEYS, collapse=", "), "` specified in order to create model. ",
      "But `", paste(MODEL_REQ_INPUT_KEYS[!(MODEL_REQ_INPUT_KEYS %in% names(.mod_list))], collapse=", "), "` are missing. ",
      "List has the following keys: ", paste(names(.mod_list), collapse=", ")
    )
    strict_mode_error(err_msg)
  }

  .model_type <- .mod_list[[YAML_MOD_TYPE]]
  if (!(.model_type %in% SUPPORTED_MOD_TYPES)) {
    stop(glue("Invalid {YAML_MOD_TYPE} `{.model_type}`. Valid options include: `{SUPPORTED_MOD_TYPES}`"))
  }

  # by default, if no model defined, will use the YAML path to look for a model and set to .ctl if none found
  if (is.null(.mod_list[[YAML_MOD_PATH]])) {
    if (!is.null(.mod_list[[YAML_YAML_NAME]])) {
      .mod_list[[YAML_MOD_PATH]] <- file.path(.mod_list[[WORKING_DIR]], .mod_list[[YAML_YAML_NAME]]) %>% find_model_file_path()
    } else {
      stop("Must specify either a YAML_MOD_PATH or YAML_YAML_NAME to create a model. User should never see this error.")
    }
  } else if (.mod_list[[YAML_MOD_TYPE]] == "nonmem" && (!is_valid_nonmem_extension(.mod_list[[YAML_MOD_PATH]]))) {
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


#' Create object of `babylon_process` class
#'
#' First checks to be sure the required keys are all present
#' @param res List to attempt to assign the class to
#' @rdname create_object
create_process_object <- function(res) {

  # check for required keys, just as an extra safety precaution
  if (!check_required_keys(res, .req = PROCESS_REQ_KEYS)) {
    err_msg <- paste0(
      "Process object must have the following named elements to be converted to an S3 object of class `babylon_process`: `", paste(PROCESS_REQ_KEYS, collapse=", "),
      "` but the following keys are missing: `", paste(PROCESS_REQ_KEYS[!(PROCESS_REQ_KEYS %in% names(res))], collapse=", "),
      "`\nObject has the following keys: ", paste(names(res), collapse=", ")
    )
    strict_mode_error(err_msg)
  }

  # assign class
  class(res) <- c("babylon_process", class(res))

  return(res)
}

