# S3 dispatch for submitting models
#' S3 generic method for submit_model
#' @export
#' @param .spec S3 object of class `bbi_{.model_type}_spec` to submit
#' @param ... additional args to pass to specific implementation
#' @rdname submit_model
submit_model <- function(.spec, ...) {
  UseMethod("submit_model", .spec)
}

#' S3 dispatch for submit_model from bbi_nonmem_spec
#' @export
#' @param .spec S3 object of class `bbi_nonmem_spec` to submit
#' @param ... additional args to pass to submit_nonmem_model()
#' @rdname submit_model
submit_model.bbi_nonmem_spec <- function(.spec, ...) {
  res <- submit_nonmem_model(.spec, ...)
  return(res)
}

#' S3 dispatch for submit_model from character scaler
#'   Should be path to yaml (with or without .yaml extension), or a valid model file (control stream, etc.).
#' @export
#' @param .path Path to YAML or model file
#' @param ... additional args to pass to specific implementation
#' @rdname submit_model
submit_model.character <- function(.path, ...) {
  ## parse type of file passed
  # if no extension, assume YAML
  if (tools::file_path_sans_ext(.path) == .path) {
    if (fs::file_exists(yaml_ext(.path))) {
      .spec <- create_model_from_yaml(yaml_ext(.path))
    } else {
      stop(glue("Cannot find file {yaml_ext(.path)}. If passing a non-YAML file to submit_model you must include the file extension."))
    }
  } else if (is_valid_yaml_extension(.path)) {
    .spec <- create_model_from_yaml(.path)
  } else if (is_valid_nonmem_extension(.path)) {
    .mod_list <- list()
    .mod_list[[YAML_MOD_PATH]] <- basename(.path)
    .mod_list[[WORKING_DIR]] <- normalizePath(dirname(.path))
    .mod_list[[YAML_MOD_TYPE]] <- "nonmem"
    .mod_list[[YAML_DESCRIPTION]] <- as.character(glue("{.path} passed directly to submit_model()"))
    .spec <- create_spec(.mod_list)
  } else {
    stop(glue("Unsupported file type passed to submit_model(): `{.path}`. Valid options are `.yaml`, `.mod`, and `.ctl`"))
  }

  # submit model
  .model_type <- .spec[[YAML_MOD_TYPE]]
  if (.model_type == "nonmem") {
    res <- submit_nonmem_model(.spec, ...)
  } else if (.model_type == "stan") {
    stop(NO_STAN_ERR_MSG)
  } else {
    stop(glue("Passed `{.model_type}`. Valid options: `{SUPPORTED_MOD_TYPES}`"))
  }
  return(res)
}


#' Submit a NONMEM model via babylon
#' @param .spec An S3 object of class `bbi_nonmem_spec`, for example from `create_model()` or `copy_model_from()`, etc.
#' @param .mode Either "local" for local execution or "sge" to submit model(s) to the grid
#' @param .bbi_args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`. Run `print_nonmem_args()` to see valid arguments.
#' @param ... args passed through to `bbi_exec()`
#' @param .config_path Optionally specify a path to a babylon.yml config. If not specified, the config in the model directory will be used by default. Path MUST be either an absolute path or relative to the model directory.
#' @param .wait Boolean for whether to wait for the bbi process to return before this function call returns.
#' @param .dry_run Returns an object detailing the command that would be run, insted of running it. This is primarily for testing but also a debugging tool.
#' @importFrom stringr str_detect
#' @importFrom tools file_path_sans_ext
#' @return An S3 object of class `bbi_nonmem_result` (or `bbi_nonmem_spec` if .dry_run=T)
#' @export
#' @rdname submit_model
submit_nonmem_model <- function(.spec,
                                .mode = c("sge", "local"),
                                .bbi_args = NULL,
                                ...,
                                .config_path=NULL,
                                .wait = TRUE,
                                .dry_run=FALSE) {

  # check for valid type arg
  .mode <- match.arg(.mode)

  # parse output directory path
  .spec[[YAML_OUT_DIR]] <- tools::file_path_sans_ext(.spec[[YAML_MOD_PATH]])

  # build command line args
  .bbi_args <- parse_args_list(.bbi_args, .spec[[YAML_BBI_ARGS]])
  args_vec <- check_nonmem_args(.bbi_args)
  cmd_args <- c("nonmem", "run", .mode, .spec[[YAML_MOD_PATH]], args_vec)

  # add config path
  if (!is.null(.config_path)) {
    cmd_args <- c(cmd_args, sprintf("--config=%s", .config_path))
  }

  # execute
  model_dir <- .spec[[WORKING_DIR]]

  if (.dry_run) {
    # construct fake res object
    call_str <- paste(
      "cd", model_dir, ";",
      getOption("rbabylon.bbi_exe_path"),
      paste(cmd_args, collapse = " ")
    )
    res <- list(cmd_args = cmd_args, call = call_str)
  } else {
    # launch model
    res <- bbi_exec(cmd_args, .wait = .wait, wd = model_dir, ...)
    class(res) <- c("bbi_nonmem_result", class(res))
  }

  # add to result object
  res <- combine_list_objects(res, .spec)

  return(res)
}
