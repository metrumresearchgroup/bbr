# S3 dispatch for submitting models
#' S3 generic method for submit_model
#' @export
#' @param .mod S3 object of class `bbi_{.model_type}_model` to submit
#' @param ... additional args to pass to specific implementation
#' @rdname submit_model
submit_model <- function(.mod, ...) {
  UseMethod("submit_model", .mod)
}

#' S3 dispatch for submit_model from bbi_nonmem_model
#' @export
#' @param .mod S3 object of class `bbi_nonmem_model` to submit
#' @param ... additional args to pass to submit_nonmem_model()
#' @rdname submit_model
submit_model.bbi_nonmem_model <- function(.mod, ...) {
  res <- submit_nonmem_model(.mod, ...)
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
      .mod <- read_model(yaml_ext(.path))
    } else {
      stop(glue("Cannot find file {yaml_ext(.path)}. If passing a non-YAML file to submit_model you must include the file extension."))
    }
  } else if (is_valid_yaml_extension(.path)) {
    .mod <- read_model(.path)
  } else if (is_valid_nonmem_extension(.path)) {
    # if NONMEM file, build model object
    .mod_list <- list()
    .mod_list[[YAML_MOD_PATH]] <- basename(.path)
    .mod_list[[WORKING_DIR]] <- normalizePath(dirname(.path))
    .mod_list[[YAML_MOD_TYPE]] <- "nonmem"
    .mod_list[[YAML_DESCRIPTION]] <- as.character(glue("{.path} passed directly to submit_model()"))
    .mod <- create_model_object(.mod_list)
  } else {
    stop(glue("Unsupported file type passed to submit_model(): `{.path}`. Valid options are `.yaml`, `.mod`, and `.ctl`"))
  }

  # submit model
  .model_type <- .mod[[YAML_MOD_TYPE]]
  if (.model_type == "nonmem") {
    res <- submit_nonmem_model(.mod, ...)
  } else if (.model_type == "stan") {
    stop(NO_STAN_ERR_MSG)
  } else {
    stop(glue("Passed `{.model_type}`. Valid options: `{SUPPORTED_MOD_TYPES}`"))
  }
  return(res)
}


#' Submit a NONMEM model via babylon
#' @param .mod An S3 object of class `bbi_nonmem_model`, for example from `new_model()`, `read_model()` or `copy_model_from()`
#' @param .mode Either "local" for local execution or "sge" to submit model(s) to the grid
#' @param .bbi_args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`. Run `print_nonmem_args()` to see valid arguments.
#' @param ... args passed through to `bbi_exec()`
#' @param .config_path Optionally specify a path to a babylon.yml config. If not specified, the config in the model directory will be used by default. Path MUST be either an absolute path or relative to the model directory.
#' @param .wait Boolean for whether to wait for the bbi process to return before this function call returns.
#' @param .dry_run Returns an object detailing the command that would be run, insted of running it. This is primarily for testing but also a debugging tool.
#' @importFrom stringr str_detect
#' @importFrom tools file_path_sans_ext
#' @return An S3 object of class `bbi_nonmem_process` (or `bbi_nonmem_model` if .dry_run=T)
#' @rdname submit_model
submit_nonmem_model <- function(.mod,
                                .mode = c("sge", "local"),
                                .bbi_args = NULL,
                                ...,
                                .config_path=NULL,
                                .wait = TRUE,
                                .dry_run=FALSE) {

  # check for valid type arg
  .mode <- match.arg(.mode)

  # parse output directory path
  .mod[[YAML_OUT_DIR]] <- tools::file_path_sans_ext(.mod[[YAML_MOD_PATH]])

  # build command line args
  .bbi_args <- parse_args_list(.bbi_args, .mod[[YAML_BBI_ARGS]])
  args_vec <- check_nonmem_args(.bbi_args)
  cmd_args <- c("nonmem", "run", .mode, .mod[[YAML_MOD_PATH]], args_vec)

  # add config path
  if (!is.null(.config_path)) {
    cmd_args <- c(cmd_args, sprintf("--config=%s", .config_path))
  }

  # execute
  model_dir <- .mod[[WORKING_DIR]]

  if (.dry_run) {
    # construct fake res object
    res <- bbi_dry_run(cmd_args, model_dir)
  } else {
    # launch model
    res <- bbi_exec(cmd_args, .wait = .wait, .dir = model_dir, ...)
  }

  return(res)
}
