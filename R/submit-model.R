# S3 dispatch for submitting models
#' S3 generic method for submit_model
#' @param .mod generic model to submit
#' @param .mode Either "local" for local execution or "sge" to submit model(s) to the grid
#' @param .bbi_args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`. Run `print_nonmem_args()` to see valid arguments.
#' @param ... args passed through to `bbi_exec()`
#' @param .config_path Optionally specify a path to a babylon.yml config. If not specified, the config in the model directory will be used by default. Path MUST be either an absolute path or relative to the model directory.
#' @param .wait Boolean for whether to wait for the bbi process to return before this function call returns.
#' @param .dry_run Returns an object detailing the command that would be run, insted of running it. This is primarily for testing but also a debugging tool.
#' @param .directory Model directory which `.mod` path is relative to. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`. Only used when passing a path for `.mod` instead of a `bbi_{.model_type}_model` object.
#' @export
#' @rdname submit_model
submit_model <- function(
  .mod,
  .mode = c("sge", "local"),
  .bbi_args = NULL,
  ...,
  .config_path=NULL,
  .wait = TRUE,
  .dry_run=FALSE,
  .directory = NULL
) {
  UseMethod("submit_model")
}

#' S3 dispatch for submit_model from bbi_nonmem_model
#' @param .mod S3 object of class `bbi_nonmem_model` to submit
#' @param .directory This argument is only used when passing a path to `submit_model()`. When `bbi_nonmem_model` object is passed, `.directory` is inferred from the model object.
#' @export
#' @rdname submit_model
submit_model.bbi_nonmem_model <- function(
  .mod,
  .mode = c("sge", "local"),
  .bbi_args = NULL,
  ...,
  .config_path=NULL,
  .wait = TRUE,
  .dry_run=FALSE,
  .directory = NULL
) {

  if (!is.null(.directory)) {
    warning(paste(glue("Passed `.directory = {.directory}` to submit_model.bbi_nonmem_model().") ,
                  "This argument is only valid when passing a path to `submit_model()`.",
                  "`bbi_nonmem_model` object was passed, so `.directory` inferred from `.mod${WORKING_DIR}`"))
  }

  res <- submit_nonmem_model(.mod,
                             .mode = .mode,
                             .bbi_args = .bbi_args,
                             ...,
                             .config_path = .config_path,
                             .wait = .wait,
                             .dry_run = .dry_run)
  return(res)
}

#' S3 dispatch for submit_model from character scaler
#'   Should be path to yaml (with or without .yaml extension), or a valid model file (control stream, etc.).
#' @param .mod Path to YAML or model file
#' @param .directory Model directory which `.mod` path is relative to. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`.
#' @export
#' @rdname submit_model
submit_model.character <- function(
  .mod,
  .mode = c("sge", "local"),
  .bbi_args = NULL,
  ...,
  .config_path=NULL,
  .wait = TRUE,
  .dry_run=FALSE,
  .directory = getOption("rbabylon.model_directory")
) {

  # check for .directory and combine with .mod
  .mod <- combine_directory_path(.directory, .mod)

  ## parse type of file passed
  # if no extension, assume YAML
  if (tools::file_path_sans_ext(.mod) == .mod) {
    if (fs::file_exists(yaml_ext(.mod))) {
      .mod <- read_model(yaml_ext(.mod))
    } else {
      stop(glue("Cannot find file {yaml_ext(.mod)}. If passing a non-YAML file to submit_model you must include the file extension."))
    }
  } else if (is_valid_yaml_extension(.mod)) {
    .mod <- read_model(.mod)
  } else if (is_valid_nonmem_extension(.mod)) {
    # if NONMEM file, build model object
    .mod_list <- list()
    .mod_list[[YAML_MOD_PATH]] <- basename(.mod)
    .mod_list[[WORKING_DIR]] <- normalizePath(dirname(.mod))
    .mod_list[[YAML_MOD_TYPE]] <- "nonmem"
    .mod_list[[YAML_DESCRIPTION]] <- as.character(glue("{.mod} passed directly to submit_model()"))
    .mod <- create_model_object(.mod_list)
  } else {
    stop(glue("Unsupported file type passed to submit_model(): `{.mod}`. Valid options are `.yaml`, `.mod`, and `.ctl`"))
  }

  # submit model
  .model_type <- .mod[[YAML_MOD_TYPE]]
  if (.model_type == "nonmem") {
    res <- submit_nonmem_model(.mod,
                               .mode = .mode,
                               .bbi_args = .bbi_args,
                               ...,
                               .config_path = .config_path,
                               .wait = .wait,
                               .dry_run = .dry_run)
  } else if (.model_type == "stan") {
    stop(NO_STAN_ERR_MSG)
  } else {
    stop(glue("Passed `{.model_type}`. Valid options: `{SUPPORTED_MOD_TYPES}`"))
  }
  return(res)
}


#' S3 dispatch for submit_model from numeric input
#' This will only work if you are calling from the same directory as the models, or if you have set the model directory with `set_model_directory()`
#' @param .mod Integer that corresponds to the name of a YAML and model file
#' @param .directory Model directory containing the files referenced by `.mod`. Defaults to `options('rbabylon.model_directory')`, which can be set globally with `set_model_directory()`.
#' @export
#' @rdname submit_model
submit_model.numeric <- function(
  .mod,
  .mode = c("sge", "local"),
  .bbi_args = NULL,
  ...,
  .config_path=NULL,
  .wait = TRUE,
  .dry_run=FALSE,
  .directory = getOption("rbabylon.model_directory")
) {
  # convert to character
  .mod <- as.character(.mod)

  # call character dispatch
  res <- submit_model(
    .mod = .mod,
    .mode = .mode,
    .bbi_args = .bbi_args,
    ...,
    .config_path = .config_path,
    .wait = .wait,
    .dry_run = .dry_run,
    .directory = .directory
  )

  return(res)
}


#' Submit a NONMEM model via babylon
#' @param .mod An S3 object of class `bbi_nonmem_model`, for example from `new_model()`, `read_model()` or `copy_model_from()`
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
