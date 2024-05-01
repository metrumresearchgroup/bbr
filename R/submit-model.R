###############################################
# S3 dispatches for submitting a single model
###############################################

#' Submit a model to be run
#'
#' Submits a model to be run by calling out to `bbi`.
#'
#' @return While `submit_model()` _does_ return something, it is primarily
#' called for its side effects, specifically that it runs the model and writes
#' all model outputs to disk under `get_output_dir(.mod)`.
#'
#' **NONMEM**
#' For NONMEM models, a `bbi_process` object is returned. The typical NONMEM
#' output files are all written into `get_output_dir(.mod)`. A summary of the
#' contents of these files can be accessed with [model_summary()], and the path
#' to individual files can be easily constructed like
#' `build_path_from_model(.mod, ".lst")`.
#'
#' A `bbi_config.json` file is also written, which stores information about the
#' configuration of the run. See [config_log()] for more details about this
#' file.
#'
#' @template nonmem-mod-ext
#'
#' @seealso [submit_models()]
#' @param .mod The model object to submit.
#' @param .bbi_args A named list specifying arguments to pass to bbi
#'   formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" =
#'   4)`. Run [print_bbi_args()] to see valid arguments. Note that bbr does
#'   not support changing the output directory (including through the model or
#'   global YAML files).
#' @param .mode Either `"sge"`, the default, to submit model(s) to the grid or
#'   `"local"` for local execution. This can be passed directly to this argument
#'   or set globally with `options("bbr.bbi_exe_mode")`.
#' @param ... args passed through to `bbi_exec()`
#' @param .overwrite Logical to specify whether or not to overwrite existing
#'   model output from a previous run. If `NULL`, the default, will defer to
#'   setting in `.bbi_args` or `bbi.yaml`. If _not_ `NULL` will override any
#'   settings in `.bbi_args` or `bbi.yaml`.
#' @param .config_path Path to a bbi configuration file. If `NULL`, the
#'   default, will attempt to use a `bbi.yaml` in the same directory as the
#'   model.
#' @param .wait If `TRUE`, the default, wait for the bbi process to return
#'   before this function call returns. If `FALSE` function will return while
#'   bbi process runs in the background.
#' @param .dry_run Returns an object detailing the command that would be run,
#'   insted of running it. This is primarily for testing but also a debugging
#'   tool.
#' @export
submit_model <- function(
    .mod,
    .bbi_args = NULL,
    .mode = getOption("bbr.bbi_exe_mode"),
    ...,
    .overwrite = NULL,
    .config_path = NULL,
    .wait = TRUE,
    .dry_run = FALSE
) {
  UseMethod("submit_model")
}

#' @describeIn submit_model Takes a `bbi_nonmem_model` object.
#' @export
submit_model.bbi_nonmem_model <- function(
    .mod,
    .bbi_args = NULL,
    .mode = getOption("bbr.bbi_exe_mode"),
    ...,
    .overwrite = NULL,
    .config_path = NULL,
    .wait = TRUE,
    .dry_run = FALSE
) {

  res <- submit_nonmem_model(.mod,
                             .bbi_args = .bbi_args,
                             .mode = .mode,
                             ...,
                             .overwrite = .overwrite,
                             .config_path = .config_path,
                             .wait = .wait,
                             .dry_run = .dry_run)
  return(res)
}

#' @describeIn submit_model Takes a `bbi_nmboot_model` object.
#' @export
submit_model.bbi_nmboot_model <- function(
    .mod,
    .bbi_args = NULL,
    .mode = getOption("bbr.bbi_exe_mode"),
    ...,
    .overwrite = NULL,
    .config_path = NULL,
    .wait = FALSE,
    .dry_run = FALSE,
    .batch_size = 100
){
  # Ensure bootstrap setup was done
  spec_path <- get_boot_spec_path(.mod, .check_exists = FALSE)
  if(!fs::file_exists(spec_path)){
    rlang::abort(
      c(
        glue("No bootstrap specification file was found at {spec_path}"),
        "i" = "Please run `setup_bootstrap_run()` with your bootstrap run model object."
      )
    )
  }

  # If bbi.yaml exists in the current modeling directory, and _not_ the nested
  # bootstrap directory, copy it to there before running
  model_dir <- get_model_working_directory(.mod)
  boot_dir <- .mod[[ABS_MOD_PATH]]
  bbi_yaml_path <- file.path(model_dir, "bbi.yaml")
  if(fs::file_exists(bbi_yaml_path) && !fs::file_exists(file.path(boot_dir, "bbi.yaml"))){
    fs::file_copy(bbi_yaml_path, file.path(boot_dir, "bbi.yaml"))
    verbose_msg(glue("Inheriting `bbi.yaml` from `{model_dir}`"))
  }

  boot_models <- get_boot_models(.mod)
  res <- if (!isTRUE(.dry_run) && .batch_size < length(boot_models)) {
    submit_batch_callr(
      .mods = boot_models,
      .batch_size = .batch_size,
      .bbi_args = .bbi_args,
      .mode = .mode,
      .overwrite = .overwrite,
      .config_path = .config_path,
      stdout_path = file.path(boot_dir, "OUTPUT")
    )

  } else {
    submit_models(
      boot_models, .bbi_args, .mode, ...,
      .overwrite = .overwrite, .config_path = .config_path,
      .wait = .wait, .dry_run = .dry_run
    )
  }

  return(res)
}

#####################################
# Private implementation function(s)
#####################################

#' Submit a NONMEM model via bbi
#'
#' Private implementation function called by `submit_model()` dispatches.
#' @param .mod An S3 object of class `bbi_nonmem_model`, for example from `new_model()`, `read_model()` or `copy_model_from()`
#' @importFrom stringr str_detect
#' @importFrom tools file_path_sans_ext
#' @importFrom checkmate assert_logical
#' @return An S3 object of class `bbi_process`
#' @keywords internal
submit_nonmem_model <- function(.mod,
                                .bbi_args = NULL,
                                .mode = getOption("bbr.bbi_exe_mode"),
                                ...,
                                .overwrite = NULL,
                                .config_path = NULL,
                                .wait = TRUE,
                                .dry_run=FALSE) {

  # check against YAML
  check_yaml_in_sync(.mod)

  # check for valid .mode arg
  check_mode_argument(.mode)

  # build command line args
  .bbi_args <- parse_args_list(.bbi_args, .mod[[YAML_BBI_ARGS]])
  if (!is.null(.overwrite)) {
    checkmate::assert_logical(.overwrite)
    .bbi_args[["overwrite"]] <- .overwrite
  }
  args_vec <- check_bbi_args(.bbi_args)


  cmd_args <- c("nonmem", "run", .mode, get_model_path(.mod), args_vec)

  # define working directory
  model_dir <- get_model_working_directory(.mod)

  .path_exists <- file_exists(.config_path %||% file.path(model_dir, "bbi.yaml"))
  if(!.path_exists){
    stop(paste("No bbi configuration was found in the execution directory.",
               "Please run `bbi_init()` with the appropriate directory to continue."))
  }

  if (!is.null(.config_path)) {
    cmd_args <- c(
      cmd_args,
      sprintf("--config=%s", normalizePath(.config_path))
    )
  }

  if (.dry_run) {
    # construct fake res object
    return(bbi_dry_run(cmd_args, model_dir))
  }

  # launch model
  res <- bbi_exec(cmd_args, .wait = .wait, .dir = model_dir, ...)

  return(res)
}


#' Private helper to check if `.mode` arg to `submit_model()` is valid
#' @param .mode argument to be checked
#' @keywords internal
check_mode_argument <- function(.mode) {
  if (is.null(.mode)) {
    stop(BBI_EXE_MODE_NULL_ERR_MSG, call. = FALSE)
  }

  checkmate::assert_string(.mode)

  if (!(.mode %in% BBI_VALID_MODES)) {
    stop(BBI_EXE_MODE_INVALID_ERR_MSG, call. = FALSE)
  }

  return(invisible(TRUE))
}
