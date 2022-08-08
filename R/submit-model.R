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
#' **Stan**
#' For Stan models, a `cmdstanr` fit object of class `"CmdStanMCMC"` is
#' returned. See the `?cmdstanr::CmdStanMCMC` docs for methods and information
#' on this object. This can be reloaded from disk later with [read_fit_model()].
#' Additionally, the following files are written to disk:
#'
#' * In the model directory:
#'   * `<run>-standata.json` -- This is the exact data that is passed to
#'   `cmdstanr`, written to disk automatically with
#'   `cmdstanr::write_stan_json()`.
#'   * `<run>-stanargs.R` -- This captures the arguments that were passed
#'   through to `cmdstanr::sample()`, for later reproducibility of this run.
#'   * `<run>` -- The binary compiled Stan model. This is added to `.gitignore`
#'   automatically.
#' * In the output directory (`<run>-output/`):
#'   * `fit.RDS` -- The `cmdstanr` fit object. Can be reloaded with
#'   [read_fit_model()] as described above.
#'   * `bbi_config.json` -- stores information about the configuration of the run.
#'   See [config_log()] for more details about this file.
#'   * `<run>-<timestamp>-<chain>-<random_id>.csv` -- CSV files of the posteriors,
#'   written out by `cmdstanr`. These are automatically added to `.gitignore`.
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
#' @param ... args passed through. For `bbi_stan_model` this is how you pass
#'   arguments through to the `$sample()` method of your `cmdstanr` model
#'   object. See [cmdstanr::sample()] for valid arguments and details. For
#'   `bbi_nonmem_model` these are passed to `bbi_exec()`.
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
  .dry_run=FALSE
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
  .dry_run=FALSE
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

#' @describeIn submit_model Takes a `bbi_stan_model` object. All arguments
#'   passed through `...` will be passed to [cmdstanr::sample()] method.
#' @export
submit_model.bbi_stan_model <- function(
  .mod,
  .bbi_args = NULL,
  .mode = c("local"),
  ...,
  .overwrite = NULL,
  .config_path = NULL,
  .wait = NULL,
  .dry_run=NULL
) {

  if(!is.null(.bbi_args))    {warning(".bbi_args is not a valid argument for submit_model.bbi_stan_model. Ignoring...")}
  if(!is.null(.config_path)) {warning(".config_path is not a valid argument for submit_model.bbi_stan_model. Ignoring...")}
  if(!is.null(.wait))        {warning(".wait is not a valid argument for submit_model.bbi_stan_model. Ignoring...")}
  if(!is.null(.dry_run))     {warning(".dry_run is not a valid argument for submit_model.bbi_stan_model. Ignoring...")}

  res <- submit_stan_model_cmdstanr(
    .mod,
    .mode = .mode,
    ...,
    .overwrite = .overwrite
  )
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

#' Submit a Stan model via cmdstanr
#'
#' Private implementation function called by `submit_model()` dispatches.
#' @param .mod An S3 object of class `bbi_stan_model`
#' @return The object returned from [cmdstanr::sample()]
#' @keywords internal
submit_stan_model_cmdstanr <- function(.mod,
                                       .mode = c("local"), # TODO: add sge mode for cmdstanr
                                       ...,
                                       .overwrite = NULL) {

  # check against YAML
  check_yaml_in_sync(.mod)
  check_stan_model(.mod, .error = TRUE)

  # check for valid type arg
  .mode <- match.arg(.mode)

  out_dir <- get_output_dir(.mod, .check_exists = FALSE)
  standata_json_path <- build_path_from_model(.mod, STANDATA_JSON_SUFFIX)
  stanargs_path <- build_path_from_model(.mod, STANARGS_SUFFIX)

  if (fs::dir_exists(out_dir)) {
    if (isTRUE(.overwrite)) {
      fs::dir_delete(out_dir)
      fs::dir_create(out_dir)
      if(fs::file_exists(standata_json_path)) { fs::file_delete(standata_json_path) }
      if(fs::file_exists(stanargs_path)) { fs::file_delete(stanargs_path) }
    } else {
      stop(glue("{out_dir} already exists. Pass submit_model(..., .overwrite = TRUE) to delete it and re-run the model."), call. = FALSE)
    }
  } else {
    fs::dir_create(out_dir)
  }

  stanmod <- compile_stanmod(.mod)

  # capture args, check against sample(), then write to stanargs.R
  valid_stanargs <- methods::formalArgs(stanmod$sample)
  stanargs <- parse_stanargs(.mod, valid_stanargs, ...)

  # construct input data set and initial estimates
  standata_list <- build_data(.mod, .out_path = standata_json_path)
  stanargs[["output_dir"]] <- get_output_dir(.mod)
  stanargs[["data"]] <- standata_json_path
  stanargs[["init"]] <- import_stan_init(.mod, .standata = standata_list)
  rm(standata_list) # once we've passed this to import_stan_init() we don't need it in memory

  # launch model
  res <- do.call(
    stanmod$sample,
    args = stanargs
  )

  # if successful, save model and write bbi_config.json to disk
  save_fit_stanmod(res, build_path_from_model(.mod, STAN_MODEL_FIT_RDS))
  build_stan_bbi_config(.mod)

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
