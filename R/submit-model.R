# S3 dispatch for submitting models
submit_model <- function(.spec, ...) {
  UseMethod("submit_model", .spec)
}

submit_model.bbi_nonmem_spec <- function(.spec, ...) {
  res <- submit_nonmem_model(.spec$yaml_path, ...)
  return(res)
}

submit_model.character <- function(.path, .model_type = c("nonmem"), ...) {
  .model_type <- match.arg(.model_type)
  if (.model_type == "nonmem") {
    res <- submit_nonmem_model(.path, ...)
  } else {
    stop(glue("Passed `{.model_type}` to model_summary(.model_type). Valid options include: `'nonmem'`"))
  }
  return(res)
}


#' Submit a NONMEM model via babylon
#' @param .path Full path to one of the following:
#'     1) path to a model file(s) that should be run OR
#'     2) path to a model YAML file that must contain:
#'         a) a path to a model file with the key `model_path`,
#'         b) any NONMEM args that would like to pass through, with keys that match the arguments in `print_nonmem_args()`,
#'         c) any extra user data that will be passed through to `bbi_config.json` with whatever keys are specified in the yaml.
#'     (Path must be either an absolute path or relative to the R working directory)
#' @param .mode Either "local" for local execution or "sge" to submit model(s) to the grid
#' @param .args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`. Run `print_nonmem_args()` to see valid arguments.
#' @param ... args passed through to `bbi_exec()`
#' @param .config_path Optionally specify a path to a babylon.yml config. If not specified, the config in the model directory will be used by default. Path MUST be either an absolute path or relative to the model directory.
#' @param .output_dir Name of directory that output will be written to. If NULL is passed (default) then it assumed to be the name of the model file (with the extension stripped).
#' @param .wait Boolean for whether to wait for the bbi process to return before this function call returns.
#' @param .dry_run Returns character string of command that would be run, insted of running it. This is primarily for testing but also a debugging tool.
#' @importFrom stringr str_detect
#' @importFrom tools file_path_sans_ext
#' @importFrom purrr list_modify
#' @return output from the model run (?)
#' @export
submit_nonmem_model <- function(.path,
                                .mode = c("sge", "local"),
                                .args = NULL,
                                ...,
                                .config_path=NULL,
                                .output_dir=NULL,
                                .wait = TRUE,
                                .dry_run=FALSE) {

  # parse yaml, if passed
  if ((str_detect(.path, "\\.yaml$")) || (str_detect(.path, "\\.yml$"))) {
    yaml_list <- parse_mod_yaml(.path)
    # reset model path
    .yaml_path <- .path
    .path <- yaml_list[[YAML_MOD_PATH]]

    # update .args from yaml
    .args <- parse_args_list(.args, yaml_list[[YAML_BBI_ARGS]])

  } else {
    .yaml_path <- NULL
  }

  # check for valid type arg
  .mode <- match.arg(.mode)

  # parse model directory and model filename
  model_dir <- dirname(.path)
  model <- basename(.path)
  if (model_dir == ".") {
    # given a path that is just the model name, set directory to getwd()
    model_dir <- getwd()
  }

  # build command line args
  args_vec <- check_nonmem_args(.args)
  cmd_args <- c("nonmem", "run", .mode, model, args_vec)

  # add config path
  if (!is.null(.config_path)) {
    cmd_args <- c(cmd_args, sprintf("--config=%s", .config_path))
  }

  # parse output directory from model name, if not passed as argument
  if (is.null(.output_dir)) {
    .output_dir <- tools::file_path_sans_ext(model)
  }

  # execute
  if (.dry_run) {
    return(list(
      cmd_args = cmd_args,
      wd = model_dir,
      call = paste(
        "cd", model_dir, ";",
        getOption("rbabylon.bbi_exe_path"),
        paste(cmd_args, collapse = " ")
      )
    ))
  } else {
    # launch model
    res <- bbi_exec(cmd_args, .wait = .wait, wd = model_dir, ...)

    # add to result object
    res <- list_modify(
      res,
      output_dir = file.path(model_dir, .output_dir),
      model_path = .path,
      yaml_path = .yaml_path
    )
    class(res) <- c("bbi_nonmem_result", class(res))

    return(res)
  }
}
