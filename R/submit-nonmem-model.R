#' Submit a NONMEM model via babylon
#' @param .path Full path to one of the following:
#'     1) path to a model file(s) that should be run OR
#'     2) path to a model YAML file that must contain:
#'         a) a path to a model file with the key `model_path`,
#'         b) any NONMEM args that would like to pass through, with keys that match the arguments in `print_nonmem_args()`,
#'         c) any extra user data that will be passed through to `bbi_config.json` with whatever keys are specified in the yaml.
#'     (Path must be either an absolute path or relative to the R working directory)
#' @param .type Either "local" for local execution or "sge" to submit model(s) to the grid
#' @param .args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)`. Run `print_nonmem_args()` to see valid arguments.
#' @param ... args passed through to `bbi_exec()`
#' @param .config_path Optionally specify a path to a babylon.yml config. If not specified, the config in the model directory will be used by default. Path MUST be either an absolute path or relative to the model directory.
#' @param .output_dir Name of directory that output will be written to. If NULL is passed (default) then it assumed to be the name of the model file (with the extension stripped).
#' @param .dry_run Returns character string of command that would be run, insted of running it. This is primarily for testing but also a debugging tool.
#' @importFrom stringr str_detect
#' @importFrom tools file_path_sans_ext
#' @return output from the model run (?)
#' @export
submit_nonmem_model <- function(.path,
                                .type = c("sge", "local"),
                                .args = NULL,
                                ...,
                                .config_path=NULL,
                                .output_dir=NULL,
                                .dry_run=FALSE) {

  # parse yaml, if passed
  if ((str_detect(.path, "\\.yaml$")) || (str_detect(.path, "\\.yml$"))) {
    yaml_list <- parse_mod_yaml(.path)
    # reset model path
    .path <- yaml_list$model_path

    # update .args from yaml
    .args <- parse_args_list(.args, yaml_list$args_list)

    # pass through user data
    parse_user_data(yaml_list$user_data)
  }

  # check for valid type arg
  .type <- match.arg(.type)

  # parse model directory and model filename
  model_dir <- dirname(.path)
  model <- basename(.path)
  if (model_dir == ".") {
    # given a path that is just the model name, set directory to getwd()
    model_dir <- getwd()
  }

  # build command line args
  args_vec <- check_nonmem_args(.args)
  cmd_args <- c("nonmem", "run", .type, model, args_vec)

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
    res <- bbi_exec(cmd_args, wd = model_dir, ...)

    # add to result object
    res$output_dir <- paste(model_dir, .output_dir, sep="/")

    # return object
    attr(res, "class") <- "babylon_result"
    return(res)
  }
}

