#' Submit a NONMEM model via babylon
#' @param .path Full path to a model file(s) that should be run. Path MUST be either an absolute path or relative to the R working directory.
#' @param .type Either "local" for local execution or "sge" to submit model(s) to the grid
#' @param .args A named list specifying arguments to pass to babylon formatted like `list("nm_version" = "nm74gf_nmfe", "json" = T, "threads" = 4)` All available arguments are in `NONMEM_ARGS`.
#' @param ... args passed through to `bbi_exec()`
#' @param .config_path Optionally specify a path to a babylon.yml config. If not specified, the config in the model directory will be used by default. Path MUST be either an absolute path or relative to the model directory.
#' @return output from the model run (?)
#' @export
submit_nonmem_model <- function(.path,
                                .type = c("sge", "local"),
                                .args = NULL,
                                ...,
                                .config_path=NULL) {

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

  # execute
  return(bbi_exec(cmd_args, wd = model_dir, ...))
}

