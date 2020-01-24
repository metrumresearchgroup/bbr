#' Submit a NONMEM model via babylon
#' @param .path Full path to a model file(s) that should be run
#' @param .type Either "local" for local execution or "sge" to submit model(s) to the grid
#' @param .args A named list specifying arguments to pass to babylon. All available arguments are `options("rbabylon.nonmen_args")`
#' @param ... args passed through to `bbi_exec()`
#' @param .config_path Optionally specify a path to a babylon.yml config. If not specified, the config in the model directory will be used by default. (?)
#' @return output from the model run (?)
#' @export
submit_nonmem_model <- function(.path,
                                .type = c("local", "sge"),
                                .args,
                                ...,
                                .config_path=NULL) {
  .type <- match.arg(.type)

  # build command line args
  cmd_args <- c("nonmem", "run", .type)

  if (!is.null(.args)) {
    args_vec <- check_nonmem_args(.args)
    cmd_args <- c(cmd_args, args_vec)
  }

  # add config path
  if (!is.null(.config_path)) {
    cmd_args <- c(cmd_args, sprintf("--config=%s", .config_path))
  }

  # execute
  return(bbi_exec(.path, cmd_args, ...)$stdout)
}

#
#submit_nonmem_model("/data/MetworxTraining2020/model/nonmem/pk/2.ctl", .args = list("threads" = 4))
