exec_summary <- function(.path) {
  model_dir <- dirname(.path)
  model <- basename(.path)
  output <- withr::with_dir(model_dir, {
    processx::run("bbi", c("nonmem", "summary", model, "--json"))
  })

  parse_model_results(output$stdout)
}

bbi_exec <- function(.path, cmd_args, ...) {
  model_dir <- dirname(.path)
  model <- basename(.path)
  if (model_dir == ".") {
  # given a path that is just the model name
    model_dir <- getwd()
  }
  output <- processx::run(getOption("rbabylon.bbi_exe_path"), c(cmd_args, model), wd = model_dir, ...)
  return(output)
}
model_summary <- function(.path) {

}
#' read the model results
#' @param .x model string
#' @param ... params to pass to jsonlite::read_json
#' @export
parse_model_results <- function(.x, ..., file = NULL) {
  if (!is.null(file)) {
    return(jsonlite::read_json(file = file, simplifyDataFrame = FALSE))
  }
  jsonlite::fromJSON(.x, ..., simplifyDataFrame = FALSE)
}
