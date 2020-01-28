#' Execute `bbi summary` to generate json output of model results
#' @param .path Full path to a model that has been run that user wants to summarize
#' @param .model_type Type of model to summarize. Currently only supports "nonmem"
#' @export
model_summary <- function(.path, .model_type = c("nonmem")) {
  .model_type <- match.arg(.model_type)

  # execute summary command
  cmd_args <- c(.model_type, "summary", "--json", .path)
  output <- bbi_exec(cmd_args)

  # parse json output
  return(parse_model_results(output$stdout))
}

#' Reads model results to a list by parsing json output from bbi summary
#' @param .x summary output string
#' @param ... params to pass to jsonlite::fromJSON
#' @param file model json summary output file path
#' @export
#' @importFrom jsonlite fromJSON read_json
parse_model_results <- function(.x, ..., file = NULL) {
  if (!is.null(file)) {
    return(jsonlite::read_json(file = file, simplifyDataFrame = FALSE))
  }
  return(jsonlite::fromJSON(.x, ..., simplifyDataFrame = FALSE))
}
