#' read the model results
#' @param .x file_name
#' @param ... params to pass to jsonlite::read_json
#' @export
read_model_results <- function(.x, ...) {
  jsonlite::read_json(.x, ...)
}
