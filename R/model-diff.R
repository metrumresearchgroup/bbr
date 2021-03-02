#' Compare model files between models
#'
#' By default, this compares a model's model file on disk to the model file on
#' its "parent" model (the model referenced in its `based_on` field).  It can
#' optionally take a second model (passed to `.mod2`), in which case it
#' will compare the first model to the second model, ignoring the `based_on`
#' field entirely.
#'
#' @details
#' **`based_on` details:** As described above, if `.mod2` is `NULL` (the default),
#' then the `.mod` will be compared to the model returned by `get_based_on(.mod)`.
#' However, _if there is more than one model_ in `based_on` for `.mod`, then user
#' will be prompted to explicitly pass the model they want to compare against to
#' the `.mod2` argument.
#'
#' **Printing vs. knitting:** FILL THIS IN...
#'
#' @param .mod The `bbi_{.model_type}_model` to compare.
#' @param .mod2 If a `bbi_{.model_type}_model` object is passed, compare `.mod` to
#'   `.mod2`. If `.mod2 = NULL`, the default, compare the `.mod` model to
#'   any models in its `based_on` field.
#' @param .file FILL THIS IN...
#' @param ... arguments passed through to methods. (Currently none.)
#' @export
model_diff <- function(.mod, .mod2 = "model", .file = NULL, ...) {
  UseMethod("model_diff")
}

#' @export
model_diff.bbi_nonmem_model <- function(
  .mod,
  .mod2 = NULL,
  .file = c("model"),
  ...
) {
  .file <- match.arg(.file)

  .mod2 <- model_diff_get_comp(.mod, .mod2)

  .file1 <- get_model_path(.mod)
  .file2 <- get_model_path(.mod2)

  model_diff_impl(.file1, .file2)
}


###################################
# PRIVATE IMPLEMENTATION FUNCTIONS
###################################

#' Private helper to get a valid comparison model
#' @inheritParams model_diff
#' @keywords internal
model_diff_get_comp <- function(.mod, .mod2) {
  check_yaml_in_sync(.mod)

  if (is.null(.mod2)) {
    based_on_path <- get_based_on(.mod)
    if (length(based_on_path) == 0) {
      stop(paste(glue("Model {get_model_id(.mod)} has no models in `based_on`.", MODEL_DIFF_ERR_MSG)), call. = F)
    } else if (length(based_on_path) > 1) {
      stop(paste(glue("Model {get_model_id(.mod)} has multiple models in `based_on`.", MODEL_DIFF_ERR_MSG)), call. = F)
    } else {
      .mod2 <- read_model(based_on_path)
    }
  }
  check_yaml_in_sync(.mod2)
  return(.mod2)
}

#' Private helper to diff two model files
#' @importFrom diffobj diffFile
#' @param model_A Path to first model file
#' @param model_B Path to second model file
#' @keywords internal
model_diff_impl <- function(model_A, model_B) {

  if (!all(c(
    checkmate::check_string(model_A),
    checkmate::check_string(model_B),
    fs::file_exists(c(model_A, model_B))
  ))) {
    dev_error(paste(
      "model_diff_impl() got the following:",
      paste(model_A, collapse =" -- "),
      paste(model_B, collapse =" -- ")
    ))
  }
  suppressSpecificWarning({
    diffobj::diffFile(
      model_A,
      model_B,
      mode = "sidebyside"
    )
  }, .regexpr = "incomplete final line")
}
