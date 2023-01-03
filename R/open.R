
#' Open main model definition file in text editor
#'
#' This is a light wrapper around [get_model_path()] to enable quickly jumping
#' to a model definition file in RStudio or any other editor supported by
#' [utils::file.edit()].
#'
#' @param .mod Model object or a file path to a model.
#' @export
open_model_file <- function(.mod) {
  checkmate::assert_class(.mod, "bbi_model")
  file_edit(get_model_path(.mod))
}

# Under RStudio, there is a distinction between a plain file.edit() and
# utils::file.edit(). RStudio overrides file.edit() so that it opens up a tab.
# utils::file.edit(), on the other hand, pops up a window that the user must
# save before returning to RStudio.
file_edit <- function(...) {
  if (!interactive()) {
    return(invisible(NULL))
  }

  fn <- if (exists("file.edit", envir = globalenv())) {
    get("file.edit", envir = globalenv())
  } else {
    utils::file.edit
  }
  fn(...)
}
