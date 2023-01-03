
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
