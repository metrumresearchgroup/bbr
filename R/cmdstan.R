########################
# HOLDING PLACE for
# functions having to do
# with stan models
########################

#' Checks a `bbi_stan_model` for necessary files
#'
#' Checks a `bbi_stan_model` object to make sure it can find
#' all of the files necesssary to submit the model.
#'
#' @details
#' Will look for the following:
#' * `build_path_from_model(.mod, "-standata.R")`
#' * `build_path_from_model(.mod, "-init.R")`
#' * `build_path_from_model(.mod, ".stan")`
check_stan_model <- function(.mod) {
  files_to_check <- build_path_from_model(.mod, STAN_MODEL_REQ_FILES)
  files_missing <- !fs::file_exists(files_to_check)
  if (any(files_missing)) {
    warning(paste(
      glue("The following files, which are necessary to run a `bbi_stan_model` are missing from {get_model_id(.mod)}:"),
      paste(paste0(" * ", names(which(files_missing))), collapse = "\n"),
      sep = "\n"
    ))
    return(FALSE)
  }
  return(TRUE)
}

#' Attaches a .stan file to a model
#'
#' When a `bbi_stan_model` is created, an empty .stan
add_stan_file <- function(.mod) {
  checkmate::assert_class(.mod, STAN_MOD_CLASS)


}
